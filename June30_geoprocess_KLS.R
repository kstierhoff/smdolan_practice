library(sf)
library(ggvoronoi)
library(ggplot2)
library(sp)
library(dplyr)
library(concaveman)
library(mapview)

load(here::here("data/nasc_final.Rdata"))
load(here::here("data/catch_info.Rdata"))

# Convert NASC to spatial
nasc.sf <- nasc %>% 
  # arrange(transect, long) %>% 
  sf::st_as_sf(coords=c("long", "lat"), crs = 4326)

# Create transect.sf for mapping later
transects.sf <- nasc.sf %>% 
  group_by(transect) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") 

# Bound box based off of min and max values from nasc transect data 
# TIP: You don't need as.numeric() if you don't quote the values
box <- data.frame(x=c(-129.25383, -117.27371, -117.27371,-129.2538371),
                  y=c(50.75796, 50.757960, 32.13575,32.13575),
                  group=1)

# remove clusters with zero catch 
# cluster <- as.data.frame(cluster.summ.wt[which(!cluster.summ.wt$cluster %in% cluster.zero$cluster),])
cluster <- filter(cluster.summ.wt, AllCPS > 0) %>% 
  as.data.frame()

# Convert to sf for mapview later
cluster.sf <- st_as_sf(cluster, coords = c("long","lat"), crs = 4326)

##plot trawl clusters, transects, and voronoi tessellation 
# Nice plot!
ggplot()+
  theme_bw()+ 
  coord_map()+
  stat_voronoi(data=cluster,
               aes(long,lat),geom="path", color="dark green", outline=box,size=0.5)+
  geom_path(data = nasc, aes(long, lat),color="gray")+
  geom_point(data=cluster, 
             aes(long, lat))+
  geom_text(data=cluster, 
            aes(long, lat, label=cluster), size=3, hjust=-1, vjust=0)

## convert to spdf  
cluster.spdf <- voronoi_polygon(cluster, x="long", y="lat", outline=box)
cluster.df <- fortify_voronoi(cluster.spdf)

# plot spdf in ggplot 
ggplot()+coord_map()+theme_bw()+
  geom_path(data = nasc, aes(long, lat))+
  geom_polygon(data=cluster.spdf,
               aes(x=long, y=lat, fill=group, group=group),color="black", alpha=0.8)+
  geom_point(data=cluster, 
             aes(long, lat))+
  geom_text(data=cluster, 
            aes(long, lat, label=cluster), size=3, hjust=-1, vjust=0) +
  theme(legend.position = "none") # Removed the legend for clarity

# Lacks CRS; mapview works but no background map
mapview::mapview(cluster.spdf)

# Create more constrained outline using a concave hull polygon with {concaveman}
box2 <- concaveman(nasc.sf, concavity = 10)
# Define CRS
st_crs(box2) = 4326

# Convert to spatial to test the as_Spatial function
box.sp <- as_Spatial(box2)
# Map now has a background
mapview::mapview(box.sp)

# Convert box to a data frame to use with voronoi_polygon
box.kls <- box2 %>% 
  # Convert polygon to points
  st_cast("POINT") %>% 
  # Extract lat/long coordinates
  mutate(long = as.data.frame(st_coordinates(.))$X,
         lat = as.data.frame(st_coordinates(.))$Y,
         group = 1) %>%
  st_set_geometry(NULL)

# Create Voronoi polygons in sf format
cluster.voronoi <- voronoi_polygon(cluster, x="long", y="lat", outline=box.kls) %>% 
  as("sf") 
st_crs(cluster.voronoi) = 4326

# Create hulls around positive clusters using KLS's original method
cluster.hull <- plyr::ddply(nasc, "cluster", atm::find_hull) %>% 
  select(long, lat, cluster) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  group_by(cluster) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON")

# Plot Voronoi polygons along with convex hull polygons
ggplot() + 
  geom_sf(data = cluster.voronoi, aes(fill = factor(cluster))) + 
  geom_sf(data = cluster.hull, fill = "gray50", alpha = 0.5) + 
  geom_text(data = cluster, 
            aes(long, lat, label = cluster), size = 3) + 
  theme_bw() + 
  theme(legend.position = "none")

# Do the same using mapview; this makes it a little easier to see the differences with zooming
mapview(cluster.voronoi, zcol = "cluster") + 
  mapview(cluster.hull) + 
  mapview(cluster.sf, size = 1) + 
  mapview(transects.sf, zcol = NULL)

# Join NASC with cluster.voronoi and cluster.hull
nasc.comp <- nasc %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  # Select only the necessary columns
  select(transect, Interval, cluster_orig = cluster) %>% 
  # Join cluster number from Voronoi tessellation
  st_join(select(cluster.voronoi, cluster_voronoi = cluster)) %>% 
  # Join cluster number from convex hull (original method)
  st_join(select(cluster.hull, cluster_hull = cluster)) %>%
  # Subtract original cluster number from new; values != 0 are different
  mutate(diff_voronoi = cluster_orig - cluster_voronoi,
         diff_hull    = cluster_orig - cluster_hull)

# Compare differences in cluster assignments
# Differences reflect minor differences near margins
# Need to try with a different projection (crs = 3310, CA Albers equal area, perhaps) to see if differences persist
ggplot() +
  geom_sf(data = cluster.voronoi, aes(fill = factor(cluster)), show.legend = FALSE) +
  geom_sf(data = cluster.hull, colour = "white", fill = NA, alpha = 0.5) +
  geom_sf(data = transects.sf, alpha = 0.5) +
  geom_sf(data = filter(nasc.comp, diff_voronoi != 0), 
          aes(colour = diff_voronoi)) + 
  theme_bw() 

# Save plot as image; open from the Files pane, and try zooming on your viewer to see details better than the Rstudio viewer
ggsave(filename = "figures/cluster_comparison.png",
       height = 10)

# Try comparison with an equal area projection
# Reproject (transform) cluster.sf to equal area
cluster.ea <- st_transform(cluster.sf, crs = 3310)
nasc.ea <- st_transform(nasc.sf, crs = 3310)
cluster.hull.ea <- st_transform(cluster.hull, crs = 3310)

# Convert box to a data frame to use with voronoi_polygon
box.kls.ea <- box2 %>% 
  # Convert polygon to points
  st_cast("POINT") %>% 
  st_transform(crs = 3310) %>% 
  # Extract lat/long coordinates
  mutate(X = as.data.frame(st_coordinates(.))$X,
         Y = as.data.frame(st_coordinates(.))$Y,
         group = 1) %>%
  st_set_geometry(NULL)

# Create Voronoi polygons in sf format
cluster.voronoi.ea <- voronoi_polygon(cluster, x="X", y="Y", outline=box.kls.ea) %>% 
  as("sf") 
st_crs(cluster.voronoi.ea) = 3310

ggplot() + 
  geom_sf(data = cluster.voronoi, aes(fill = factor(cluster)), show.legend = FALSE) + 
  geom_sf(data = cluster.voronoi.ea, fill = NA, colour = "white") + 
  geom_sf(data = cluster.hull, fill = NA, linetype = "dashed")

# Do the same using mapview; this time with the projected data
# A close comparison between the voronoi polygons from equal area data and the convex hull polygons is much more similar, YAY!
mapview(cluster.voronoi.ea, zcol = "cluster") + 
  mapview(cluster.hull, color = "white") + 
  mapview(cluster.sf, size = 1) + 
  mapview(transects.sf, zcol = NULL)

# Join NASC with cluster.voronoi and cluster.hull
nasc.comp.ea <- nasc %>% 
  st_as_sf(coords = c("X", "Y"), crs = 3310) %>% 
  # Select only the necessary columns
  select(transect, Interval, cluster_orig = cluster) %>% 
  # Join cluster number from Voronoi tessellation
  st_join(select(cluster.voronoi.ea, cluster_voronoi = cluster)) %>% 
  # Join cluster number from convex hull (original method)
  st_join(select(cluster.hull.ea, cluster_hull = cluster)) %>%
  # Subtract original cluster number from new; values != 0 are different
  mutate(diff_voronoi = cluster_orig - cluster_voronoi,
         diff_hull    = cluster_orig - cluster_hull)

# Compare differences in cluster assignments on projected data (crs = 3310, CA Albers equal area)
ggplot() +
  geom_sf(data = cluster.voronoi.ea, aes(fill = factor(cluster)), show.legend = FALSE) +
  geom_sf(data = cluster.hull.ea, colour = "white", fill = NA, alpha = 0.5) +
  geom_sf(data = transects.sf, alpha = 0.5) +
  geom_sf(data = filter(nasc.comp.ea, diff_voronoi != 0), 
          aes(colour = diff_voronoi)) + 
  theme_bw()

# Save plot as image; open from the Files pane, and try zooming on your viewer to see details better than the Rstudio viewer
ggsave(filename = "figures/cluster_comparison_equal_area.png",
       height = 10)
