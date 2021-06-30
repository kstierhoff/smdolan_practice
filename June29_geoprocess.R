load(here::here("data/nasc_final.Rdata"))
load(here::here("data/catch_info.Rdata"))
library(sf)
library(ggvoronoi)
library(ggplot2)
library(sp)
library(dplyr)

nasc.sf <- sf::st_as_sf(nasc, coords=c("long", "lat"))
bound.box <- (sf::st_bbox(nasc.sf))
print(bound.box)
box.old <- st_as_sfc(st_bbox(bound.box))

#this box.old does not fit format of the outline function required in the stat_voronoi 
#and I'm not sure how to do it any other way than hard code a box using the
#min and max values from above

#bound box based off of min and max values from nasc transect data 

box <- data.frame(x= as.numeric(c("-129.25383", "-117.27371", "-117.27371","-129.2538371")),
                  y=as.numeric(c("50.75796", "50.757960", "32.13575","32.13575")),
                  group=as.numeric(c("1", "1", "1", "1")))


#remove clusters with zero catch 
cluster <- as.data.frame(cluster.summ.wt[which(!cluster.summ.wt$cluster %in% cluster.zero$cluster),])

##plot trawl clusters, transects, and voronoi tessellation 

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


#plot spdf in ggplot 

ggplot()+coord_map()+theme_bw()+
  geom_path(data = nasc, aes(long, lat))+
  geom_polygon(data=cluster.spdf,
               aes(x=long, y=lat, fill=group, group=group),color="black", alpha=0.8)+
  geom_point(data=cluster, 
             aes(long, lat))+
  geom_text(data=cluster, 
            aes(long, lat, label=cluster), size=3, hjust=-1, vjust=0)

mapview::mapview(cluster.spdf)

#convert to sf

cluster.sf <- as(cluster.spdf, "sf")
class(cluster.sf)

#maybe join? idk I'm working on it now

cluster.nasc.join <- st_join(cluster.sf, nasc.sf, join=st_intersects)

