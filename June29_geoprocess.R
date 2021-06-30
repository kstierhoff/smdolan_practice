load(here::here("data/nasc_final.Rdata"))
load(here::here("data/catch_info.Rdata"))
library(sf)
library(ggvoronoi)
library(ggplot2)
library(sp)
library(dplyr)

nasc.spatialp <- sf::st_as_sf(nasc, coords=c("long", "lat"))
bound.box <- (sf::st_bbox(nasc.spatialp))
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
cluster <- cluster.summ.wt[which(!cluster.summ.wt$cluster %in% cluster.zero$cluster),]

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

