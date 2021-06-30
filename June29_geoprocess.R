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
box <- st_as_sfc(st_bbox(bound.box))
                  

cluster <- cluster.summ.wt[which(!cluster.summ.wt$cluster %in% cluster.zero$cluster),]

##ggvoronoi

ggplot()+
  theme_bw()+ 
  coord_map()+
  geom_point(data=cluster, 
             aes(long, lat))+
  geom_text(data=cluster, 
            aes(long, lat, label=cluster), size=2, hjust=-1, vjust=0)+
  stat_voronoi(data=cluster,
               aes(long,lat), geom="path")+
  geom_path(data = nasc, aes(long, lat, group = transect))

