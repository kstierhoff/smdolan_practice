# Load required libraries
library(tidyverse, here)

# Import acoustic backscatter data
load(here::here("data/nasc_final.Rdata"))

# Import trawl data
load("data/trawl_data.Rdata")

# Plot backscatter data
# There are many points, and it plots very slowly
ggplot() + 
  geom_point(data = nasc, aes(long, lat)) +
  coord_map() # By default, cartesian coordinates are used; you can use coord_map for lat/long

# Grouping by transect and plotting as a path is a bit faster just to visualize the data
ggplot() + 
  geom_path(data = nasc, aes(long, lat, group = transect)) +
  coord_map() # By default, cartesian coordinates are used; you can use coord_map for lat/long

