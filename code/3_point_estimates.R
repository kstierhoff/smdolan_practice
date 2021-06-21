# Install and load pacman (library management package) -------------------------
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse, lubridate, here, sf, mapview)

# Install and load required packages from Github -------------------------------
# atm
pacman::p_load_gh("kstierhoff/atm")

# Load backscatter data
load(here::here("data/nasc_final.Rdata"))

# Load strata.final
## These are the 
load(here::here("data/strata_info.Rdata"))

# Load strata.primary
## 
load(here::here("data/strata_primary_final.Rdata"))

# Load cluster length frequency data

# Generate point estimate for anchovy
## i is used to loop through the various species; I set it to anchovy for testing
i = "Engraulis mordax"

# Subset strata for species i
strata.temp <- filter(strata.final, scientificName == i) %>% 
  select(transect, stratum)

# Add stratum numbers to nasc
nasc.temp <- nasc %>%
  left_join(strata.temp) %>% 
  filter(!is.na(stratum))

# Create data frame with stratum and area (m^2)  
stratum.info <- strata.primary %>%
  filter(scientificName == i) %>%
  select(stratum, area) %>%
  mutate(area = as.numeric(area)) %>%
  st_set_geometry(NULL) 

# Generate point estimate for anchovy
point.estimate <- data.frame(scientificName = i,
                             estimate_point(nasc.temp, stratum.info, species = i))
