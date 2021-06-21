# Install and load pacman (library management package) -------------------------
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse, lubridate, here, sf, mapview)

# Install and load required packages from Github -------------------------------
# atm
pacman::p_load_gh("kstierhoff/atm")

# Load required data -----------------------------------------------------------
# Load backscatter data (nasc)
## These are the integrated backscatter for each 100-m transect interval
## They contain the total CPS backscatter, backscattering coefficients, 
## and acoustic proportions for each species, which is all that's needed to compute
## the acoustic biomass point estimate (some might call this the "mean" biomass)

load(here::here("data/nasc_final.Rdata"))

# Load stratum definitions (strata.final)
## These are the stratum definitions for each species
## Most importantly, for each species, it identifies all transects with a given species present
## and which strata each transect belongs to
load(here::here("data/strata_info.Rdata"))

## View the info for anchovy
filter(strata.final, scientificName == "Engraulis mordax")

# Load stratum polygons (strata.primary)
## This is a simple feature object which contains polygons drawn around each stratum
## for each species, used to compute the area of the stratum
load(here::here("data/strata_primary_final.Rdata"))

# View stratum polygons for anchovy; symbolize by the stratum number
mapview::mapview(filter(strata.primary, scientificName == "Engraulis mordax"),
                 zcol = "stratum")

# Generate the point estimate for anchovy
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
point.estimate.i <- data.frame(scientificName = i,
                               estimate_point(nasc.temp, stratum.info, species = i))

# View point estimates
point.estimate.i

# Challenges
## 1) For each species, generate the point estimate and combine into one data frame
## Hint: create a for loop, where i updates for each species
