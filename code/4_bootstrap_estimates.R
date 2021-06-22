# Load cluster data (cluster.final) and length frequency table (lf.final) 
load(here::here("data/cluster_length_frequency_all.Rdata"))
load(here::here("data/cluster_length_frequency_tables.Rdata"))

# Specify the number of bootstrap samples
boot.num <- 5
do.lf    <- TRUE

# Species to generate bootstrap estimates
bootstrap.est.spp      <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")

# Generate multiple bootstrap biomass estimates
# Create data frame for biomass estimates
bootstrap.estimates <- data.frame()
# Create data frame for abundance estimates by length
abundance.estimates <- data.frame()

# Configure progress bar
pb1 <- tkProgressBar("R Progress Bar", 
                     "Multiple Bootstrap Estimation - Species", 0, 100, 0)
spp.counter <- 1

for (i in unique(strata.final$scientificName)) {
  # Get vector of lengths from clf.df column names
  L.cols  <- grep("L\\d",names(cluster.final[[i]]))
  L.vec   <- sort(as.numeric(str_extract(names(cluster.final[[i]][L.cols]),"\\d{1,2}")))
  
  # Create data frame with stratum and area (m^2)
  stratum.info <- strata.primary %>% 
    select(scientificName, stratum, area) %>%
    mutate(area = as.numeric(area)) %>% 
    st_set_geometry(NULL) 
  
  # Summarize strata areas per species and stock
  strata.summ.primary <- strata.primary %>% 
    select(scientificName, stratum, stock, area) %>%
    mutate(area = as.numeric(area)) %>% 
    st_set_geometry(NULL)
  
  # Subset strata for species i
  strata.temp <- filter(strata.final, scientificName == i) %>% 
    select(transect, stratum) %>% 
    left_join(filter(strata.summ.primary, scientificName == i)) 
  
  # Add stratum numbers to nasc and remove transects outside of defined strata
  nasc.temp <- nasc %>%
    left_join(strata.temp) %>% 
    filter(!is.na(stratum))
  
  # Summarize nasc.temp to get strata to merge with pos.clusters below
  nasc.temp.summ <- nasc.temp %>% 
    group_by(stratum, cluster) %>% 
    summarise(n = n_distinct(cluster))
  
  # Configure progress bar
  pb2 <- tkProgressBar("R Progress Bar", "Multiple Bootstrap Estimation - Stratum", 0, 100, 0)
  # Initialize species counter
  stratum.counter <- 1
  # Estimate biomass for each stratum
  for (j in unique(nasc.temp$stratum)) {
    # Extract stratum area
    stratum.area <- stratum.info$area[stratum.info$scientificName == i & 
                                        stratum.info$stratum == j]
    # Calculate biomass using bootstrap function ----
    set.seed(1) # Set seed for repeatable results
    boot.df <- estimate_bootstrap(nasc.temp, cluster.final[[i]], j, 
                                  stratum.area = stratum.area, 
                                  species = i, do.lf = do.lf, 
                                  boot.number = boot.num)$data.frame
    
    # Extract biomass estimates; remove first (point) estimate
    boot.temp <- data.frame(Species = i, Stratum = j, Area = stratum.area,
                            Sample = seq(1,boot.num), boot.df[2:nrow(boot.df), ])
    # Combine results
    bootstrap.estimates <- bind_rows(bootstrap.estimates, boot.temp)
    
    # Calculate abundance by length class using bootstrap function ----
    abund.vec <- estimate_bootstrap(nasc.temp, cluster.final[[i]], j, 
                                    stratum.area = stratum.area, 
                                    species = i, do.lf = do.lf, 
                                    boot.number = 0)$abundance.vector
    # Extract abundance estimates
    abundance.temp <- data.frame(Species = i, Stratum = j,
                                 SL = L.vec, freq = abund.vec)
    # Combine results
    abundance.estimates <- bind_rows(abundance.estimates, abundance.temp)
    
    # Update the progress bar
    pb.prog2 <- round(stratum.counter/n_distinct(nasc.temp$stratum)*100)
    info2 <- sprintf("%d%% done", pb.prog2)
    setTkProgressBar(pb2, pb.prog2, sprintf("Bootstrap - Stratum (%s)", info2), info2)
    # Update stratum counter
    stratum.counter <- stratum.counter + 1      
  }
  # Close the stratum counter
  close(pb2)
  # Update the progress bar
  pb.prog1 <- round(spp.counter/length(bootstrap.est.spp)*100)
  info1    <- sprintf("%d%% done", pb.prog1)
  setTkProgressBar(pb1, pb.prog1, sprintf("Bootstrap - Species (%s)", info1), info1)
  
  # Update the species counter
  spp.counter     <- spp.counter + 1
}

# Close the species counter
close(pb1)
