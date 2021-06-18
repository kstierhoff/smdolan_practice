# Load libraries
library(tidyverse)
library(lubridate)

# Define ERDDAP data variables
erddap.vessel        <- "WTEG"    # Lasker == WTEG; Shimada == WTED; add "nrt" if during survey
erddap.survey.start  <- "2019-06-12" # Start of survey for ERDDAP vessel data query
erddap.survey.end    <- "2019-09-10" # End of survey for ERDDAP vessel data query
erddap.vars          <- c("time,latitude,longitude,seaTemperature,platformSpeed") # Variables to be downloaded
erddap.classes       <- c("factor", "numeric", "numeric", "numeric","numeric") # Data classes
erddap.headers       <- c("time", "lat", "long", "SST", "SOG") # Column names
survey.lat           <- c(32,51) # Latitude range
survey.long          <- c(-130,-117) # Longitude range

# Define leg breaks; used by cut() to classify data by survey leg
leg.breaks <- as.numeric(lubridate::ymd(c("2019-06-12", "2019-07-06", 
                                          "2019-07-29", "2019-08-20",
                                          "2019-09-10")))
# Generate ERDDAP URL
dataURL <- URLencode(paste(
  "http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip",
  erddap.vessel, ".csv0?", erddap.vars,
  "&time>=", erddap.survey.start, "&time<=", erddap.survey.end,
  sep = ""))

# Download and parse ERDDAP nav data
nav.temp <- data.frame(read.csv(dataURL, header = F, colClasses = erddap.classes, 
                                row.names = NULL, skip = 0))
# Rename columns
names(nav.temp) <- erddap.headers

# Filter to remove bad SST values and data outside the geographic range of the survey
nav <- nav.temp %>% 
  mutate(long     = long - 360, # Data are in a 0-360 range, not +/- 180 deg
         SOG      = SOG * 1.94384, # Convert from m/s to knots
         datetime = ymd_hms(time),
         SST      = na_if(SST, NaN),
         leg      = paste("Leg", cut(as.numeric(date(datetime)), 
                                     leg.breaks, labels = F))) %>%
  filter(is.nan(SOG) == F, SOG > 0, SOG < 15,
         between(lat, min(survey.lat), max(survey.lat)), 
         between(long, min(survey.long), max(survey.long)))

# Plot navigation data
ggplot(nav, aes(long, lat, group = leg, colour = leg)) + 
  geom_path() + 
  coord_map() + 
  theme_bw()
