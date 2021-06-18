#6-15-2021 practice 
library(rvest)
library(pacman)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)
library(scatterpie)
#
#previous version of loading data from ERDDAP, commented out because I wanted to try coding r to generate the html link 
#
#url <- "https://coastwatch.pfeg.noaa.gov/erddap/tabledap/FRDCPSTrawlLHSpecimen.csv0?cruise%2Cship%2Chaul%2Ccollection%2Clatitude%2Clongitude%2Ctime%2Citis_tsn%2Cscientific_name%2Cspecimen_number%2Csex%2Cis_random_sample%2Cweight%2Cstandard_length%2Cfork_length%2Ctotal_length%2Cmantle_length&time%3E=2019-09-01T00%3A00%3A00Z&time%3C=2019-09-08T09%3A25%3A00Z"
# Download and parse ERDDAP catch data
#l.frequency <- data.frame(read_csv(url, col_names=FALSE))
# Apply names to the columns
#names(l.frequency) <-  c("cruise","ship","haul","collection","lat","long",
#                        "time", "itis_tsn", "scientificName", "specimenNumber", "sex", 
#                      "randomSample", "weight", "standardLength", "forkLength", 
#                       "totalLength", "mantleLength")
#
#
#
#New way of loading ERDDAP data
#
# Define ERDDAP data variables
erddap.data.type     <- "FRDCPSTrawlLHSpecimen"    # RL Life history of specimens
erddap.survey.start  <- "2019-09-01" # Start of survey for ERDDAP vessel data query
erddap.survey.end    <- "2019-09-08" # End of survey for ERDDAP vessel data query
erddap.vars          <- c("haul,latitude,longitude,scientific_name, specimen_number, weight, standard_length, fork_length, total_length, mantle_length") # Variables to be downloaded
erddap.classes       <- c("numeric", "numeric", "numeric", "character","numeric", "numeric", "numeric", "numeric", "numeric", "numeric") # Data classes
erddap.headers       <- c("haul", "lat", "long", "scientific.name", "species.number", "weight", "s.length", "f.length", "t.length", "m.length") # Column names
survey.lat           <- c(32,51) # Latitude range
survey.long          <- c(-130,-117) # Longitude range
#
# Generate ERDDAP URL
dataURL <- URLencode(paste(
  "https://coastwatch.pfeg.noaa.gov/erddap/tabledap/",
  erddap.data.type, ".csv0?", erddap.vars,
  "&time>=", erddap.survey.start, "&time<=", erddap.survey.end,
  sep = ""))
#
# Download and parse ERDDAP LH data
LH.specimen <- data.frame(read.csv(dataURL, header = FALSE, colClasses = erddap.classes, 
                                   row.names = NULL, skip = 0))
names(LH.specimen) <- erddap.headers
#
# Following code is data manipulation to be able to get a frequency per fish species per haul 
#
LH.specimen <- LH.specimen %>%
  mutate(key=(paste(haul, scientific.name))) #add key column which has both haul and species name 

speciesNumber <-  count(LH.specimen$key) %>% #count the number of each species per haul using the key 
  mutate(haul=substr(x, 1, 3), #extract the haul number into a new column 
         scientificName=substr(x, 4,50)) #extract the scientific name into a new column 

# Try this, may be a little cleaner
# speciesNumber <- LH.specimen %>% 
#   group_by(haul, scientific.name) %>% 
#   tally()

#
TotalperHaul <- speciesNumber %>% #determine the total number of fish per haul in a new table 
  group_by(haul) %>%
  summarize_at(vars(freq),
               Total<-list(Total=sum))
#
#
Combined <- merge(TotalperHaul, speciesNumber, by = "haul", all=TRUE) 
Combined <-Combined %>% #merge the two tables so now I have number of individuals per species per haul, total number of individuals in a haul, and haul number
  mutate(PercentCatch= freq/Total) #calculate percent catch so now I can make a series of pie charts and they'll be based on similar values 
#
#honestly don't recall why I didn't just add a column to SpeciesNumber table but maybe it was because 
#I didn't want to lose this information and wanted to keep it separate in case
#
#add xy data back to combined table (to be used for mapping pie charts further along)
#
xydata <- LH.specimen %>% #extract long and lat from original table, kept haul to be able to merge datasets
  select("haul", "long", "lat")
#
xydata <- xydata[!duplicated(xydata$haul),] #removed duplicate of rows with same haul and long and lat data
#  
Combined <- merge(Combined, xydata, by="haul", all=TRUE) #merge two datasets using haul as the common factor 
Combined$haul <- as.numeric(Combined$haul) #made haul value numeric 
#
#pie chart for percent catch by individual in each species per haul
#
pies <- ggplot(data=Combined)+ theme_bw()+
  geom_bar(aes(x="", y=PercentCatch, fill=scientificName), 
           stat= "identity", color="white") +
  coord_polar("y",start=0)+
  facet_wrap(~haul)+
  labs(title="Percentage of Total Catch per Haul (number of individuals per species/total number in haul)",
       x=" ",
       y="Percent Catch")+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank())
pies
#
#
#now for some more data manipulation to be able to plot pie charts on a scatter plot 
#
species.catch.perc <- Combined %>%
  select("haul", "scientificName", "lat", "long", "PercentCatch")%>%
  mutate(squid=Combined$scientificName==" Doryteuthis (Loligo) opalescens",
         northern.anchovy=Combined$scientificName==" Engraulis mordax",
         pacific.sardine=Combined$scientificName==" Sardinops sagax",
         pacific.mackerel=Combined$scientificName==" Scomber japonicus",
         jack.mackerel=Combined$scientificName==" Trachurus symmetricus",
         blue.shark=Combined$scientificName==" Prionace glauca")
#results in true/false values
#want to assign False values 0 and TRUE values the appropriate percent catch value in the same row 
#
species.catch.perc$squid <- ifelse(species.catch.perc$squid %in% c('FALSE'), 0, species.catch.perc$PercentCatch)
species.catch.perc$northern.anchovy <- ifelse(species.catch.perc$northern.anchovy %in% c('FALSE'), 0, species.catch.perc$PercentCatch)
species.catch.perc$pacific.sardine <- ifelse(species.catch.perc$pacific.sardine %in% c('FALSE'), 0, species.catch.perc$PercentCatch)
species.catch.perc$pacific.mackerel <- ifelse(species.catch.perc$pacific.mackerel %in% c('FALSE'), 0, species.catch.perc$PercentCatch)
species.catch.perc$jack.mackerel <- ifelse(species.catch.perc$jack.mackerel %in% c('FALSE'), 0, species.catch.perc$PercentCatch)
species.catch.perc$blue.shark <- ifelse(species.catch.perc$blue.shark %in% c('FALSE'), 0, species.catch.perc$PercentCatch)
#
#test to see if scatterpie works the way I imagined it would 
#
ggplot()+
  geom_scatterpie(data=species.catch.perc,
                  aes(x=lat, y=long),
                  cols=colnames(species.catch.perc[,c(6:11)]),
                  legend_name="CommonName")+
  coord_equal()
#
# add CA state for context 
mapdata<-map_data("state") %>%
  filter(region=="california")
#
pie.map <- ggplot()+ theme_bw()+
  coord_map()+
  #
  #
  geom_polygon(data=mapdata,  #plot CA for context
               aes(x=long, y=lat,))+
  #
  geom_scatterpie(data=species.catch.perc,
                  aes(x=long, y=lat),
                  cols=colnames(species.catch.perc[,c(6:11)]),
                  legend_name="CommonName")+
  labs(title = "Percent of Individuals Per Species per Haul",
       x="Latitude",
       y="Longitude")
#
pie.map 
#
mapdata<-map_data("state") %>%
  filter(region=="california")
#
pie.map.zoomedin <- ggplot()+ theme_bw()+
  # ylim(32, 34.25)+
  # xlim(-119.25,-117.5)+
  coord_map(ylim=c(32, 34.25),
            xlim=c(-119.25,-117.5)) +
  #
  geom_polygon(data=mapdata,  #plot CA for context
               aes(x=long, y=lat,))+
  #
  geom_scatterpie(data=species.catch.perc,
                  aes(x=long, y=lat),
                  cols=colnames(species.catch.perc[,c(6:11)]),
                  legend_name="CommonName")+
  labs(title = "Percent of Individuals Per Species per Haul",
       x="Latitude",
       y="Longitude")
#
pie.map.zoomedin          
#
#not sure how to prevent the CA coastline from getting distorted and wonky looking in the zoomed in version

# If you put the xlim and ylim in the coord_*() function, it'll plot correctly
