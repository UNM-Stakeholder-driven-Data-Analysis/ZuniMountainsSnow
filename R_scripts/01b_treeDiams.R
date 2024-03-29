#### read me ####
#the purpose of this script is to analyse the distributino of canopy diameters
#these were found by outputing a kml file of points to be measured in google earth
#then manually measuring those points and saving that info as a csv

#### libraries ####
library(sf)
library(terra)
library(dplyr)
library(readr)
source("./R_Scripts/00_Functions.R")

#### load data ####
forests <- sf::st_read("./data/ProclaimedForest.kml")
act_raw <- sf::st_read("./data/ActivityPolygon/Activities.gdb")

##### Isolate Thinning Polygons in Zuni Mountains ####
#TODO: refactor into a function
CIBOLA_FOREST = "03" #this is inferred by inspecting the map at the link:
#https://usfs.maps.arcgis.com/apps/mapviewer/index.html?layers=eb8f23442f374ea2adae683e6eb0f16a

tc2005 <- MergedRaster(2005)
zuni_forest <- forests[forests$Name=="Zuni Mountains", 1]
zuni_forest <- sf::st_transform(zuni_forest, crs=st_crs(tc2005))

yr2015 <- GetRangeDateTime(2015)
yr2010 <- GetRangeDateTime(2010)
yr2005 <- GetRangeDateTime(2005)
yr2000 <- GetRangeDateTime(2000)

dtRanges <- tibble(yr2000, yr2005, yr2010, yr2015)
START_DATE = dtRanges$yr2000["start"]
END_DATE = dtRanges$yr2015["end"]


all_cibola <- act_raw %>% 
  filter(AU_FOREST_CODE==CIBOLA_FOREST) %>% #filter immediately to increase processing speed
  filter(between(DATE_COMPLETED, START_DATE, END_DATE)) %>%
  filter(!ACTIVITY %in% NO_ALTER_TC) %>%
  sf::st_transform(crs=st_crs(tc2005))

intersection <- sf::st_intersects(zuni_forest, all_cibola)[[1]]
all_zuni <- all_cibola[intersection, ]

#### isolate to thinning, 2014####
haz_fr <- all_zuni %>% 
  filter(ACTIVITY=="Thinning for Hazardous Fuels Reduction") %>%
  filter(DateNotIn(DATE_COMPLETED, dtRanges))
all_other <- filter(all_zuni, ACTIVITY!="Thinning for Hazardous Fuels Reduction")
haz_fr_iso <- st_difference(haz_fr, st_union(all_other))
haz_fr_merged <- aggregate(haz_fr_iso, 
                           by = list(haz_fr_iso$DATE_COMPLETED),
                           FUN=function(x) x) #dummy function

p2014 <- "2014-09-29 18:00:00"
dateCompleted <- p2014

one_polyVec <- terra::vect(filter(haz_fr_merged, Group.1==dateCompleted))

#### save kml files ####
writeVector(one_polyVec, "./R_output/polyThin2014-09-29.kml", overwrite=TRUE)
randSamp <- spatSample(one_polyVec, size=20)
set.seed(1)
writeVector(randSamp, "./R_output/polySample.kml", overwrite=TRUE)

#### csv from manually measured canopies in google earth ####
diams <- read_delim("./R_output/canopyDiameters_2015.csv")

diams5 <- as_tibble(fivenum(diams$diameter))

ggplot(diams) + 
  geom_boxplot(aes(x="", y=diameter))+
  geom_jitter(aes(x="", y=diameter), width=.01, alpha=0.5, size=2) +
  geom_text(data=diams5, aes(x="", y=value, label=value), nudge_x = -0.45) +
  labs(title="Tree Canopy Diameters for D2-W BW Thinning Area \nGoogle Earth 2015",
      y="Canopy Diameter", 
       x="") +
  theme_bw()


#ggsave("./GeneratedPlots/crownDiams.jpeg", width=WIDTHSMALL, height=HEIGHTSMALL, units = "in")
ggsave("./GeneratedPlots/crownDiams.jpeg")
