##### read me ####
# the purpose of this script is to visualize spatial tree cover data for the 
# zuni mountains as well as
# - describe dataset size
# - describing data types
# - checking data distributions
# - checking for spatial autocorrelation
# - checking for temporal autocorrelation
# - checking for correlation between variables <?

#### Libraries ####
library(tidyverse)
library(raster)
#NOTE rgdal is going to be retired by end of 2023. work for now so leaving it
# See https://r-spatial.org/r/2022/04/12/evolution.html and 
# https://github.com/r-spatial/evolution
library(rgdal)
library(sf)
#### load data ####
p034r035_TC_2015 <- raster("./data/GFCC30TC_2015/GFCC30TC_p034r035_TC_2015/p034r035_TC_2015.tif")
p034r035_TC_2015
hasValues(p034r035_TC_2015)
inMemory(p034r035_TC_2015)

plot(p034r035_TC_2015)

# Load Puerco Project area shp file
puerco_area <- st_read(dsn="./data/Puerco Project Area/puerco_Project-polygon.shp")
plot(puerco_area)
