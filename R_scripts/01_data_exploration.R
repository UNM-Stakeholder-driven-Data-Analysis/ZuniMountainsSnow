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
library(terra)

#### load data ####

p034r035_TC_2015 <- rast("./data/GFCC30TC_2015/GFCC30TC_p034r035_TC_2015/p034r035_TC_2015.tif")
p034r036_TC_2015 <- rast("./data/GFCC30TC_2015/GFCC30TC_p034r036_TC_2015/p034r036_TC_2015.tif")
#TODO: once can download again try to find western edge of zunis
#this one appears to be way far away
p035r035_TC_2015 <- rast("./data/GFCC30TC_2015/GFCC30TC_p035r035_TC_2015/p035r035_TC_2015.tif")
#also appears to be way far away
p035r036_TC_2015 <- rast("./data/GFCC30TC_2015/GFCC30TC_p035r036_TC_2015/p035r036_TC_2015.tif")

p036r035_TC_2015 <- rast("./data/GFCC30TC_2015/GFCC30TC_p036r035_TC_2015/p036r035_TC_2015.tif")

new <- merge(p035r035_TC_2015, p035r036_TC_2015)
plot(new)



# Load Puerco Project area shp file
#puerco_area <- st_read(dsn="./data/Puerco Project Area/puerco_Project-polygon.shp")
puerco_area_spat <- terra::vect("./data/Puerco Project Area/puerco_Project-polygon.shp")
puerco_area_spat <- terra::project(puerco_area_spat, "EPSG:32612")

puerco_area_raster = terra::rasterize(puerco_area_spat, new)
