##### read me ####
# the purpose of this script is to visualize spatial tree cover data for the 
# zuni mountains as well as
# - merge TC images
# - crop merged TC image to project area
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

puerco_area_spat <- terra::vect("./data/Puerco Project Area/puerco_Project-polygon.shp")


zm <- merge(p035r035_TC_2015, p035r036_TC_2015)
plot(zm)

#### Crop by Puerco Project Area ####

terra::ext(puerco_area_spat)
puerco_area_spat <- terra::project(puerco_area_spat, zm)
terra::ext(puerco_area_spat)

puerco_area_raster <- terra::rasterize(puerco_area_spat, zm)
#manual extent is clunky but it allows trim operation to be wayyy faster
manual_extent = ext(700000, 750000, 3900000, 3950000) 
puerco_area_raster <- terra::crop(puerco_area_raster, manual_extent)
#visual check taht we are not cropping parts of the project area
plot(puerco_area_raster) 
#remove border of NaN values
puerco_area_raster <- terra::trim(puerco_area_raster) 
plot(puerco_area_raster)

#zuni mountains cropped
zm_c <- terra::crop(zm, puerco_area_raster)
plot(zm_c)
plot(puerco_area_raster, add=TRUE)
