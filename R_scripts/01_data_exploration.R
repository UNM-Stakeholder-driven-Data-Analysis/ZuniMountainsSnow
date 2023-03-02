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


#### functions ####
YearFile <- function(year, path, row){
  identifier = paste("p", path, "r", row, "_TC_", toString(year), sep="")
  file_path <- file.path(".",
                         "data", 
                         paste("GFCC30TC_", toString(year), sep=""),
                         paste("GFCC30TC_", identifier, sep=""),
                         paste(identifier, ".tif", sep=""))
  return(file_path)
}

MergedRaster <- function(year){

  p035r035 <- rast(YearFile(year, "035", "035"))
  p035r036 <- rast(YearFile(year, "035", "036"))
  
  return(merge(p035r035, p035r036))
}

PlotStudy <- function(treeCover, projectArea, year){

  plot(treeCover)
  plot(projectArea, add=TRUE, alpha=0.35)
  title("Project Area and Tree Cover", toString(year))
  
}

OnlyTreeCover <- function(treeCover){
  #From https://lpdaac.usgs.gov/documents/1371/GFCC_User_Guide_V1.pdf
  # 0-100: percent tree cover
  # 200: water
  # 210: cloud
  # 211: shadow
  # 220: fill value
  replace_mat = cbind(id=c(200, 210, 211, 220), v=c(NaN, NaN, NaN, NaN))
  treeCover <- classify(treeCover, replace_mat)
  return(treeCover)
  
}

ProcessData <- function(treeCoverRaw, projectArea){
  #TODO: use pipes?
  
  # crop to study area
  zm_cover <- terra::crop(treeCoverRaw, projectArea)
  
  # remove non tree cover pixels
  zm_cover <- OnlyTreeCover(zm_cover)
  
  return(zm_cover)
  
}





#### load data ####
puerco_area_spat <- terra::vect("./data/Puerco Project Area/puerco_Project-polygon.shp")

tc2015 <- MergedRaster(2015)
tc2010 <- MergedRaster(2010)
tc2005 <- MergedRaster(2005)
tc2000 <- MergedRaster(2000)


#### Crop Data ####
puerco_area_spat <- terra::project(puerco_area_spat, tc2015)
puerco_area_raster <- terra::rasterize(puerco_area_spat, tc2015)
#manual extent is clunky but it allows trim operation to be wayyy faster
manual_extent = ext(700000, 750000, 3900000, 3950000) 
puerco_area_raster <- terra::crop(puerco_area_raster, manual_extent)
#visual check that we are not cropping parts of the project area
plot(puerco_area_raster) 
#remove border of NaN values
puerco_area_raster <- terra::trim(puerco_area_raster) 
plot(puerco_area_raster)


#### process Data ####

zm_c_2015 <- ProcessData(tc2015, puerco_area_raster)
zm_c_2010 <- ProcessData(tc2010, puerco_area_raster)
zm_c_2005 <- ProcessData(tc2005, puerco_area_raster)
zm_c_2000 <- ProcessData(tc2000, puerco_area_raster)
#TODO: verifiy only contains 0-100 value now


#### Visualize ####
#TODO: make color scale bars consistent for comparison
PlotStudy(zm_c_2015, puerco_area_raster, 2015)
PlotStudy(zm_c_2010, puerco_area_raster, 2010)
PlotStudy(zm_c_2005, puerco_area_raster, 2005)
PlotStudy(zm_c_2000, puerco_area_raster, 2000)
