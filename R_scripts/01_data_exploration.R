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
library(raster)
library(terra)
library(rasterVis)
library(sp)
library(gridExtra)
library(grid)
library(ggplot2)
library(comprehenr)
#load the diver0 function which sets colors to divergin from 0
devtools::source_gist('306e4b7e69c87b1826db')


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
  
  final <- merge(p035r035, p035r036)
  time(final) <- year
  set.names(final, paste("yr", toString(year), sep=""))
  return(final)
}

PlotStudy <- function(treeCover, year, maxPercent){
  
  p <- levelplot(treeCover, 
            at=seq(0, maxPercent),
            main=toString(year),
            xlab='UTM meters',
            ylab='UTM meters',
            margin=FALSE,
            colorKey=list(title="Percent Tree Cover"))
  return(p)
  
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
  
  # remove tree cover pixels outside the project area
  zm_cover <- terra::mask(zm_cover, projectArea)
  
  return(zm_cover)
  
}

#### load data ####
#TODO: get .gdb file loaded of SW USFS Activities

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

# get max percent value for all
all <- c(zm_c_2015, zm_c_2010, zm_c_2005, zm_c_2000)
minMaxAll <- minmax(all)
maxPercent <- max(minMaxAll[2,])


#### Visualize ####
#plot options: default r (plot), ggplot, rasterVis
#TODO: make color scale bars consistent for comparison
p1 <- PlotStudy(zm_c_2015, 2015, maxPercent)
p2 <- PlotStudy(zm_c_2010, 2010, maxPercent)
p3 <- PlotStudy(zm_c_2005, 2005, maxPercent)
p4 <- PlotStudy(zm_c_2000, 2000, maxPercent)


jpeg("percentTreeCover.jpeg", height = 1024 * 0.707, width = 1024)  #0.707 is a convenient aspect.ratio
grid.arrange(p1, p2, p3, p4, 
                         ncol=2,
                         top=textGrob("Percent Tree Cover for the Puerco Project Area", gp=gpar(fontsize=25)))
dev.off()

# Density plots
#TODO: would be better to extract percent cover as 1d list and use ggplot's
#geom_density() function to have greater control over aesthetics
allYrsRaster <- raster::stack(raster(zm_c_2015), raster(zm_c_2010), raster(zm_c_2005), raster(zm_c_2000))
rasterVis::densityplot(allYrsRaster,
                       xlab='Percent Cover', 
                       ylab='Density', 
                       main='Kernel Density Plot for Percent Cover in the Puerco Project Area, 2000-2015',
                       draw.labels = FALSE,
                       lwd = 2)
                       #auto.key = list(space = c("right", "left", "left")))

#another way to show a kernel density plot:
#allYrs <- c(zm_c_2015, zm_c_2010, zm_c_2005, zm_c_2000)
#density(allYrs)
#density(zm_c_2010)
#density(zm_c_2005)
#density(zm_c_2000)


# Histograms
RelFreq <- function(rasterData){
  h <- as.data.frame(terra::freq(rasterData))
  numPix = sum(h$count)
  return(mutate(h, relFreq=count/numPix))
  
  
}

freq_2000 <- RelFreq(zm_c_2000) %>% mutate(year=2000)
freq_2005 <- RelFreq(zm_c_2005) %>% mutate(year=2005)
freq_2010 <- RelFreq(zm_c_2010) %>% mutate(year=2010)
freq_2015 <- RelFreq(zm_c_2015) %>% mutate(year=2015)
freqAll <- bind_rows(freq_2000, freq_2005, freq_2010, freq_2015)

p1 <- ggplot(data=freqAll, aes(factor(value), relFreq, fill=factor(year))) + geom_col( position=position_dodge2(10))
p1 <- p1 + labs(x="Percent Cover", y="relative frequency",title ="Frequencey Histogram for Precent Cover Values, 2000-2015")
print(p1)

HistYear <- function(freqAll, yr){
  p1 <- ggplot(data=filter(freqAll, year==yr), aes(factor(value), relFreq)) + geom_col( position=position_dodge2(10))
  p1 <- p1 + labs(x="Percent Cover", y="relative frequency",title =toString(yr))
  return(p1)
}


p1 <- HistYear(freqAll, 2015)
p2 <- HistYear(freqAll, 2010)
p3 <- HistYear(freqAll, 2005)
p4 <- HistYear(freqAll, 2000)

grid.arrange(p1, p2, p3, p4, 
             ncol=2,
             top=textGrob("Percent Tree Cover for the Puerco Project Area", gp=gpar(fontsize=25)))

##### Autocorrelation test #####
#TODO: figure out how to iterate over layered spatRaster
#zm_c_yrs <- c(zm_c_2015, zm_c_2010, zm_c_2005, zm_c_2000)
#autoCor <- to_list(for(yr in zm_c_yrs) terra::autocor(yr, global=FALSE))
#AutoCor <- function(zm_yrs){
#}

##Trying to applyt to all at once
all_zm <- terra::sds(zm_c_2015, zm_c_2010, zm_c_2005, zm_c_2000)

autocor_all <- terra::sapp(all, terra::terrain)
aa <- lapply(all, terra::autocor, global=FALSE)
aa <- c(aa[[1]], aa[[2]], aa[[3]], aa[[4]])

DivergePlot2 <- function(tc){
  p <- levelplot(tc,
                 margin=FALSE,)
  p <- diverge0(p, 'RdBu')
  return(p)
  
}
DivergePlot2(aa)
## 

autocor_2015 <- terra::autocor(zm_c_2015, global=FALSE)
autocor_2010 <- terra::autocor(zm_c_2010, global=FALSE)
autocor_2005 <- terra::autocor(zm_c_2005, global=FALSE)
autocor_2000 <- terra::autocor(zm_c_2000, global=FALSE)

DivergePlot <- function(rasterData, year){
  p <- levelplot(rasterData,
                 margin=FALSE,
                 main=toString(year))
  p <- diverge0(p, 'RdBu')
  return(p)
  
  
}

p4 <- DivergePlot(autocor_2000, 2000)
p3 <- DivergePlot(autocor_2005, 2005)
p2 <- DivergePlot(autocor_2010, 2010)
p1 <- DivergePlot(autocor_2015, 2015)

grid.arrange(p1, p2, p3, p4, 
             ncol=2,
             top=textGrob("Local Autocorrelation For Percent Tree Cover", gp=gpar(fontsize=25)))

