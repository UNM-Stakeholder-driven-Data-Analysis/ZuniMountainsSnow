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
library(assertthat)
library(tidyverse)
library(raster)
#NOTE rgdal is going to be retired by end of 2023. work for now so leaving it
# See https://r-spatial.org/r/2022/04/12/evolution.html and 
# https://github.com/r-spatial/evolution
library(rgdal)
library(sf)
library(spdep)
library(raster)
library(terra)
library(rasterVis)
library(sp)
library(gridExtra)
library(grid)
library(ggplot2)
library(comprehenr)
library(dplyr)
library(xml2)
library(XML)
#load the diver0 function which sets colors to divergin from 0
devtools::source_gist('306e4b7e69c87b1826db')


#### constants ####
options(digits=2)
im.width = 1024
aspect.r = 0.707 #0.707 is a conve nient aspect.ratio
queens = matrix(c(1,1,1,1,0,1,1,1,1),3)
rooks = matrix(c(0,1,0,1,0,1,0,1,0),3)
bishops = matrix(c(1,0,1,0,0,0,1,0,1),3)

imgFolder = "./GeneratedPlots"



#### functions ####
Identifier <- function(year, path, row){
  return(paste("p", path, "r", row, "_TC_", toString(year), sep=""))
}

YearFileIntermediate <- function(year, path, row, suffix){
  #useful because have file with same name as folder
  identifier <- Identifier(year, path, row)
  file_path <- file.path(".",
                         "data", 
                         paste("GFCC30TC_", toString(year), sep=""),
                         paste("GFCC30TC_", identifier, suffix, sep=""))
  return(file_path)
}

YearFile <- function(year, path, row, suffix=".tif"){
  identifier <- Identifier(year, path, row)
  file_path <- file.path(YearFileIntermediate(year, path, row, suffix=""),
                         paste(identifier, suffix, sep=""))
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

GetRangeDateTime <- function(yr){
  # wrapper to make sure both images composing the zuni mountains have the same
  # datetime range
  rangeA <- RangeDateTime(YearFileIntermediate(yr, "035", "036", suffix=".zip.xml"))
  rangeB <- RangeDateTime(YearFileIntermediate(yr, "035", "035", suffix=".zip.xml"))
  assert_that(all(rangeA == rangeB))
  return(rangeA)
}

RangeDateTime <- function(path){
  xmlData <- xmlParse(read_xml(path))
  dtimeRange <- xmlToDataFrame(nodes=getNodeSet(xmlData, "//RangeDateTime"))
  endDT <- as.POSIXct(paste(dtimeRange$RangeEndingDate, dtimeRange$RangeEndingTime))
  startDT <- as.POSIXct(paste(dtimeRange$RangeBeginningDate, dtimeRange$RangeBeginningTime))
  
  out <- c(startDT, endDT)
  out <- set_names(out, c("start", "end"))
  return(out)
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

DateNotIn <- function(date, ranges){
  #return True if the date does not fall in any of the datetime ranges
  #INPUT:
  # date: posixct datetime
  # ranges: tibble of dates
  val = TRUE
  for (dtRange in ranges){
    val = val & !(date >= dtRange["start"] & date <= dtRange["end"])
  }
  return(unname(val))
}

#### load data ####

puerco_area_spat <- terra::vect("./data/Puerco Project Area/puerco_Project-polygon.shp")

yr2015 <- GetRangeDateTime(2015)
yr2010 <- GetRangeDateTime(2010)
yr2005 <- GetRangeDateTime(2005)
yr2000 <- GetRangeDateTime(2000)

dtRanges <- tibble(yr2000, yr2005, yr2010, yr2015)
#TODO: figure out if want to store dtRange within the spatRaster or as 
# a separate vector

tc2015 <- MergedRaster(2015)
tc2010 <- MergedRaster(2010)
tc2005 <- MergedRaster(2005)
tc2000 <- MergedRaster(2000)

START_DATE = dtRange2000["start"]
END_DATE = dtRange2015["end"]


#For some reason tc2010 has a different coord. ref designation that omits
# the datum, although the UTM zone is the same.
tc2010 <- terra::project(tc2010, tc2015)



#### Get Puerco Project Area ####
# TODO: remove this code, it is deprecated
puerco_area_spat <- terra::project(puerco_area_spat, tc2015)
puerco_area_raster <- terra::rasterize(puerco_area_spat, tc2015)
#manual extent is clunky but it allows trim operation to be wayyy faster
manual_extent = ext(700000, 750000, 3900000, 3950000) 
puerco_area_raster <- terra::crop(puerco_area_raster, manual_extent)
#visual check that we are not cropping parts of the project area
#plot(puerco_area_raster) 
#remove border of NaN values
puerco_area_raster <- terra::trim(puerco_area_raster) 
plot(puerco_area_raster)


##### Thinning in Zuni Mountains ####
CIBOLA_FOREST = "03" #this is inferred by inspecting the map at the link:
#https://usfs.maps.arcgis.com/apps/mapviewer/index.html?layers=eb8f23442f374ea2adae683e6eb0f16a


#TODO: this should not rely on puerco_area but instead the tc data
puerco_area <- sf::st_as_sf(puerco_area_spat)

forests <- sf::st_read("./data/ProclaimedForest.kml")
zuni_forest <- forests[forests$Name=="Zuni Mountains", 1]
zuni_forest <- sf::st_transform(zuni_forest, crs=st_crs(puerco_area))

#sf can get the .gdb without issue, but maybe we want to convert everything to terra (spatVectors)
# so that we can do the other operations more easily
act_raw <- sf::st_read("./data/ActivityPolygon/Activities.gdb")
all_cibola <- act_raw %>% 
  filter(AU_FOREST_CODE==CIBOLA_FOREST) %>% #filter immediately to increase processing speed
  filter(between(DATE_COMPLETED, START_DATE, END_DATE)) %>%
  sf::st_transform(crs=st_crs(puerco_area))

intersection <- sf::st_intersects(zuni_forest, all_cibola)[[1]]
all_zuni <- all_cibola[intersection, ]

all_thin <- filter(all_zuni, grepl("thin", ACTIVITY, ignore.case=TRUE))

#remove TSI Need Created- Precommercial Thin and SI Need (precommercial thinning) Eliminated
# I don't think those are actuall thinning activities
all_thin <- filter(all_thin, !grepl("Need", ACTIVITY)) 

haz_fr <- filter(all_thin, ACTIVITY=="Thinning for Hazardous Fuels Reduction")
haz_fr <- filter(haz_fr, DateNotIn(DATE_COMPLETED, dtRanges))

all_other <- filter(all_zuni, ACTIVITY!="Thinning for Hazardous Fuels Reduction")
st_erase = function(x, y) st_difference(x, st_union(y))
haz_fr_iso <- st_erase(haz_fr, all_other)

#TODO: switch to ggplot as it seems to have reasonable suport fot this mapping stuff.
# or convert to spatVector and plot from their? Too many options!!
plot(zuni_forest["Name"], reset=FALSE, border=1, col=NA)
plot(all_zuni["ACTIVITY"], add = TRUE, border=NA)


plot(haz_fr["DATE_COMPLETED"], add=TRUE, border=NA)
plot(all_other, add=TRUE, border=NA, col="blue")
plot(haz_fr, add=TRUE, border=NA, col="yellow")
plot(haz_fr_iso, add=TRUE, border=NA, col="magenta")

plot(haz_fr_iso["DATE_COMPLETED"], border=NA)

#number of unique thinning events (not necessarily one polygon) that occured:
length(unique(haz_fr$DATE_COMPLETED))

#arbitrarily choose a date_completed and show all polygons associated with it
date_chosen <- unique(act_range$DATE_COMPLETED)[8]
sub_df <- filter(act_range, DATE_COMPLETED==date_chosen)
plot(sub_df, add=TRUE, border=NA, col="magenta")

# This polygon was identified using arc map
# it is a "thinning for haz fuel reduction" and occurs between 2010 and 2015
# and doesn't overlap with other treatments
poly_id <- "{AF6C2148-55FF-485C-93C1-7CA862749E18}"
one_poly_sf <- activities[activities$REPLICATION_ID==poly_id,]


one_poly <- terra::vect(one_poly_sf)
poly_ext <- ext(one_poly)
one_poly <- terra::rasterize(one_poly, tc2015)
one_poly <- terra::crop(one_poly, poly_ext)
plot(one_poly)
#### process Data ####

zm_c_2015 <- ProcessData(tc2015, one_poly)
zm_c_2010 <- ProcessData(tc2010, one_poly)
zm_c_2005 <- ProcessData(tc2005, one_poly)
zm_c_2000 <- ProcessData(tc2000, one_poly)
#TODO: verifiy only contains 0-100 value now

# get max percent value for all
all <- c(zm_c_2015, zm_c_2010, zm_c_2005, zm_c_2000)
minMaxAll <- minmax(all)
maxPercent <- max(minMaxAll[2,])



#### Visualize ####


# PERCENT COVER PLOTS
jpeg(file.path(imgFolder, "percentTreeCover.jpeg"), height = 1024 * aspect.r, width = 1024)
levelplot(all, 
               at=seq(0, maxPercent),
               main="Percent Tree cover for the Puerco Project Area",
               xlab='UTM meters',
               ylab='UTM meters',
               margin=FALSE)
dev.off()

# DIFFERENCE PLOTS
diffs <- c(all$yr2015 - all$yr2010, all$yr2010-all$yr2005, all$yr2005-all$yr2000)
names(diffs) <- c("yr2015vs2010", "yr2010vs2005", "yr2005vs2000")

jpeg(file.path(imgFolder, "diffPercentTreeCover.jpeg"), height = 1024 * aspect.r, width = 1024)
p <- levelplot(diffs, 
          #at=seq(0, maxPercent),
          main="Difference in Percent Tree cover for the Puerco Project Area",
          xlab='UTM meters',
          ylab='UTM meters',
          margin=FALSE,
          par.settings=list(panel.background=list(col="lightgrey"), 
                            strip.background=list(col="white")))
diverge0(p, 'PiYG')
dev.off()



### KERNEL DENSITY PLOTS ###
#TODO: would be better to extract percent cover as 1d list and use ggplot's
#geom_density() function to have greater control over aesthetics
allYrsRaster <- raster::stack(raster(zm_c_2015), raster(zm_c_2010), raster(zm_c_2005), raster(zm_c_2000))
jpeg(file.path(imgFolder, "percentTreeCover_kernelDensity.jpeg"), height=im.width * aspect.r, width=im.width)
rasterVis::densityplot(allYrsRaster,
                       xlab='Percent Cover', 
                       ylab='Density', 
                       main='Kernel Density Plot for Percent Cover in the Puerco Project Area, 2000-2015',
                       draw.labels = FALSE,
                       lwd = 2)
                       #auto.key = list(space = c("right", "left", "left")))
dev.off()

#another way to show a kernel density plot:
#allYrs <- c(zm_c_2015, zm_c_2010, zm_c_2005, zm_c_2000)
#density(allYrs)
#density(zm_c_2010)
#density(zm_c_2005)
#density(zm_c_2000)


# HISTOGRAMS
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
p1 <- p1 + labs(x="Percent Cover", y="relative frequency",title ="Frequencey Histogram for Percent Cover Values, 2000-2015")
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

jpeg(file.path(imgFolder, "percentTreeCover_hist.jpeg"), height=im.width * aspect.r, width=im.width) 
grid.arrange(p1, p2, p3, p4, 
             ncol=2,
             top=textGrob("Percent Tree Cover for the Puerco Project Area", gp=gpar(fontsize=25)))
dev.off()

##### Autocorrelation test #####

#TODO: terra::autocor does not give p-values, which are needed to asses significance 
#of computed Moran's I. shoot.
#global moran's range from -1 (dispersed) to 1 (clustered)

q.ac.loc <- lapply(all, terra::autocor, w=queens, global=FALSE)
#I know this is ugly but can't seem to get terra::sapp to work
q.ac.loc <- c(q.ac.loc[[1]], q.ac.loc[[2]], q.ac.loc[[3]], q.ac.loc[[4]])
queens.ac.glob <- unlist(lapply(all, terra::autocor, w=queens, global=TRUE))

rooksAc <- lapply(all, terra::autocor, w=rooks, global=FALSE)
rooksAc <- c(rooksAc[[1]], rooksAc[[2]], rooksAc[[3]], rooksAc[[4]])
rooks.ac.glob <- unlist(lapply(all, terra::autocor, w=rooks, global=TRUE))



bishopsAc <- lapply(all, terra::autocor, w=bishops, global=FALSE)
bishopsAc <- c(bishopsAc[[1]], bishopsAc[[2]], bishopsAc[[3]], bishopsAc[[4]])
bishops.ac.glob <- unlist(lapply(all, terra::autocor, w=bishops, global=TRUE))

#comparison of Moran's I, Global
comp.i.glob <- data.frame(bishops.ac.glob, queens.ac.glob, rooks.ac.glob)
comp.i.glob
write.csv(comp.i.glob, file.path(imgFolder, "moransIglobal_compare.csv"), row.name=TRUE)


DivergePlot <- function(tc, caseType){
  p <- levelplot(tc,
                 margin=FALSE,
                 main = paste("Local Spatial Autocorrelation", caseType),
                 par.settings=list(panel.background=list(col="lightgrey"),
                                   strip.background=list(col="white")))
  p <- diverge0(p, 'RdBu')
  return(p)
}


jpeg("./GeneratedPlots/autocor_queens.jpeg", height = im.width * aspect.r, width = im.width)
DivergePlot(q.ac.loc, "Queen's Case")
dev.off()

jpeg("./GeneratedPlots/autocor_rooks.jpeg", height = im.width * aspect.r, width = im.width)
DivergePlot(rooksAc, "Rook's Case")
dev.off()

jpeg("./GeneratedPlots/autocor_bishops.jpeg", height = im.width * aspect.r, width = im.width)
DivergePlot(bishopsAc, "Bishop's Case")
dev.off()


