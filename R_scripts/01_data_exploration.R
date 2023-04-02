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
library(tictoc)
library(tidyterra)
library(plotly)
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
MonteCarloImg <- function(img){
  #make an empty image with same extent, crs, and higher resolution
  # by randomly generating a plausable 10x10 grid for each single cell in img
  #ASSUMPTIONS:
  # img has resolution of 30x30
  # img values range from 0,100
  
  #copy the input image but at a higher resolution
  samp <- terra::rast(ext(img), resolution=c(3,3))
  crs(samp) <- crs(img)
  
  iterI <- nrow(img)
  iterJ <- ncol(img)
  
  rRng = c(low=1, high=10)
  cRng = c(low=1, high=10)
  for (i in 1:iterI){
    for (j in 1:iterJ){
      value = img[i, j][[1]]
      if (is.na(value)){
        samp[rRng["low"]:rRng["high"], cRng["low"]:cRng["high"]] = NA
      }
      else{
        #print(value)
        samp[rRng["low"]:rRng["high"], cRng["low"]:cRng["high"]] = MonteCarloMat(value)
      }
      cRng <- cRng + 10
    }
    cRng <- c(low=1, high=10)
    rRng <- rRng + 10
  }
  return(samp)
}
MonteCarloMat <- function(cover){
  #generate a possible distribution of 100% cover sub-cells so that the % cover
  #of the entire 10x10 matrix is equal to cover
  #INPUT:
  # cover has range 0-100 integer
  #OUPUT:
  # 10x10 matrix where each value is 0 or 100
  
  maxCover <- 100 #maximum value that "cover" can take
  maxVal <-100 #value to represent 100% cover in the new matrix
  if (cover==0){
    return(as.data.frame(replicate(100, 0))) #spatRaster wants a data frame
  }
  else{
    vec <- c(replicate(cover, maxVal), replicate(maxCover-cover, 0))
    return(as.data.frame(sample(vec, size=100)))
  }
}

Identifier <- function(year, path, row, suffix=""){
  return(paste("p", path, "r", row, "_TC_", toString(year), suffix, sep=""))
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

YearFile <- function(year, path, row, type="", suffix=".tif"){
  # type can be "", "_err" or "_idx"
  identifier <- Identifier(year, path, row, type)
  file_path <- file.path(YearFileIntermediate(year, path, row, suffix=""),
                         paste(identifier, suffix, sep=""))
  return(file_path)
}

MergedRaster <- function(year, type=""){

  p035r035 <- rast(YearFile(year, "035", "035", type))
  p035r036 <- rast(YearFile(year, "035", "036", type))
  
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


ClassifiedImage <- function(cover){
  #create an image where optimal pixels have value 100, no optimal have value 0
  m <- c(0, 10, 0,
         11, 100, 100)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  terra::classify(cover, rclmat, right=NA)
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

#TODO: remove below line
puerco_area_spat <- terra::vect("./data/Puerco Project Area/puerco_Project-polygon.shp")

yr2015 <- GetRangeDateTime(2015)
yr2010 <- GetRangeDateTime(2010)
yr2005 <- GetRangeDateTime(2005)
yr2000 <- GetRangeDateTime(2000)

dtRanges <- tibble(yr2000, yr2005, yr2010, yr2015)
START_DATE = dtRanges$yr2000["start"]
END_DATE = dtRanges$yr2015["end"]

tc2015 <- MergedRaster(2015)
tc2010 <- MergedRaster(2010)
tc2005 <- MergedRaster(2005)
tc2000 <- MergedRaster(2000)
#2015 and 2010 happen to have larger extents than 2005 and 2000
#making them the same allows us to combine as layers
tc2015 <- terra::crop(tc2015, tc2005)
tc2010 <- terra::crop(tc2010, tc2005)
#For some reason tc2010 has a different coord. ref designation that omits
# the datum, although the UTM zone is the same.
tc2010 <- terra::project(tc2010, tc2015)

tcAll <- c(tc2015, tc2010, tc2005, tc2000)
  
err2015 <- MergedRaster(2015, type="_err")
err2010 <- MergedRaster(2010, type="_err")
err2005 <- MergedRaster(2005, type="_err")
err2000 <- MergedRaster(2000, type="_err")
#2015 and 2010 happen to have larger extents than 2005 and 2000
#making them the same allows us to combine as layers
err2015 <- terra::crop(err2015, err2005)
err2010 <- terra::crop(err2010, err2005)
#For some reason err2010 has a different coord. ref designation that omits
# the datum, although the UTM zone is the same.
err2010 <- terra::project(err2010, err2015)

errAll <- c(err2015, err2010, err2005, err2000)





##### Isolate Thinning Polygons in Zuni Mountains ####
CIBOLA_FOREST = "03" #this is inferred by inspecting the map at the link:
#https://usfs.maps.arcgis.com/apps/mapviewer/index.html?layers=eb8f23442f374ea2adae683e6eb0f16a


forests <- sf::st_read("./data/ProclaimedForest.kml")
zuni_forest <- forests[forests$Name=="Zuni Mountains", 1]
zuni_forest <- sf::st_transform(zuni_forest, crs=st_crs(tc2015))

act_raw <- sf::st_read("./data/ActivityPolygon/Activities.gdb")
#TODO: rename these categories to other and still plot them
NO_ALTER_TC = c("Silvicultural Stand Examination", 
                "Stand Diagnosis Prepared", 
                "TSI Need Created- Precommercial Thin",
                "TSI Need (precommercial thinning) Eliminated",
                "Yarding - Removal of Fuels by Carrying or Dragging",
                "Underburn - Low Intensity (Majority of Unit)",
                "Piling of Fuels, Hand or Machine ",
                "Range Cover Manipulation",
                "Rearrangement of Fuels",
                "Stand Silviculture Prescription",
                "Burning of Piled Material")
all_cibola <- act_raw %>% 
  filter(AU_FOREST_CODE==CIBOLA_FOREST) %>% #filter immediately to increase processing speed
  filter(between(DATE_COMPLETED, START_DATE, END_DATE)) %>%
  filter(!ACTIVITY %in% NO_ALTER_TC) %>%
  sf::st_transform(crs=st_crs(tc2015))

intersection <- sf::st_intersects(zuni_forest, all_cibola)[[1]]
all_zuni <- all_cibola[intersection, ]

haz_fr <- all_zuni %>% 
  filter(ACTIVITY=="Thinning for Hazardous Fuels Reduction") %>%
  filter(DateNotIn(DATE_COMPLETED, dtRanges))

all_other <- filter(all_zuni, ACTIVITY!="Thinning for Hazardous Fuels Reduction")
haz_fr_iso <- st_difference(haz_fr, st_union(all_other))

p <- ggplot() +
  #geom_sf(data=zuni_forest) +
  geom_sf(data=all_zuni, aes(fill=ACTIVITY), alpha=0.70) +
  scale_fill_brewer(palette="Set3")

ggplotly(p) #interactive plot

ggplot() +
  geom_sf(data=haz_fr, fill="green") +
  geom_sf(data=haz_fr_iso, aes(fill=factor(DATE_COMPLETED))) +
  scale_fill_brewer()


#number of unique thinning events (not necessarily one polygon) that occured:
length(unique(haz_fr_iso$DATE_COMPLETED))

haz_fr_merged <- aggregate(haz_fr_iso, 
               by = list(haz_fr_iso$DATE_COMPLETED),
               FUN=function(x) x) #dummy function

ggplot() +
  geom_sf(data=haz_fr_merged, aes(fill=factor(Group.1)))

#### crop tc raster by isolated polygon ####
p2014 <- "2014-09-29 18:00:00"
p2012 <- "2012-09-29 18:00:00" # doesn't have as obvious of reduction in TC
p2003 <- "2003-05-04 18:00:00" # 

one_poly <- terra::vect(filter(haz_fr_merged, Group.1==p2014))
poly_ext <- ext(one_poly)
one_poly <- terra::rasterize(one_poly, tc2015)
one_poly <- terra::crop(one_poly, poly_ext)
plot(one_poly)

#### process Data ####

errAll_p <- errAll %>% terra::crop(one_poly) %>% terra::mask(one_poly)
ggplot() + geom_spatraster(data=errAll_p) + facet_wrap(~lyr)

all <- tcAll %>% 
  terra::crop(one_poly) %>% 
  terra::mask(one_poly) %>%
  OnlyTreeCover()

# OnlyTreeCover NaNs any values not in 0-100
stopifnot(all(terra::minmax(all)[2,] <= 100))

ggplot() + geom_spatraster(data=all) + facet_wrap(~lyr)
  
# get max percent value for all
minMaxAll <- minmax(all)
maxPercent <- max(minMaxAll[2,])

#### generate a monte carlo sample ####
for (i in 1:10){
tic("MonteCarloImg")
zm_c_2000_samp <- MonteCarloImg(all$yr2000)
toc()
}

plot(zm_c_2000_samp)
ggplot() +
  geom_spatraster(data = all$yr2000)

ggplot() +
  geom_spatraster(data = zm_c_2000_samp)


ci <- ClassifiedImage(all)
wm <- matrix(1, nc=3, nr=3)
Optimal <- function(y, na.rm, ...){
  print("A")
  print(y)
  print("---")
  CENTER = 5
  if(!is.na(y[CENTER]) & y[CENTER] == 0){
    #cell values are either 0 or 100
    return(ifelse(sum(y[c(1:4, 6:9)], na.rm=na.rm) >= 800, 100, 0))
  }
  else {
    return(0)
  }
}
#something breaks when the whole stack is applied. 
opt_img <- focal(ci["yr2005"], w=wm, fun=Optimal, na.policy="omit", na.rm=TRUE, silent=FALSE)
#trying to get it to work on layers
# https://stackoverflow.com/questions/68615434/using-sapp-to-apply-terrafocal-to-each-layer-of-a-spatraster
opt_img <- sapp(ci, fun = function(z, ...) {focal(z, fun=Optimal, na.policy="omit", w=wm)})
s <- sapp(ci, fun = function(x, ...) {focal(x, fun = "any", w = 3)})

m <- matrix(1:16, nrow=4, ncol=4)
x <- rast(m)

#### Visualize ####

#Classified Image
plot(ci)
plot(opt_img)


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
allYrsRaster <- raster::stack(raster(all$yr2015), raster(all$yr2010), raster(all$yr2005), raster(all$yr2000))
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

#freq_2000 <- RelFreq(zm_c_2000) %>% mutate(year=2000)
#freq_2005 <- RelFreq(zm_c_2005) %>% mutate(year=2005)
#freq_2010 <- RelFreq(zm_c_2010) %>% mutate(year=2010)
#freq_2015 <- RelFreq(zm_c_2015) %>% mutate(year=2015)
#freqAll <- bind_rows(freq_2000, freq_2005, freq_2010, freq_2015)
freqAll <- terra::freq(all, usenames=TRUE) %>% 
  group_by(layer) %>% 
  mutate(total=sum(count)) %>%
  ungroup %>%
  mutate(relFreq=count/total)

# plot hisograms together one one axes
p1 <- ggplot(data=freqAll, aes(factor(value), relFreq, fill=factor(layer))) + geom_col( position=position_dodge2(10))
p1 <- p1 + labs(x="Percent Cover", y="relative frequency",title ="Frequencey Histogram for Percent Cover Values, 2000-2015")
print(p1)

# Plot each histogram in a separate facet
jpeg(file.path(imgFolder, "percentTreeCover_hist.jpeg"), height=im.width * aspect.r, width=im.width) 
ggplot(data=freqAll, aes(factor(value), relFreq)) +
  geom_col() +
  facet_wrap(~layer) + 
  labs(x="Percent Cover", y="relative frequency",title ="MEOW")
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


