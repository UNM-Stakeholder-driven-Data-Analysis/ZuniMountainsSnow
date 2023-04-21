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
source("./R_Scripts/00_Functions.R")
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
#NO_ALTER_TC = c("Silvicultural Stand Examination", 
#                "Stand Diagnosis Prepared", 
#                "TSI Need Created- Precommercial Thin",
#                "TSI Need (precommercial thinning) Eliminated",
#                "Yarding - Removal of Fuels by Carrying or Dragging",
#                "Underburn - Low Intensity (Majority of Unit)",
#                "Piling of Fuels, Hand or Machine ",
#                "Range Cover Manipulation",
#                "Rearrangement of Fuels",
#                "Stand Silviculture Prescription",
#                "Burning of Piled Material")
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
p2007 <- "2007-08-30 18:00:00" # appears to be nothing
#all the 2009s should probably be merged!
p2009_06 <- "2009-06-11 18:00:00"
p2009_09 <- "2009-09-30 18:00:00" # tiny area
p2009_10 <- "2009-10-30 18:00:00"

dateCompleted <- p2014

one_polyVec <- terra::vect(filter(haz_fr_merged, Group.1==dateCompleted))
poly_ext <- ext(one_polyVec)
one_poly <- terra::rasterize(one_polyVec, tc2015)
one_poly <- terra::crop(one_poly, poly_ext)
plot(one_poly)

writeVector(one_polyVec, "polyThin2014-09-29.kml")


#### process Data ####

errAll_p <- errAll %>% terra::crop(one_poly) %>% terra::mask(one_poly)
ggplot() + geom_spatraster(data=errAll_p) + facet_wrap(~lyr)

all <- tcAll %>% 
  terra::crop(one_poly) %>% 
  terra::mask(one_poly) %>%
  OnlyTreeCover()

# OnlyTreeCover NaNs any values not in 0-100
stopifnot(all(terra::minmax(all)[2,] <= 100))



  
# get max percent value for all
minMaxAll <- minmax(all)
maxPercent <- max(minMaxAll[2,])


#### Visualize ####


# PERCENT COVER PLOTS
jpeg(file.path(imgFolder, "percentTreeCover.jpeg"), height = 1024 * aspect.r, width = 1024)

projName <- haz_fr_merged$NEPA_DOC_NAME[haz_fr_merged$Group.1==dateCompleted][[1]]
actType <- haz_fr_merged$ACTIVITY[haz_fr_merged$Group.1==dateCompleted][[1]]
ggplot() + 
  geom_spatraster(data=all) + 
  facet_wrap(~lyr) +
  labs(title=paste("Tree Cover for activity completed on", toString(dateCompleted), 
                   "\nActivity: ", tolower(actType),
                   "\n", tolower(projName)))
dev.off()

# DIFFERENCE PLOTS
diffs <- c(all$yr2015 - all$yr2010, all$yr2010-all$yr2005, all$yr2005-all$yr2000)
names(diffs) <- c("yr2015vs2010", "yr2010vs2005", "yr2005vs2000")

jpeg(file.path(imgFolder, "diffPercentTreeCover.jpeg"), height = 1024 * aspect.r, width = 1024)
ggplot() +
  geom_spatraster(data=diffs) +
  facet_wrap(~lyr) +
  scale_fill_distiller(palette="RdBu") +
  labs(title=paste("Differene in Tree Cover for activity completed on", toString(dateCompleted), 
                   "\nActivity: ", tolower(actType),
                   "\n", tolower(projName)))
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
freqAll <- terra::freq(all, usenames=TRUE) %>% 
  group_by(layer) %>% 
  mutate(total=sum(count)) %>%
  ungroup %>%
  mutate(relFreq=count/total)

#there must be a better way of doing this.
yr2015 <- c(all$yr2015, errAll_p$yr2015)
yr2010 <- c(all$yr2010, errAll_p$yr2010)
yr2005 <- c(all$yr2010, errAll_p$yr2005)
yr2000 <- c(all$yr2000, errAll_p$yr2000)
set.names(yr2015, c("cover", "err"))
set.names(yr2010, c("cover", "err"))
set.names(yr2005, c("cover", "err"))
set.names(yr2000, c("cover", "err"))

crossTabResults <- bind_rows(CrossTabSummary(yr2015, lyrName="yr2015"), 
               CrossTabSummary(yr2010, lyrName="yr2010"),
               CrossTabSummary(yr2005, lyrName="yr2005"),
               CrossTabSummary(yr2000, lyrName="yr2000")
               )
#TODO get this working again!
freqAll <- left_join(freqAll, crossTabResults, by=join_by(layer, value))


#TODO: add sumError column (use cross tab to do this!)
#maybe want to use sds to combine err_p and all?

# plot hisograms together on one axes
p1 <- ggplot(data=freqAll, aes(factor(value), relFreq, fill=factor(layer))) + geom_col( position=position_dodge2(10))
p1 <- p1 + labs(x="Percent Cover", y="relative frequency",title ="Frequencey Histogram for Percent Cover Values, 2000-2015")
print(p1)

# Plot each histogram in a separate facet
jpeg(file.path(imgFolder, "percentTreeCover_hist.jpeg"), height=im.width * aspect.r, width=im.width) 
b <- 0:100
b[b%%5 > 0 ] = NA #turn anything not a multiple of 5 into an NA
labels <- ifelse(!is.na(b), b, "") #convert NA's into empty strings
ggplot(data=freqAll, aes(factor(value), relFreq, fill=errAvg)) +
  geom_col() +
  facet_wrap(~layer) + 
  scale_fill_gradient(name = "average \nerror",
                      limits = c(0, 19), 
                       breaks = c(0, 5, 10, 15, 19),
                        labels = c(0, 5, 10, 15, 19)) +
  scale_x_discrete(breaks = seq(0, 100, by = 1),
                   labels = labels) +
  labs(x="Percent Cover", y="relative frequency",title =paste("Cover Histogram for Polygon ", dateCompleted))
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


