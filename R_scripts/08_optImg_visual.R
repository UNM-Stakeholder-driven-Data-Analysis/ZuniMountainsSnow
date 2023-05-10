#### libraries ####
library(ggplot2)
library(tidyterra)
library(terra)
library(tidyr)
library(dplyr)
source("./R_Scripts/00_Functions.R")

#### load data ####
folders <- c("n1_3x3_north" 
  ,"n2_3x3_north"
  ,"n3_3x3_north"
  ,"n4_3x3_north"
  ,"n5_3x3_north"
  ,"c1_3x3_northline"
  ,"c2_3x3_northline")
names <- c("North Semicircle, 1"
           ,"North Semicircle, 2"
           ,"North Semicircle, 3"
           ,"North Semicircle, 4"
           ,"North Semicircle, 5"
           ,"North Line, 1"
           ,"North Line, 2")
optVariants <- lapply(folders, function(folder) LoadOptList(folder, parentFolder=POLYNAME))
names(optVariants) <- names

#### visualize whole area ####
optAll2015 <- rast(lapply(optVariants, function(x) x$yr2015[[1]]))
PlotOptImageLyr(optAll2015, "Optimal Images for 2015, one simulation")
ggsave("./GeneratedPlots/optArea_2015.jpeg")

#### visualized zoomed area ####
zoomSize=4
xStart=42 
yStart=59
cropExt <- ZoomExt(widthMult=zoomSize, xCellStart=xStart, yCellStart=yStart, img=optAll2015)
cropRect <- as.polygons(cropExt, crs=crs(optAll2015))
tiles <- ZoomParts(zoomSize, xStart, yStart, optAll2015)

optAllCrop2015 <- crop(optAll2015, cropRect)

p <- PlotOptImageLyr(optAllCrop2015, "Optimality Criteria Results, 2015")
p <- p + geom_spatvector(data=tiles, fill=NA, colour="grey", linewidth=0.2)
plot(p)
ggsave("./GeneratedPlots/optAreaZoom_2015.jpeg")
