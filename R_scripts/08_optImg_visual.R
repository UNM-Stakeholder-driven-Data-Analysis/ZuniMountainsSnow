#### libraries ####
library(ggplot2)
library(tidyterra)
library(terra)
library(tidyr)
library(dplyr)
source("./R_Scripts/00_Functions.R")

WIDTH = 9
HEIGHT = 8 #in

#### load data ####
folders <- c("n1_3x3_north" 
  ,"n2_3x3_north"
  ,"n3_3x3_north"
  ,"n4_3x3_north"
  ,"n5_3x3_north"
  ,"c1_3x3_northline"
  ,"c2_3x3_northline")
names <- c("A) North Semicircle, 1"
           ,"B) North Semicircle, 2"
           ,"C) North Semicircle, 3"
           ,"D) North Semicircle, 4"
           ,"E) North Semicircle, 5"
           ,"F) North Line, 1"
           ,"G) North Line, 2")
optVariants <- lapply(folders, function(folder) LoadOptList(folder, parentFolder=POLYNAME))
names(optVariants) <- names

#### visualize whole area ####
yrs <- c("yr2015", "yr2010", "yr2005", "yr2000")
simLyr <- 2


for (yr in yrs){
  optAll2015 <- rast(lapply(optVariants, function(x) x[[yr]][[simLyr]]))
  p <- PlotOptImageLyr(optAll2015, paste("Optimal Images for ", yr, " ,one simulation", sep=""))
  p <- p + scale_x_continuous(breaks=c(-108.165, -108.155)) #avoids overlapping labels
  ggsave(file.path("./GeneratedPlots", paste("optArea_", yr, ".jpeg", sep="")), width=WIDTH, height=HEIGHT, units="in")
}

#### visualized zoomed area ####
zoomSize=4
xStart=42 
yStart=59
cropExt <- ZoomExt(widthMult=zoomSize, xCellStart=xStart, yCellStart=yStart, img=optAll2015)
cropRect <- as.polygons(cropExt, crs=crs(optAll2015))
tiles <- ZoomParts(zoomSize, xStart, yStart, optAll2015)

optAllCrop2015 <- crop(optAll2015, cropRect)

p <- PlotOptImageLyr(optAllCrop2015, "Optimality Criteria Results, 2015")
p <- p + 
  #avoids overlapping labels
  geom_spatvector(data=tiles, fill=NA, colour="grey", linewidth=0.2)+
  scale_x_continuous(breaks=c(-108.1565, -108.156, -108.155))

ggsave("./GeneratedPlots/optAreaZoom_2015.jpeg", width=WIDTH, height=HEIGHT, units="in")
