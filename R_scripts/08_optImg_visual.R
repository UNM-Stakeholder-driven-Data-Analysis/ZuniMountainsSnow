#### libraries ####
library(ggplot2)
library(tidyterra)
library(terra)
library(tidyr)
library(dplyr)
library(ggsn)
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

#### setup zoom area ####
zoomSize=4
xStart=42 
yStart=59
anyImg <- optVariants[[1]][[1]] #get any image for crs and resolution
cropExt <- ZoomExt(widthMult=zoomSize, xCellStart=xStart, yCellStart=yStart, img=anyImg)
cropRect <- as.polygons(cropExt, crs=crs(anyImg))

#### visualize whole area ####
yrs <- c("yr2015", "yr2010", "yr2005", "yr2000")
simLyr <- 2
zoomYr <- "yr2000" #this one happens to have a great clearing for seeing a lone tree


for (yr in yrs){
  optAllCriteria <- rast(lapply(optVariants, function(x) x[[yr]][[simLyr]]))
  p <- PlotOptImageLyr(optAllCriteria, paste("Optimal Images for ", yr, "\n simulation #", toString(simLyr), sep=""))
  p <- p + 
    scale_x_continuous(breaks=c(-108.165, -108.155)) + 
    scalebar(x.min=xmin(optAllCriteria)
             ,x.max=xmax(optAllCriteria)
             ,y.min=ymin(optAllCriteria)
             ,y.max=ymax(optAllCriteria)
             ,dist = 0.5
             ,dist_unit = "km"
             ,transform = FALSE 
             ,model = "WGS84"
             ,border.size=0.5
             ,st.size=2
             ,box.fill="white"
             ,box.color="grey"
             ,st.color="grey"
    ) +
    labs(y="", x="")
  if (yr==zoomYr){
    p <- p + geom_spatvector(data=cropRect, fill=NA, colour="black")
  }
  #to save at native resolution use .tiff and dpi=600
  ggsave(file.path("./GeneratedPlots", paste("optArea_", yr, ".jpeg", sep="")), width=WIDTH, height=HEIGHT, units="in")
}

#### visualized zoomed area ####

tiles <- ZoomParts(zoomSize, xStart, yStart, optAllCriteria)
optAll2000 <- rast(lapply(optVariants, function(x) x[[zoomYr]][[simLyr]]))
optAllCrop2000 <- crop(optAll2015, cropRect)

p <- PlotOptImageLyr(optAllCrop2000, paste("Optimality Criteria Results, ", zoomYr, "\nsimulation #", toString(simLyr), sep=""))
p <- p + 
  #avoids overlapping labels
  geom_spatvector(data=tiles, fill=NA, colour="grey", linewidth=0.2)+
  scale_x_continuous(breaks=c(-108.1565, -108.156, -108.155))
ggsave("./GeneratedPlots/optAreaZoom_2015.jpeg", width=WIDTH, height=HEIGHT, units="in")
