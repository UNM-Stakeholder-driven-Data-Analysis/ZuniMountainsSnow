#### READ ME ####
# the purpose of this script is to create optimal images from the output of 02_simulate_tree_distributions
# the analysis involves creating an image of optimal area for each simulation layer and for each year
# NOTE: this will take about one hour to run

#### packages ####
library(tictoc)
library(terra)
library(cowplot)
library(ggplot2)
library(tidyterra)
source("./R_Scripts/00_Functions.R")

NUMCELLS <- 2 #number of covered cells required to exist in area of consideration
#for the cell to be considered optimal
AREA <- "lineNeighbors1"
POLY="poly2014-09-29"

NAMES <- c("yr2000", "yr2005", "yr2010", "yr2015")


#zuni mountain cover, simulation
zm_c_2000_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2000_sim.tif")
zm_c_2005_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2005_sim.tif")
zm_c_2010_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2010_sim.tif")
zm_c_2015_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2015_sim.tif")

# create folder to save output
resol <- toString(res(zm_c_2000_sim)[1])
runName = paste("n", NUMCELLS, "_", resol, "x", resol,  "_", AREA, sep="")
#TODO: would be good to save metadata about the optimal criterion instead of encoding in folder name
folder <- file.path("./R_output", POLY, runName)
dir.create(folder)

#### Initialize Constants for Focal Function ####

#circleMat = GetCircleMat(zm_c_2015_sim)
circleMat <- NorthLines(zm_c_2015_sim, radius=15, n=1)
circleMat
nRow = nrow(circleMat)
weightMat = matrix(data=1, nrow=nRow, ncol=nRow)
#for an odd sized square matrix represented at a vector, going from top left to right, then down by column,
#the central cell is equivalent to:
CENTER = nRow * floor(nRow/2) + ceiling(nRow/2) #11 columns, go to the 6th row, go in by 6 columns to get

#### create focal function ####

OptImage <- function(img, ...){
  # to be applied to a single layer
  res <- focal(img, 
        w=weightMat, 
        fun=function(x, na.rm) IsOptimal(x, na.rm, CENTER, circleMat, numCells=NUMCELLS), 
        na.policy="omit", 
        silent=FALSE, 
        na.rm=TRUE)
  return(res)
  
}

#### test and plot one layer ####
# TEST ON ONE SIM LAYER
oi <- OptImage(zm_c_2015_sim[[1]])
#TODO: consider converting to factor within PlotOptImage
oi <- as.factor(oi)
#levels(oi) <- OptLevels()

# PLOT ENTIRE IMG W/ zoom area
cropExt <- ZoomExt(widthMult=4, xCellStart=32, yCellStart=30, img=zm_c_2015_sim)
cropRect <- as.polygons(cropExt, crs=crs(oi))
p1 <- PlotOptImage(oi, paste("Opt img, 3x3m subcell, 2015,", "n=", toString(NUMCELLS), AREA))
p1 <- p1 + geom_spatvector(data=cropRect, fill=NA, colour="black")
plot(p1)
#YESSSS! this saves it a native res
ggsave(file.path(folder, paste("2015", "n", toString(NUMCELLS),",", AREA,".tiff",sep="")), dpi=600)

# PLOT ZOOM AREA
zoomedOi <- crop(oi, cropExt)
PlotRastAsMat(zoomedOi)
p2 <- PlotOptImage(crop(oi, cropExt), "zoom")
plot(p2)

#### apply focal func to all spatrasters, all layers ####

OptImages <- function(imgStack){
  # ^ to be applied to a list of spatrasters each with layers
  
  #this line applies OptImage to all the layers in imgStack
  return(sapp(imgStack, fun=OptImage))
}

all <- list(zm_c_2000_sim, zm_c_2005_sim, zm_c_2010_sim, zm_c_2015_sim)

#this will generally take around 1 hr
tic("Getting Optimal Images")
resultAll <- lapply(all, FUN=OptImages)
toc()

#organize reults into a spatRasterDataset for convenient access
optImgSDS <- sds(resultAll[[1]], resultAll[[2]], resultAll[[3]], resultAll[[4]])
names(optImgSDS) <-  NAMES

#### save optimal imgs ####

#TODO: lapply or something?
writeRaster(optImgSDS$yr2000, filename=file.path(folder, "zm2000_opt.tif"), overwrite=FALSE)
writeRaster(optImgSDS$yr2005, filename=file.path(folder, "zm2005_opt.tif"), overwrite=FALSE)
writeRaster(optImgSDS$yr2010, filename=file.path(folder, "zm2010_opt.tif"), overwrite=FALSE)
writeRaster(optImgSDS$yr2015, filename=file.path(folder, "zm2015_opt.tif"), overwrite=FALSE)

#### visual check of optimal images, one layer ####
#plot(optImgSDS$yr2015[[1]])
#layr <- 1
#optLayerSamp <- c(optImgSDS$yr2000[[layr]], 
#                   optImgSDS$yr2005[[layr]], 
#                   optImgSDS$yr2010[[layr]],
#                   optImgSDS$yr2015[[layr]])
#names(optLayerSamp) <- NAMES
#plot(optLayerSamp)
#ggplot() +
#  geom_spatraster(data=optLayerSamp) +
#  facet_wrap(~lyr) + 
#  labs(title="optimal image for one layer accross all years")
