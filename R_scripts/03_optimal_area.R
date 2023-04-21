#### READ ME ####
# the purpose of this script is to process the output of 02_simulate_tree_distributions
# the analysis involves creating an image of optimal area for each simulation layer and for each year
# then collecting the total optimal area per layer, per year
# then exploring the distribution of optimal area for each year, over the layers

#### packages ####
library(tictoc)
library(terra)
library(cowplot)
library(ggplot2)
library(tidyterra)
source("./R_Scripts/00_Functions.R")
NAMES <- c("yr2000", "yr2005", "yr2010", "yr2015")
#zuni mountain cover, simulation
#zm_c_2000_sim <- rast("./R_output/poly2014-09-29/zm2000_sim.tif")
#zm_c_2005_sim <- rast("./R_output/poly2014-09-29/zm2005_sim.tif")
#zm_c_2010_sim <- rast("./R_output/poly2014-09-29/zm2010_sim.tif")

#FOR NOW RUNNING FOR ONE SIM
zm_c_2015_sim <- rast("./R_output/poly2014-09-29/6x6/zm2015_sim_sinlge.tif")

#visual example of two simulations for one year
#ggplot() + 
#  geom_spatraster(data=zm_c_2000_sim[[1:2]]) + 
#  facet_wrap(~lyr) + 
#  labs(title="Two Simulated Tree Distributions based on Tree Cover and 3x3m canopy \n for the year 2000")

  

#### Visual check that years differ ####
#lyer = "2"
#firstSim = c(zm_c_2000_sim[[lyer]], 
#             zm_c_2005_sim[[lyer]], 
#             zm_c_2010_sim[[lyer]],
#             zm_c_2015_sim[[lyer]])
#set.names(firstSim, c("yr2000", "yr2005", "yr2010", "yr2015"))
#ggplot() +
#  geom_spatraster(data=firstSim) +
#  facet_wrap(~lyr) +
#  labs(title="One simulation per year")

#### Initialize Constants for Focal Function ####
#NOTE: might be cleaner code to use buffer! (buffer, then remove tree cover from buffer)
#zm_c_2000_sim[zm_c_2000_sim==0] = NA


circleMat = GetCircleMat(zm_c_2015_sim)
circleMat[,1:2] = 0 #ignroe southside, unkown why this corresponds to the side instead of the bottom
circleMat
nRow = nrow(circleMat)
weightMat = matrix(data=1, nrow=nRow, ncol=nRow)
#for a odd sized square matrix represented at a vector, going from top left to right, then down by column,
#the central cell is equivalent to:
CENTER = nRow * floor(nRow/2) + ceiling(nRow/2) #11 columns, go to the 6th row, go in by 6 columns to get

#### apply focal to all layers ####
#result <- focal(zm_c_2015_sim[[1:2]], 
#                w=weightMat, 
#                fun=function(x, na.rm) IsOptimal(x, na.rm, CENTER, circleMat), 
#                na.policy="omit", 
#                silent=FALSE, 
#                na.rm=TRUE)

OptImage <- function(img, ...){
  # to be applied to a single layer
  res <- focal(img, 
        w=weightMat, 
        fun=function(x, na.rm) IsOptimal(x, na.rm, CENTER, circleMat), 
        na.policy="omit", 
        silent=FALSE, 
        na.rm=TRUE)
  return(res)
  
}

oi <- OptImage(zm_c_2015_sim)
oi <- as.factor(oi)
levels(oi) <- OptLevels()




OptImages <- function(imgStack){
  # to be applied to a list of spatrasters each with layers
  
  #this lineapplies OptImage to all the layers in imgStack
  return(sapp(imgStack, fun=OptImage))
}

all <- list(zm_c_2000_sim, zm_c_2005_sim, zm_c_2010_sim, zm_c_2015_sim)

#### get a visual on a smaller portion ####
simImg <- zm_c_2015_sim[[1]]
plot(simImg)
yStart = 3898600
cropExt <- ext(758000, 758000+250, yStart, yStart+250)
out <- crop(simImg, cropExt)
optCrop <- OptImage(out)
plot(out)
plot(optCrop)

#this will generally take around 1 hr
tic("Getting Optimal Images")
resultAll <- lapply(all, FUN=OptImages)
toc()

#organize reults into a spatRasterDataset for convenient access
optImgSDS <- sds(resultAll[[1]], resultAll[[2]], resultAll[[3]], resultAll[[4]])
names(optImgSDS) <-  c("yr2000", "yr2005", "yr2010", "yr2015")

#### save optimal imgs ####
#since it takes so long better to cache it.
writeRaster(optImgSDS$yr2000, filename="./R_output/poly2014-09-29/zm2000_opt.tif", overwrite=FALSE)
writeRaster(optImgSDS$yr2005, filename="./R_output/poly2014-09-29/zm2005_opt.tif", overwrite=FALSE)
writeRaster(optImgSDS$yr2010, filename="./R_output/poly2014-09-29/zm2010_opt.tif", overwrite=FALSE)
writeRaster(optImgSDS$yr2015, filename="./R_output/poly2014-09-29/zm2015_opt.tif", overwrite=FALSE)

# do a global sum for all layers (0,1 image, so results in count of 1 cells)
# multiply by 9sq meters to get total area

#### visual check of optimal images, one layer ####
plot(optImgSDS$yr2015[[1]])
layr <- 1
optLayerSamp <- c(optImgSDS$yr2000[[layr]], 
                   optImgSDS$yr2005[[layr]], 
                   optImgSDS$yr2010[[layr]],
                   optImgSDS$yr2015[[layr]])
names(optLayerSamp) <- NAMES
plot(optLayerSamp)
ggplot() +
  geom_spatraster(data=optLayerSamp) +
  facet_wrap(~lyr) + 
  labs(title="optimal image for one layer accross all years")

# <- ggplot() + geom_spatraster(data=zm_c_2015_sim[["1"]])
#p2 <- ggplot() + geom_spatraster(data=result)
#plot_grid(p1, p2, labels = c('Tree Cover', 'Is Optimal'), label_size = 12)

