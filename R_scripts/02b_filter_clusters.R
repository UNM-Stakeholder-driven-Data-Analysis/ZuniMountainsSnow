#### read me ####
# generates optimal area by first filtering out clusters of tree cover below a certain size
#### libraries ####
library(tictoc)
library(terra)
library(cowplot)
library(ggplot2)
library(tidyterra)
source("./R_Scripts/00_Functions.R")

#### load data ####
zm_c_2000_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2000_sim.tif")
zm_c_2005_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2005_sim.tif")
zm_c_2010_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2010_sim.tif")
zm_c_2015_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2015_sim.tif")

simList <- list(zm_c_2000_sim, zm_c_2005_sim, zm_c_2010_sim, zm_c_2015_sim)
names(simList) <- c("yr2000", "yr2005", "yr2010", "yr2015")

THRESH = 2 #minimum size of cluster (queens case neighbors considered)
QUEENS = 8

#### save out each sim layer ####
# save out each layer of each year because sieve only applies to the first
#layer, even if you pass it a different layer! This is the workaround I found
maxLyr = nlyr(simList[[1]])
for (i in seq_along(simList)){
  saveFolder <- file.path("./R_output/poly2014-09-29/sim3x3", names(simList)[i])
  dir.create(saveFolder)
  simImg <- simList[[i]]
  for (j in 1:maxLyr){
    writeRaster(simImg[[j]], 
                file.path(saveFolder, paste(toString(j), ".tiff", sep="")), 
                overwrite=TRUE)
  }
  
}


#### filter out small clusters ####
# do this for each year, for each layer (saved as a seperate image)
imgFilt <- list()
maxLyr <- nlyr(simList[[1]])
for (yrName in names(simList)){
  lyrs <- list()
  for (j in 1:maxLyr){
    lyr <- rast(file.path("./R_output/poly2014-09-29/sim3x3", 
                          yrName, 
                          paste(toString(j), ".tiff", sep="")))
    lyrs[[j]] <- sieve(lyr, threshold=THRESH, directions=QUEENS)
  
  }
  img <- rast(lyrs) #merge all layers back into one image with layers
  #unkown why but sieve is converting NA's to the following value
  img <- subst(img,-2147483648, NA)
  imgFilt[[yrName]] <- img
}


#### save out filtered simImages ####
name = paste("thresh", toString(THRESH), "_neigh", QUEENS, sep="")
saveLoc = file.path("./R_output/poly2014-09-29/sim3x3", name)
saveLoc
dir.create(saveLoc)

for (yrName in names(imgFilt)){
  writeRaster(imgFilt[[yrName]], file.path(saveLoc, paste(yrName, ".tiff", sep="")))
}

#### TEMP: visual check this is working ####
#PlotSimImage(zm2000_sim_f, "CATS", cropRect)
cropExt <- ZoomExt(widthMult=4, xCellStart=42, yCellStart=59, img=zm2000_sim_f)
cropRect <- as.polygons(cropExt, crs=crs(zm2000_sim_f))
p1 <- PlotSimImage(crop(zm_c_2015_sim[[1]], cropExt), "zoom orign")
p2 <- PlotSimImage(crop(imgFilt$yr2015[[1]], cropExt), "zoom sieved")
grid.arrange(p1, p2, ncol = 2, top="original vs sieved")
