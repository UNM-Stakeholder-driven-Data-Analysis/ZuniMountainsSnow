#### READ ME ####
# the purpose of this script is to process the output of 02_simulate_tree_distributions
# the analysis involves creating an image of optimal area for each simulation layer and for each year
# then collecting the total optimal area per layer, per year
# then exploring the distribution of optimal area for each year, over the layers

#### packages ####
library(terra)
library(cowplot)
library(ggplot2)
library(tidyterra)
source("./R_Scripts/00_Functions.R")

#zuni mountain cover, simulation
zm_c_2000_sim <- rast("./R_output/poly2014-09-29/zm2000_sim.tif")
zm_c_2005_sim <- rast("./R_output/poly2014-09-29/zm2005_sim.tif")
zm_c_2010_sim <- rast("./R_output/poly2014-09-29/zm2010_sim.tif")

#### Visual check that years differ ####
lyer = "2"
firstSim = c(zm_c_2000_sim[[lyer]], zm_c_2005_sim[[lyer]], zm_c_2010_sim[[lyer]])
set.names(firstSim, c("yr2000", "yr2005", "yr2010"))
ggplot() +
  geom_spatraster(data=firstSim) +
  facet_wrap(~lyr)

#### Initialize Constants for Focal Function ####
#NOTE: might be cleaner code to use buffer! (buffer, then remove tree cover from buffer)
#zm_c_2000_sim[zm_c_2000_sim==0] = NA


circleMat = GetCircleMat(zm_c_2000_sim[["1"]])
nRow = nrow(circleMat)
weightMat = matrix(data=1, nrow=nRow, ncol=nRow)
#for a odd sized square matrix represented at a vector, going from top left to right, then down by column,
#the central cell is equivalent to:
CENTER = nRow * floor(nRow/2) + ceiling(nRow/2) #11 columns, go to the 6th row, go in by 6 columns to get

#### apply focal to all layers ####
#TODO get this working for all layers
result <- focal(zm_c_2000_sim[["1"]], 
                w=weightMat, 
                fun=function(x, na.rm) IsOptimal(x, na.rm, CENTER, circleMat), 
                na.policy="omit", 
                silent=FALSE, 
                na.rm=TRUE)

p1 <- ggplot() + geom_spatraster(data=zm_c_2000_sim[["1"]])
p2 <- ggplot() + geom_spatraster(data=result)
plot_grid(p1, p2, labels = c('Tree Cover', 'Is Optimal'), label_size = 12)
