#### README ####
#Purpose: compare simulated tree cover images to google earth imagery
#strategy: for now manually use exported .kml file from 01_data_exploration
#so this script is only to show simulated images

#### libraries ####
library(terra)
library(ggplot2)
library(tidyterra)
source("./R_Scripts/00_Functions.R")

#### load data ####
zm2000_sim <- rast("./R_output/poly2014-09-29/zm2000_sim.tif")
zm2005_sim <- rast("./R_output/poly2014-09-29/zm2005_sim.tif")
zm2010_sim <- rast("./R_output/poly2014-09-29/zm2010_sim.tif")
zm2015_sim <- rast("./R_output/poly2014-09-29/zm2015_sim.tif")

zm2000_sim_fac <- SimToFactor(zm2000_sim)
zm2005_sim_fac <- SimToFactor(zm2005_sim)
zm2010_sim_fac <- SimToFactor(zm2010_sim)
zm2015_sim_fac <- SimToFactor(zm2015_sim)


### visualize ####
PlotOneSim <- function(simImgFac, year, layr=1){
  path <- file.path("./GeneratedPlots", paste("simImg", year, ".jpeg", sep=""))
  ggplot() +
    geom_spatraster(data=simImgFac[[layr]], maxcell=6e+05) +
    scale_fill_manual(name = "value", values = c(GROUND, TREE), na.translate=F) +
    labs(title=paste("A simulated tree cover image for the year", toString(year)))
  ggsave(path)
}

PlotOneSim(zm2000_sim_fac, 2000)
PlotOneSim(zm2005_sim_fac, 2005)
PlotOneSim(zm2010_sim_fac, 2010)
PlotOneSim(zm2015_sim_fac, 2015)

