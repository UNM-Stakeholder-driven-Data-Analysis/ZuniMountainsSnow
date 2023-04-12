##### libraries #####
library(terra)
library(tidyterra)
library(ggplot2)
library(cowplot)
library(gridExtra)



#### load data ####
zm2000_sim <- rast("./R_output/poly2014-09-29/zm2000_sim.tif")
zm2005_sim <- rast("./R_output/poly2014-09-29/zm2005_sim.tif")
zm2010_sim <- rast("./R_output/poly2014-09-29/zm2010_sim.tif")
zm2015_sim <- rast("./R_output/poly2014-09-29/zm2015_sim.tif")


zm2000_opt <- rast("./R_output/poly2014-09-29/zm2000_opt.tif")
zm2005_opt <- rast("./R_output/poly2014-09-29/zm2005_opt.tif")
zm2010_opt <- rast("./R_output/poly2014-09-29/zm2010_opt.tif")
zm2015_opt <- rast("./R_output/poly2014-09-29/zm2015_opt.tif")

#dang probably shold have converted to factor rasters in the first place. oh well
simLevels <- lapply(1:100, function(x) data.frame(ID=c(0, 100), cover=c("ground", "tree")))
optLevels <- lapply(1:100, function(x) data.frame(ID=c(0, 1), cover=c("suboptimal", "optimal")))


zm2000_sim <- as.factor(zm2000_sim)
levels(zm2000_sim) <- simLevels
names(zm2000_sim) <- 1:100

zm2000_opt <- as.factor(zm2000_opt)
levels(zm2000_opt) <- optLevels
names(zm2000_opt) <- 1:100

#### zoomed in plot ####

ZoomExt <- function(widthMult, xCellStart, yCellStart, img, cellSize=30 ){
  
  width = cellSize * widthMult
  xMin = xmin(img)
  yMin = ymin(img)
  xStart = xMin + cellSize * xCellStart 
  yStart = yMin + cellSize * yCellStart
  
  cropExt <- ext(xStart, xStart+width, yStart, yStart+width)
  
}

cropExt <- ZoomExt(widthMult=4, xCellStart=13, yCellStart=25, img=zm2000_sim)
cropRect <- as.polygons(cropExt, crs=crs(zm2000_sim))

ggplot() +
  geom_spatraster(data=zm2000_sim[[1]]) +
  geom_spatvector(data=cropRect, fill=NA, colour="white")

zm2000_sim_crop <- crop(zm2000_sim, cropExt)
zm2000_opt_crop <- crop(zm2000_opt, cropExt)

p1 <- ggplot() +
  geom_spatraster(data=zm2000_sim_crop[[1:3]]) + 
  facet_wrap(~lyr) +
  labs(title="Simulated Tree Distributions") +
  scale_fill_manual(name = "value", values = c("#E2CDA5", "#168920"))+  
  scale_x_continuous(breaks=1) + #hack to turn off labels + 
  scale_y_continuous(breaks=1) #hack to turn off labels 
p2 <- ggplot() +
   geom_spatraster(data=zm2000_opt_crop[[1:3]]) + facet_wrap(~lyr) +
  labs(title="Optimal Cells")+ 
  scale_fill_manual(name = "value", values = c("#8C8984", "white"))+  
  scale_x_continuous(breaks=1) + #hack to turn off labels + 
  scale_y_continuous(breaks=1) #hack to turn off labels

grid.arrange(p1, p2, ncol = 1, top="year 2000")


