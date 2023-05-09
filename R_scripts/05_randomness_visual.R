##### libraries #####
library(terra)
library(tidyterra)
library(ggplot2)
library(cowplot)
library(gridExtra)
source("./R_Scripts/00_Functions.R")

##### visual example of a stochastic simulation for one 30x30 ####
coverVal = 30
mat <- as.vector(MonteCarloMat(coverVal))
img <- rast(matrix(mat$`sample(vec, size = 100)`, nrow=10, ncol=10))
img_fac <- as.factor(img)
levels(img_fac) <-  data.frame(ID=c(0, 100), cover=c("ground", "tree cover"))

jpeg("./GeneratedPlots/one30x30randSim.jpeg")
ggplot() + 
  geom_spatraster(data=img_fac) + 
  coord_fixed() +
  labs(title=paste("A random configuration of", toString(coverVal), "3x3m subcells")) +
  scale_fill_manual(name="", values = c(GROUND, TREE), na.translate=F) +
  scale_y_continuous(breaks=seq(1,10,by=1), minor_breaks = 0) + 
  scale_x_continuous(breaks=seq(1,10,by=1), minor_breaks = 0) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE
  )
dev.off()



#### load data ####
zm2000_sim <- rast("./R_output/poly2014-09-29/zm2000_sim.tif")
zm2005_sim <- rast("./R_output/poly2014-09-29/zm2005_sim.tif")
zm2010_sim <- rast("./R_output/poly2014-09-29/zm2010_sim.tif")
zm2015_sim <- rast("./R_output/poly2014-09-29/zm2015_sim.tif")



zm2000_opt_raw <- rast("./R_output/poly2014-09-29/zm2000_opt.tif")
zm2005_opt <- rast("./R_output/poly2014-09-29/zm2005_opt.tif")
zm2010_opt <- rast("./R_output/poly2014-09-29/zm2010_opt.tif")
zm2015_opt <- rast("./R_output/poly2014-09-29/zm2015_opt.tif")

#### distinguish between suboptimal types ####


zm2000_opt <- SubOptDistinguish(zm2000_sim, zm2000_opt_raw)

#### convert to factors ####
#dang probably shold have converted to factor rasters in the first place. oh well
simLevels <- lapply(1:100, function(x) data.frame(ID=c(0, 100), cover=c("ground", "tree")))
optLevels <- lapply(1:100, function(x) data.frame(ID=c(-1, 0, 1), cover=c("tree", "bare", "optimal")))


zm2000_sim_fac <- as.factor(zm2000_sim)
levels(zm2000_sim_fac) <- simLevels
names(zm2000_sim_fac) <- 1:100

zm2000_opt_fac <- as.factor(zm2000_opt)
levels(zm2000_opt_fac) <- optLevels
names(zm2000_opt_fac) <- 1:100

#### zoomed in plot sim, opt ####



cropExt <- ZoomExt(widthMult=4, xCellStart=42, yCellStart=59, img=zm2000_sim)
cropRect <- as.polygons(cropExt, crs=crs(zm2000_sim))

#REFERENCE OF ZOOM LOCATION
jpeg("./GeneratedPlots/OneStochasticSim_withZoom.jpeg", height = im.width * aspect.r, width = im.width)
ggplot() +
  geom_spatraster(data=zm2000_sim_fac[[1]], maxcell=10e+05) +
  scale_fill_manual(name = "value", values = c(GROUND, TREE), na.translate=F)+  
  geom_spatvector(data=cropRect, fill=NA, colour="black", linewidth=0.5) +
  labs(title="One Stochastic Simulation for the year 2000")
dev.off()

ggplot() +
  geom_spatraster(data=zm2000_opt_fac[[1]], maxcell=10e+05) +
  scale_fill_manual(name = "value", values = c(SUBTREE, SUBGROUND, OPTIMAL), na.translate=F)+  
  geom_spatvector(data=cropRect, fill=NA, colour="black")

zm2000_sim_crop <- crop(zm2000_sim_fac, cropExt)
zm2000_opt_crop <- crop(zm2000_opt_fac, cropExt)

# ZOOM IN COMPARISON OF SIM AND OPT
p1 <- ggplot() +
  geom_spatraster(data=zm2000_sim_crop[[1:3]]) + 
  facet_wrap(~lyr) +
  labs(title="Simulated Tree Distributions") +
  scale_fill_manual(name = "value", values = c(GROUND, TREE))+  
  scale_x_continuous(breaks=1) + #hack to turn off labels + 
  scale_y_continuous(breaks=1) #hack to turn off labels 
p2 <- ggplot() +
   geom_spatraster(data=zm2000_opt_crop[[1:3]]) + facet_wrap(~lyr) +
  labs(title="Optimal Cells")+ 
  scale_fill_manual(name = "value", values = c(SUBTREE, SUBGROUND, OPTIMAL))+  
  scale_x_continuous(breaks=1) + #hack to turn off labels + 
  scale_y_continuous(breaks=1) #hack to turn off labels

jpeg("./GeneratedPlots/zoom_sim_opt_compare.jpeg", height = im.width * aspect.r, width = im.width)
grid.arrange(p1, p2, ncol = 1, top="year 2000")
dev.off()

TotArea <- function(imgStack){
  #in meters squared
  #note using subst 0 -> NA and then expanse would also work. but this is probably faster
  numCells <- global(imgStack, fun="sum", na.rm=TRUE)
  totArea <- numCells * 9 #cells are 3x3m
  return(totArea)
}

areaTot<- TotArea(zm2000_opt_crop)

#### alpha sum plot ####

#convert 100 values to an "alpha" -> 1/100
zm2000_sim_alpha <- zm2000_sim * 1/100 * 1/100

zm2000_sim_alphaSum <- sum(zm2000_sim_alpha)

cropExt <- ZoomExt(widthMult=4, xCellStart=13, yCellStart=25, img=zm2000_sim)
cropRect <- as.polygons(cropExt, crs=crs(zm2000_sim))

zm2000_sim_alphaSum_crop <- crop(zm2000_sim_alphaSum, cropExt)

p1 <- ggplot() +
  geom_spatraster(data=zm2000_sim_alphaSum, maxcell=10e+05) +
  geom_spatvector(data=cropRect, fill=NA, colour="white")
  
  
p2<- ggplot() + 
  geom_spatraster(data=zm2000_sim_alphaSum_crop) +
  labs(title="Zoomed Area")

jpeg("./GeneratedPlots/simsSummedAlpha.jpeg", height = im.width * aspect.r, width = im.width)
grid.arrange(p1, p2, nrow = 1, top="Sum of Tree distributation simulations, where tree values = 1/100, nSimulations=100. Year 2000 ")
dev.off()


