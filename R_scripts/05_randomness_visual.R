##### libraries #####
library(terra)
library(tidyterra)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggpubr)
source("./R_Scripts/00_Functions.R")

##### visual example of a stochastic simulation for one 30x30 ####
coverVal = 30
mat <- as.vector(MonteCarloMat(coverVal, numSub=10))
img <- rast(matrix(mat$`sample(vec, size = numCell)`, nrow=10, ncol=10))
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
zm2000_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2000_sim.tif")
zm2005_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2005_sim.tif")
zm2010_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2010_sim.tif")
zm2015_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2015_sim.tif")



#### zoomed in plot sim, opt ####

cropExt <- ZoomExt(widthMult=4, xCellStart=42, yCellStart=59, img=zm2000_sim)
cropRect <- as.polygons(cropExt, crs=crs(zm2000_sim))

#REFERENCE OF ZOOM LOCATION
simLyr <- 2

p <- PlotSimImage(zm2000_sim[[simLyr]] 
                  ,paste("One Stochastic Simulation for the year 2000", "\n simulation #", toString(simLyr), sep="")
                  ,cropRect)
p <- p +
  scalebar(x.min=xmin(optAllCriteria)
           ,x.max=xmax(optAllCriteria)
           ,y.min=ymin(optAllCriteria)
           ,y.max=ymax(optAllCriteria)
           ,dist = 0.5
           ,dist_unit = "km"
           ,transform = FALSE 
           ,model = "WGS84"
           ,border.size=0.5
           ,st.size=3
           ,box.fill="white"
           ,box.color="grey"
           ,st.color="grey"
  ) +
  labs(y="", x="")
ggsave("./GeneratedPlots/OneStochasticSim_withZoom.jpeg"
       ,height=HEIGHT
       , width=WIDTH
       ,units="in")

#ggplot() +
#  geom_spatraster(data=zm2000_opt_fac[[1]], maxcell=10e+05) +
#  scale_fill_manual(name = "value", values = c(SUBTREE, SUBGROUND, OPTIMAL), na.translate=F)+  
#  geom_spatvector(data=cropRect, fill=NA, colour="black")

#zm2000_sim_crop <- crop(zm2000_sim_fac, cropExt)
#zm2000_opt_crop <- crop(zm2000_opt_fac, cropExt)


#### alpha sum plot ####

#convert 100 values to an "alpha" -> 1/100
zm2000_sim_alpha <- zm2000_sim * 1/100 * 1/100

zm2000_sim_alphaSum <- sum(zm2000_sim_alpha)

cropExt <- ZoomExt(widthMult=4, xCellStart=13, yCellStart=25, img=zm2000_sim)
cropRect <- as.polygons(cropExt, crs=crs(zm2000_sim))

zm2000_sim_alphaSum_crop <- crop(zm2000_sim_alphaSum, cropExt)

p1 <- ggplot() +
  geom_spatraster(data=zm2000_sim_alphaSum, maxcell=10e+05) +
  geom_spatvector(data=cropRect, fill=NA, colour="white", linewidth=0.4)+
  scale_fill_viridis_c(na.value=NA
                       ,name="value")
  
  
p2<- ggplot() + 
  geom_spatraster(data=zm2000_sim_alphaSum_crop) +
  scale_fill_viridis_c(na.value=NA
                       ,name="value") +
  labs(title="Zoomed Area")


jpeg("./GeneratedPlots/simsSummedAlpha.jpeg", height = im.width * aspect.r, width = im.width)
grid.arrange(p1, p2, nrow = 1, top="Sum of Tree distributation simulations, \nwhere tree values = 1/100, nSimulations=100. Year 2000 ")
dev.off()


