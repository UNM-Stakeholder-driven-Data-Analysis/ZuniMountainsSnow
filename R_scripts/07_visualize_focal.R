#### readme ####
#the purpose of this script is to create a plot which visualizes the focal functions
#used to generate optimal images

#### libraries ####
library(ggplot2)
library(cowplot)
library(terra)
library(ggpubr)
source("./R_Scripts/00_Functions.R")

#### Get Focal Matrices ####
zm_c_2000_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2000_sim.tif")

circleMat <- GetCircleMat(zm_c_2000_sim)
circleMat <- OnlyNorth(circleMat)
circleMat
circleMat <- t(apply(circleMat, 1, rev))
circleMat[CenterCell(circleMat)] <- -1
circleMat


lineMat <- NorthLine(zm_c_2000_sim)
lineMat <- t(apply(lineMat, 1, rev))
lineMat[CenterCell(lineMat)] <- -1

lM <- melt(lineMat)
lM$value <- as.factor(lM$value)



cM <- melt(circleMat)
cM$value <- as.factor(cM$value)


p1 <- FocalPlot(cM) +
  geom_tile(data=cM[c(16),], fill=NA, color="green", linewidth=1 ) +
  geom_tile(data=cM[c(25),], fill=NA, color="green", linewidth=1 ) +
  labs(title="North Semicircle")


p2 <- FocalPlot(lM) +
  geom_tile(data=cM[c(17),], fill=NA, color="green", linewidth=1 ) +
  geom_tile(data=cM[c(5),], fill=NA, color="green", linewidth=1 ) +
  labs(title="North Line")



FocalPlot <- function(focalDat){
  p1 <- ggplot(data=focalDat, aes(x=Var1, y=Var2, fill=value) ) +
    geom_tile(color="black") +
    scale_fill_manual(values=c("magenta", "grey", "red"),
                      labels=c("Cell In Question", "Ignored", "Considered"),
                      name="") +
    coord_fixed() +
    theme_void()
}




p<- ggarrange(p1, p2, common.legend = T, legend="right")
annotate_figure(p, top = "Focal Functions")
