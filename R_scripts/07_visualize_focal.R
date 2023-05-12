#### readme ####
#the purpose of this script is to create a plot which visualizes the focal functions
#used to generate optimal images

#### libraries ####
library(ggplot2)
library(cowplot)
library(terra)
library(ggpubr)
library(reshape)

source("./R_Scripts/00_Functions.R")

#### funcs and constants ####
INQUESTION <- GROUND
IGNORE <- "#D4D4D4"
CONSIDER <- "#FF59A3"
#"#fc8d62"

FocalPlot <- function(focalDat){
  return(ggplot(data=focalDat, aes(x=X1, y=X2, fill=value) ) +
           geom_tile(color="white") +
           scale_fill_manual(values=c(INQUESTION, IGNORE, CONSIDER),
                             labels=c("Cell In Question", "Ignored", "Considered"),
                             name="") +
           coord_fixed() +
           labs(x="", y="")
         
  )
}

#### Get Focal Matrices ####
zm_c_2000_sim <- rast("./R_output/poly2014-09-29/sim3x3/zm2000_sim.tif")

circleMat <- GetCircleMat(zm_c_2000_sim)
circleMat <- OnlyNorth(circleMat)
circleMat <- t(apply(circleMat, 1, rev))
circleMat[CenterCell(circleMat)] <- -1


lineMat <- NorthLine(zm_c_2000_sim)
lineMat <- t(apply(lineMat, 1, rev))
lineMat[CenterCell(lineMat)] <- -1

lM <- melt(lineMat)
lM$value <- as.factor(lM$value)

cM <- melt(circleMat)
cM$value <- as.factor(cM$value)


p1 <- FocalPlot(cM) +
  geom_tile(data=cM[c(16),], fill=NA, color=TREE, linewidth=1 ) +
  geom_tile(data=cM[c(25),], fill=NA, color=TREE, linewidth=1 ) +
  labs(title="A) North Semicircle") +
  theme_void()


p2 <- FocalPlot(lM) +
  geom_tile(data=cM[c(17),], fill=NA, color=TREE, linewidth=1 ) +
  geom_tile(data=cM[c(5),], fill=NA, color=TREE, linewidth=1 ) +
  labs(title="B) North Line") +
  theme_void()

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave("./GeneratedPlots/focalFuncs.png")


