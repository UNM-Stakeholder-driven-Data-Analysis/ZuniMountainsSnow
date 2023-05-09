#### libraries ####
library(ggplot2)
library(tidyterra)
library(terra)
library(tidyr)
library(dplyr)
source("./R_Scripts/00_Functions.R")

#### load data ####
folders <- c("n1_3x3_all", "n5_3x3_all", "n5_3x3_north")
optVariants <- lapply(folders, LoadOptList)
names(optVariants) <- folders

#### visualize ####
iData <- rast(lapply(optVariants, function(x) x$yr2015[[1]]))
iData <- as.factor(iData)

jpeg("./GeneratedPlots/optArea_CriteriaComparison.jpeg")
ggplot()+
  geom_spatraster(data=iData, maxcell=10e+05) +
  scale_fill_manual(name="", 
                    values = c(SUBTREE, SUBGROUND, OPTIMAL),
                    labels = c("tree cover", "ground", "optimal"),
                    na.translate=F) +  
  facet_wrap(~lyr) +
  labs(title="Optimal Images for different criteria, 2015")
dev.off()

