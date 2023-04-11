#### readme ####
#process optimal area images to find total optimal area distributions

#### libraries ####
library(terra)
library(tidyr)
NAMES <-  c("yr2000", "yr2005", "yr2010", "yr2015")

#### load data ####

zm2000_opt <- rast("./R_output/poly2014-09-29/zm2000_opt.tif")
zm2005_opt <- rast("./R_output/poly2014-09-29/zm2005_opt.tif")
zm2010_opt <- rast("./R_output/poly2014-09-29/zm2010_opt.tif")
zm2015_opt <- rast("./R_output/poly2014-09-29/zm2015_opt.tif")

optImgs <- list(zm2000_opt, zm2005_opt, zm2010_opt, zm2015_opt)
#optImgSDS <- sds(zm2000_opt, zm2005_opt, zm2010_opt, zm2015_opt)
#names(optImgSDS) <-  c("yr2000", "yr2005", "yr2010", "yr2015")

#### visual sanity check ####
layr <- 2
optLayerSamp <- c(optImgSDS$yr2000[[layr]], 
                  optImgSDS$yr2005[[layr]], 
                  optImgSDS$yr2010[[layr]],
                  optImgSDS$yr2015[[layr]])
names(optLayerSamp) <- NAMES
ggplot() +
  geom_spatraster(data=optLayerSamp) +
  facet_wrap(~lyr) + 
  labs(title="optimal image for one layer accross all years")

#### calculate total optimal area ####
TotArea <- function(imgStack){
  #in meters squared
  #note using subst 0 -> NA and then expanse would also work. but this is probably faster
  numCells <- global(imgStack, fun="sum", na.rm=TRUE)
  totArea <- numCells * 9 #cells are 3x3m
  return(totArea)
}

totAreaWide <- as_tibble(as.data.frame(lapply(optImgs, TotArea)))
names(totAreaWide) <- NAMES
totArea <- totAreaWide %>% 
  pivot_longer(everything(), names_to="year", values_to="area") %>%
  mutate(area=area/(1000*1000)) #convert to km^2


#### visualize total optimal area distributions ###
ggplot(totArea) +
  #geom_violin(mapping=aes(factor(year), area)) +
  #geom_boxplot(mapping=aes(factor(year), area)) +
  geom_jitter(mapping = aes(factor(year), area), alpha=0.5) +
  labs(title="Total Optimal Area for Polygon 2014-09-29",
       x="",
       y="total optimal area [km^2]")
