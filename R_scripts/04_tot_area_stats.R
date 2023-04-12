#### readme ####
#process optimal area images to find total optimal area distributions

#### libraries ####
library(ggplot2)
library(tidyterra)
library(terra)
library(tidyr)
library(dplyr)
NAMES <-  c("yr2000", "yr2005", "yr2010", "yr2015")

#### load data ####

zm2000_opt <- rast("./R_output/poly2014-09-29/zm2000_opt.tif")
zm2005_opt <- rast("./R_output/poly2014-09-29/zm2005_opt.tif")
zm2010_opt <- rast("./R_output/poly2014-09-29/zm2010_opt.tif")
zm2015_opt <- rast("./R_output/poly2014-09-29/zm2015_opt.tif")

optImgs <- list(zm2000_opt, zm2005_opt, zm2010_opt, zm2015_opt)
optImgSDS <- sds(zm2000_opt, zm2005_opt, zm2010_opt, zm2015_opt)
#names(optImgSDS) <-  c("yr2000", "yr2005", "yr2010", "yr2015")

#### visual sanity check ####
layr <- 50
optLayerSamp <- c(zm2000_opt[[layr]], 
                  zm2005_opt[[layr]], 
                  zm2010_opt[[layr]],
                  zm2015_opt[[layr]])
names(optLayerSamp) <- NAMES
ggplot() +
  geom_spatraster(data=optLayerSamp) +
  facet_wrap(~lyr) + 
  labs(title="optimal image for one layer accross all years")

#### calculate total optimal area ####
NumOptCells <- function(imgStack){
  #in meters squared
  #note using subst 0 -> NA and then expanse would also work. but this is probably faster
  numCells <- global(imgStack, fun="sum", na.rm=TRUE)
  #totArea <- numCells * 9 #cells are 3x3m
  return(numCells)
}

totNumWide <- as_tibble(as.data.frame(lapply(optImgs, NumOptCells))) 
names(totNumWide) <- NAMES
totNumWide <- mutate(totNumWide, layer=1:100)

optimalInfo <- totNumWide %>% 
  pivot_longer(all_of(NAMES), names_to="year", values_to="count") %>%
  mutate(area_m=count * 9,
         area_km = area_m /(1000*1000)) #convert to km^2


#### get max and min layers ####
optMax <- optimalInfo %>%
  group_by(year) %>%
  filter(count == max(count)) %>%
  mutate(status="max")
optMin <- optimalInfo %>%
  group_by(year) %>%
  filter(count==min(count)) %>%
  mutate(status="min")

optMinMax <- bind_rows(optMax, optMin)
maxLyr <- filter(optMinMax, year=="yr2000", status=="max")$layer
maxLyr
minLyr <- filter(optMinMax, year=="yr2000", status=="min")$layer
# show the layers with maximium and minimum amount of optimal area
ggplot() +
  geom_spatraster(data=optImgSDS$zm2000_opt[[c(maxLyr, minLyr)]], maxcell=10e+05) +
  facet_wrap(~lyr) +
  labs(title="Year 2000 max (left) and min(right) opt images")



#totArea1yr <- filter(totNum, year=="yr2000")
#### visualize total optimal area distributions ###
ggplot(optimalInfo) +
  #geom_violin(mapping=aes(factor(year), area)) +
  #geom_boxplot(mapping=aes(factor(year), area)) +
  geom_jitter(mapping = aes(factor(year), area_km), alpha=0.5) +
  labs(title="Total Optimal Area for Polygon 2014-09-29",
       x="",
       y="total optimal area [km^2]")
