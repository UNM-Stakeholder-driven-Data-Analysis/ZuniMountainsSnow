#### readme ####
#process optimal area images to find total optimal area distributions

#### libraries ####
library(ggplot2)
library(tidyterra)
library(terra)
library(tidyr)
library(dplyr)
source("./R_Scripts/00_Functions.R")
NAMES <-  c("yr2000", "yr2005", "yr2010", "yr2015")
polyName = "poly2014-09-29"

folders <- c("3x3_n1_all", "3x3_n5_north")
fileNames <- c("zm2000_opt.tif",
                "zm2005_opt.tif",
                "zm2010_opt.tif",
                "zm2015_opt.tif")

#### functions ####

LoadOptList <- function(folder){
  optImgList <- lapply(fileNames, function(file) rast(file.path("./R_output", polyName, folder, file)))
  names(optImgList) <-  c("yr2000", "yr2005", "yr2010", "yr2015")
  return(optImgList)
}

FreqStats <- function(optImgList){
  freqAll <- lapply(optImgList, freq)
  for (i in 1:length(freqAll)){
    freqAll[[i]] <- mutate(freqAll[[i]], year=names(freqAll)[i])
  }
  freqAll <- bind_rows(freqAll)
  freqAll <- as_tibble(freqAll)
  
  
  optimalInfo <- freqAll %>% 
    mutate(area_m =count * 9, area_km = area_m /(1000*1000))%>%
    group_by(year, layer) %>%
    mutate(tot_count=sum(count)) %>%
    ungroup() %>%
    mutate(prop=count/tot_count) %>%
    mutate(value=factor(value, labels=c("tree", "ground", "optimal")))
  
  assert_that(length(unique(optimalInfo$tot_count)) == 1)
  return(optimalInfo)
}

#### load data ####

n5.3x3.north <- LoadOptList("3x3_n5_north")
n5.3x3.north

n1.3x3.all <- LoadOptList("3x3_n1_all")
n1.3x3.all

# Plot optimal images for all years, one layer

#jpeg("./GeneratedPlots/optImg_allyrs_1lyr.jpeg")
#ggplot() +
#  geom_spatraster(data=zmAll_1sim, maxcell=10e+05) +
#  scale_fill_manual(name = "value", values = c(TREE, "magenta", OPTIMAL), na.translate=F)+  
#  facet_wrap(~lyr) +
#  labs(title="Optimal Image for one simulation per year.")
#dev.off()
  

#optImgs <- list(zm2000_opt, zm2005_opt, zm2010_opt, zm2015_opt)
#names(optImgs) <-  c("yr2000", "yr2005", "yr2010", "yr2015")


#optImgSDS <- sds(zm2000_opt, zm2005_opt, zm2010_opt, zm2015_opt)
#names(optImgSDS) <-  c("yr2000", "yr2005", "yr2010", "yr2015")

#### visual sanity check ####
#layr <- 50
#optLayerSamp <- c(zm2000_opt[[layr]], 
#                  zm2005_opt[[layr]], 
#                  zm2010_opt[[layr]],
#                  zm2015_opt[[layr]])
#names(optLayerSamp) <- NAMES
#ggplot() +
#  geom_spatraster(data=optLayerSamp, maxcell=10e+05) +
#  facet_wrap(~lyr) + 
#  labs(title="optimal image for one layer accross all years")

#### calculate total optimal area ####

#get counts of each cell type, accross all years and layers
n5.3x3.north_freq <- FreqStats(n5.3x3.north)
n1.3x3.all_freq <- FreqStats(n1.3x3.all)

n5.3x3.north_freq <- mutate(n5.3x3.north_freq,criterion="n5.3x3.north")
n1.3x3.all_freq <- mutate(n1.3x3.all_freq, criterion="n1.3x3.all")

allData <- bind_rows(n5.3x3.north_freq, n1.3x3.all_freq)

#### optimal summary ####
#not sure if optimal summary is necessary, for now commenting out
#optimalSummary <- optimalInfo %>% 
#  filter(value=="optimal") %>%
#  group_by(year) %>%
#  summarise(average_area=mean(area_km),
#            percent_area=mean(area_km)/(totExpanseArea / 1000^2) * 100,
#            range_area=max(area_km) - min(area_km),
#            range_area_m= max(area_m) - min(area_m))
#optimalSummary

#look at the contributions of tree vs ground on the total optimal area
#treeGroundContribSummary <- optimalInfo %>%
#  group_by(year, value) %>%
#  summarize(avg_prop = mean(prop) * 100, std_prop=sd(prop) * 100, rng_m=max(area_m)-min(area_m))
#treeGroundContribSummary
#write_delim(treeGroundContribSummary, "./GeneratedPlots/treeGroundContrib.csv", delim=",")

#library(knitr)
#kable(optimalSummary, format="markdown", 
#      col.names = c("year", "average area (km^2^)", "percent area optimal", "range area (km^2^)", "range area (m^2^)"),
#      digits=c(1, 2, 1, 4,0))


#### get max and min layers ####
#optMax <- optimalInfo %>%
#  group_by(year) %>%
#  filter(count == max(count)) %>%
#  mutate(status="max")
#optMin <- optimalInfo %>%
#  group_by(year) %>%
#  filter(count==min(count)) %>%
#  mutate(status="min")

#optMinMax <- bind_rows(optMax, optMin)
#maxLyr <- filter(optMinMax, year=="yr2000", status=="max")$layer
#maxLyr
#minLyr <- filter(optMinMax, year=="yr2000", status=="min")$layer
# show the layers with maximium and minimum amount of optimal area
#ggplot() +
#  geom_spatraster(data=optImgSDS$zm2000_opt[[c(maxLyr, minLyr)]], maxcell=10e+05) +
#  facet_wrap(~lyr) +
#  labs(title="Year 2000 max (left) and min(right) opt images")



#### visualize total optimal area distributions ####
jpeg("./GeneratedPlots/totOptArea.jpeg")
ggplot(filter(allData, value=="optimal")) +
  geom_jitter(mapping = aes(factor(year), area_km, color=criterion),width=0.1, height=0, alpha=0.5) +
  scale_y_continuous(limits=c(0,3),breaks=seq(0,3,by=0.5)) +
  labs(title="Total Optimal Area for Polygon 2014-09-29",
       x="",
       y="total optimal area [km^2]")
dev.off()


jpeg("./GeneratedPlots/totOptArea_2015Zoom.jpeg")
optInfo2015 <- filter(optimalInfo, year=="yr2015")
ggplot(optimalInfo) +
  geom_violin(mapping=aes(factor(year), area_km), width=0.25) +
  geom_jitter(mapping = aes(factor(year), area_km),width=0.03, height=0, alpha=0.5) +
  scale_y_continuous(labels=function(x) sprintf("%.4f", x)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  facet_wrap(~year, scales = "free") +
  labs(title="Total Optimal Area for Polygon 2014-09-29",
       x="",
       y="total optimal area [km^2]")
dev.off()
