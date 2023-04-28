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



#### functions ####

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
    mutate(value=factor(value, labels=c("tree", "ground", "optimal"))) %>%
    #mutate(criterion=name) #usefull so multiple criterion outputs can be combined
  
  #assert_that(length(unique(optimalInfo$tot_count)) == 1)
  return(optimalInfo)
}

#### load data ####
folders <- c(#"n1_3x3_all", 
             "n1_3x3_north", 
             "n2_3x3_north",
             "n3_3x3_north",
             "n4_3x3_north",
             #"n5_3x3_all", 
             "n5_3x3_north",
             "n1_3x3_northline",
             "n2_3x3_lineNeighbors1")
optVariants <- lapply(folders, LoadOptList)
names(optVariants) <- folders

#### calculate total optimal area ####

#get counts of each cell type, accross all years and layers
freqStats <- lapply(optVariants, function(variant) FreqStats(variant))

#add a criterion variable which is set to the name of the optimal criterion run
#so that all stats can be bound together
freqStats <- lapply(seq_along(freqStats), function(i) mutate(freqStats[[i]], criterion=folders[i]))

allData <- bind_rows(freqStats)

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
#jpeg("./GeneratedPlots/totOptArea.jpeg")
ggplot(filter(allData, value=="optimal", criterion %in% c("n1_3x3_northline", "n2_3x3_lineNeighbors1"))) +
  geom_jitter(mapping = aes(factor(year), area_km, color=criterion),width=.2, height=0, alpha=0.5, size=.5) +
  #geom_boxplot(mapping=aes(factor(year), area_km, color=criterion))+
  scale_y_continuous(limits=c(0,3),breaks=seq(0,3,by=0.5)) +
  labs(title="Total Optimal Area for Polygon 2014-09-29",
       x="",
       y="total optimal area [km^2]")+
  guides(colour = guide_legend(override.aes = list(size=4)))
#dev.off()


#jpeg("./GeneratedPlots/totOptArea_2015Zoom.jpeg")
#optInfo2015 <- filter(optimalInfo, year=="yr2015")
#ggplot(optimalInfo) +
#  geom_violin(mapping=aes(factor(year), area_km), width=0.25) +
#  geom_jitter(mapping = aes(factor(year), area_km),width=0.03, height=0, alpha=0.5) +
#  scale_y_continuous(labels=function(x) sprintf("%.4f", x)) +
#  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
#  facet_wrap(~year, scales = "free") +
#  labs(title="Total Optimal Area for Polygon 2014-09-29",
#       x="",
#       y="total optimal area [km^2]")
#dev.off()
