#### readme ####
#process optimal area images to find total optimal area distributions

#### libraries ####
library(ggplot2)
library(tidyterra)
library(terra)
library(tidyr)
library(dplyr)
library(viridis) 
library(ggnewscale)
library(forcats)
source("./R_Scripts/00_Functions.R")
NAMES <-  c("yr2000", "yr2005", "yr2010", "yr2015")


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
             "c1_3x3_northline",
             "c2_3x3_northline",
             "n2_3x3_lineNeighbors1")
optVariants <- lapply(folders, function(folder) LoadOptList(folder, parentFolder=POLYNAME))
names(optVariants) <- folders

#### calculate total optimal area ####

#get counts of each cell type, accross all years and layers
freqStats <- lapply(optVariants, function(variant) FreqStats(variant))

#add a criterion variable which is set to the name of the optimal criterion run
#so that all stats can be bound together
freqStats <- lapply(seq_along(freqStats), function(i) mutate(freqStats[[i]], criterion=folders[i]))

allData <- bind_rows(freqStats)
#hack to access the number parameter, which has a different meaning depending on criterion
#for all criterion the number parameter is the second character in the string!
allData <- mutate(allData, numParam=substring(criterion, 2, 2)) 
allDataSummary <- allData %>%
  group_by(value, year, criterion, numParam)%>%
  summarise(mean_area_km=mean(area_km))


#### visualize total optimal area distributions ####
north_lines <- c("c1_3x3_northline"
                 #,"n2_3x3_lineNeighbors1" #thisone doesn't make sense
                 ,"c2_3x3_northline"
                 )
north_halfc <- c("n1_3x3_north"
             ,"n2_3x3_north"
             ,"n3_3x3_north"
             ,"n4_3x3_north"
             ,"n5_3x3_north"
             )
#category <-"ground"
category <-"optimal"
#category <- "tree"

ggplot() +
  ## NORTH LINE PLOT
  geom_jitter(data=filter(allData, value==category, criterion %in% north_lines), 
              aes(factor(year), area_km, color=numParam),
              width=.2, height=0, alpha=0.5, size=.5) +
  geom_line(data=filter(allDataSummary, value==category, criterion %in% north_lines)
            ,aes(factor(year), mean_area_km, color=numParam, group=criterion)
            ,show.legend = FALSE) +
  scale_color_brewer(palette = "Set2", name="North Line:\nminimum cluster size")+
  guides(colour = guide_legend(override.aes = list(size=4, alpha=1)))+
  
  new_scale_color() +
  
  ## NORTH HALF CIRCLE PLOT
  geom_jitter(data=filter(allData, value==category, criterion %in% north_halfc), 
              aes(factor(year), area_km, color=numParam),
              width=.2, height=0, alpha=0.5, size=.5) +
  geom_line(data=filter(allDataSummary, value==category, criterion %in% north_halfc)
            ,aes(factor(year), mean_area_km, color=numParam, group=criterion)
            ,show.legend = FALSE) +
  scale_color_viridis(discrete = TRUE, direction=-1, 
                      name="North Semicircle:\nminimum number of covered cells",) +
  guides(colour = guide_legend(override.aes = list(size=4, alpha=1),order=2))+

  # plot wide settings
  scale_y_continuous(limits=c(0,3),breaks=seq(0,3,by=0.5)) +
  labs(title="Total Optimal Area for Polygon 2014-09-29",
       x="",
       y=expression("total optimal area, km"^2))+
  theme_bw()
ggsave("./GeneratedPlots/totOpt_all.jpeg", width=PLOTWIDTH, height=PLOTHEIGHT, units="in")



#### visualize variation for one criteria ####
criteria <- "c1_3x3_northline"
category <- "optimal"
ggplot(filter(allData, value==category, criterion==criteria)) +
  theme_bw() +
  theme(legend.position="none") +
  geom_violin(mapping=aes(factor(year), area_km), width=0.25) +
  geom_jitter(aes(year, area_km),
              color="#66c2a5",
              width=.02, 
              height=0, 
              alpha=0.5, 
              size=.5,
              ) +
  scale_y_continuous(labels=function(x) sprintf("%.4f", x)) +
  facet_wrap(~fct_rev(year), scales="free")+
  labs(title="Within Year Variation for NorthLine Criteria, \nminimum cluster size=1"
       ,y=expression("total optimal area, km"^2)
       ,x="")
ggsave("./GeneratedPlots/c1_inYarVar.jpeg", width=PLOTWIDTH, height=PLOTHEIGHT, units="in")
  
  

