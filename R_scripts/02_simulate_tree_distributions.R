#### README ####
# Generate MonteCarloImages
# for a chosen polygon group: generate 1000 stochastic simulations of the spatial
# distribution of 3x3m tree canopies (100% covered areas) based on the tree cover values
# in the 30x30m cell image
#### Packages ####
source("./R_Scripts/00_Functions.R")
#TODO: fill out other needed packages


#### load data ####

#TODO: remove below line
puerco_area_spat <- terra::vect("./data/Puerco Project Area/puerco_Project-polygon.shp")

yr2015 <- GetRangeDateTime(2015)
yr2010 <- GetRangeDateTime(2010)
yr2005 <- GetRangeDateTime(2005)
yr2000 <- GetRangeDateTime(2000)

dtRanges <- tibble(yr2000, yr2005, yr2010, yr2015)
START_DATE = dtRanges$yr2000["start"]
END_DATE = dtRanges$yr2015["end"]

tc2015 <- MergedRaster(2015)
tc2010 <- MergedRaster(2010)
tc2005 <- MergedRaster(2005)
tc2000 <- MergedRaster(2000)
#2015 and 2010 happen to have larger extents than 2005 and 2000
#making them the same allows us to combine as layers
tc2015 <- terra::crop(tc2015, tc2005)
tc2010 <- terra::crop(tc2010, tc2005)
#For some reason tc2010 has a different coord. ref designation that omits
# the datum, although the UTM zone is the same.
tc2010 <- terra::project(tc2010, tc2015)

tcAll <- c(tc2015, tc2010, tc2005, tc2000)


##### Isolate Thinning Polygons in Zuni Mountains ####
CIBOLA_FOREST = "03" #this is inferred by inspecting the map at the link:
#https://usfs.maps.arcgis.com/apps/mapviewer/index.html?layers=eb8f23442f374ea2adae683e6eb0f16a


forests <- sf::st_read("./data/ProclaimedForest.kml")
zuni_forest <- forests[forests$Name=="Zuni Mountains", 1]
zuni_forest <- sf::st_transform(zuni_forest, crs=st_crs(tc2015))

act_raw <- sf::st_read("./data/ActivityPolygon/Activities.gdb")
#TODO: rename these categories to other and still plot them
NO_ALTER_TC = c("Silvicultural Stand Examination", 
                "Stand Diagnosis Prepared", 
                "TSI Need Created- Precommercial Thin",
                "TSI Need (precommercial thinning) Eliminated",
                "Yarding - Removal of Fuels by Carrying or Dragging",
                "Underburn - Low Intensity (Majority of Unit)",
                "Piling of Fuels, Hand or Machine ",
                "Range Cover Manipulation",
                "Rearrangement of Fuels",
                "Stand Silviculture Prescription",
                "Burning of Piled Material")
all_cibola <- act_raw %>% 
  filter(AU_FOREST_CODE==CIBOLA_FOREST) %>% #filter immediately to increase processing speed
  filter(between(DATE_COMPLETED, START_DATE, END_DATE)) %>%
  filter(!ACTIVITY %in% NO_ALTER_TC) %>%
  sf::st_transform(crs=st_crs(tc2015))

intersection <- sf::st_intersects(zuni_forest, all_cibola)[[1]]
all_zuni <- all_cibola[intersection, ]

haz_fr <- all_zuni %>% 
  filter(ACTIVITY=="Thinning for Hazardous Fuels Reduction") %>%
  filter(DateNotIn(DATE_COMPLETED, dtRanges))

all_other <- filter(all_zuni, ACTIVITY!="Thinning for Hazardous Fuels Reduction")
haz_fr_iso <- st_difference(haz_fr, st_union(all_other))

p <- ggplot() +
  #geom_sf(data=zuni_forest) +
  geom_sf(data=all_zuni, aes(fill=ACTIVITY), alpha=0.70) +
  scale_fill_brewer(palette="Set3")

ggplotly(p) #interactive plot

ggplot() +
  geom_sf(data=haz_fr, fill="green") +
  geom_sf(data=haz_fr_iso, aes(fill=factor(DATE_COMPLETED))) +
  scale_fill_brewer()


#number of unique thinning events (not necessarily one polygon) that occured:
length(unique(haz_fr_iso$DATE_COMPLETED))

haz_fr_merged <- aggregate(haz_fr_iso, 
                           by = list(haz_fr_iso$DATE_COMPLETED),
                           FUN=function(x) x) #dummy function

ggplot() +
  geom_sf(data=haz_fr_merged, aes(fill=factor(Group.1)))

#### isolate one polygon group ####

p2014 <- "2014-09-29 18:00:00"
p2012 <- "2012-09-29 18:00:00" # doesn't have as obvious of reduction in TC
p2003 <- "2003-05-04 18:00:00" #

USE <- p2014

one_poly <- terra::vect(filter(haz_fr_merged, Group.1==USE))
poly_ext <- ext(one_poly)
one_poly <- terra::rasterize(one_poly, tc2015)
one_poly <- terra::crop(one_poly, poly_ext)
plot(one_poly)


#### isolate tree cover to the one polygon group ####
all <- tcAll %>% 
  terra::crop(one_poly) %>% 
  terra::mask(one_poly) %>%
  OnlyTreeCover()

#### generate a monte carlo sample ####

#test how long it takes to run MonteCarloImg
numSamples <- 100

tic(paste( "MonteCarloImg:", toString(numSamples), "samples"))
#initialize the first image
zm_c_2000_samp <- MonteCarloImg(all$yr2000)
set.names(zm_c_2000_samp, toString(1))
for (i in 2:numSamples){
  #TODO: set the name of the image so we don't have to do it after.
  img <- MonteCarloImg(all$yr2000)
  set.names(img, toString(i))
  #add the new image as a layer
  add(zm_c_2000_samp) <- img
  
}
toc()


writeRaster(zm_c_2000_samp, filename="./R_output/poly2014-09-29/zm2000_sim.tif", overwrite=TRUE)

zm_c_2000_samp_loaded <- rast("zm2000_sim.tif")


