##### libraries #####
library(terra)


#### load data ####
zm2000_sim <- rast("./R_output/poly2014-09-29/zm2000_sim.tif")
zm2005_sim <- rast("./R_output/poly2014-09-29/zm2005_sim.tif")
zm2010_sim <- rast("./R_output/poly2014-09-29/zm2010_sim.tif")
zm2015_sim <- rast("./R_output/poly2014-09-29/zm2015_sim.tif")


zm2000_opt <- rast("./R_output/poly2014-09-29/zm2000_opt.tif")
zm2005_opt <- rast("./R_output/poly2014-09-29/zm2005_opt.tif")
zm2010_opt <- rast("./R_output/poly2014-09-29/zm2010_opt.tif")
zm2015_opt <- rast("./R_output/poly2014-09-29/zm2015_opt.tif")


#### zoomed in plot ####

cellSize = 30
width = cellSize * 4
xMin = xmin(zm2000_sim)
yMin = ymin(zm2000_sim)
xStart = xMin + cellSize * 13 #round(758000/30) * 30
yStart = yMin + cellSize * 25 #round(3898600/30) * 30 # set to the nearest multiple of 30

cropExt <- ext(xStart, xStart+width, yStart, yStart+width)
cropRect <- as.polygons(cropExt, crs=crs(zm2000_sim))

ggplot() +
  geom_spatraster(data=zm2000_sim[[1]]) +
  geom_spatvector(data=cropRect, fill=NA, colour="white")
