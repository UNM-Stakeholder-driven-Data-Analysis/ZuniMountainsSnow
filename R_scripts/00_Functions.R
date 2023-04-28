#### Libraries ####
library(rgdal)
library(sf)
library(spdep)
library(raster)
library(terra)
library(rasterVis)
library(sp)
library(gridExtra)
library(grid)
library(ggplot2)
library(comprehenr)
library(dplyr)
library(xml2)
library(XML)
library(assertthat)
library(plotly)

#### Constants ####
im.width = 1024
aspect.r = 0.707 #0.707 is a conve nient aspect.ratio

#activities that are assumed to not alter tree cover
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

GROUND <- "#E2CDA5"
TREE <- "#168920"
OPTIMAL <- "white"
SUBTREE <- TREE
SUBGROUND <- GROUND
#### loading functions ####

fileNames <- c("zm2000_opt.tif",
               "zm2005_opt.tif",
               "zm2010_opt.tif",
               "zm2015_opt.tif")

LoadOptList <- function(folder){
  #list of spatRasters
  optImgList <- lapply(fileNames, function(file) rast(file.path("./R_output", polyName, folder, file)))
  names(optImgList) <-  c("yr2000", "yr2005", "yr2010", "yr2015")
  return(optImgList)
}

#### visualization functions ####

ZoomExt <- function(widthMult, xCellStart, yCellStart, img, cellSize=30 ){
  
  width = cellSize * widthMult
  xMin = xmin(img)
  yMin = ymin(img)
  xStart = xMin + cellSize * xCellStart 
  yStart = yMin + cellSize * yCellStart
  
  cropExt <- ext(xStart, xStart+width, yStart, yStart+width)
  
}

OptLevels <- function(){data.frame(ID=c(-1, 0, 1), cover=c("tree", "bare", "optimal"))}

PlotOptImageLyr <- function(oi, title){
  oi <- as.factor(oi)
  levels(oi) <- lapply(1:nlyr(oi), function(x) OptLevels())
  return(ggplot() +
           geom_spatraster(data=oi, maxcell=10e+05) +
           scale_fill_manual(name="value", values = c(SUBTREE, SUBGROUND, OPTIMAL),na.translate=F)+  
           facet_wrap(~lyr) +
           labs(title=title))
}
PlotOptImage <- function(oi, title){
  return(ggplot() +
    geom_spatraster(data=oi, maxcell=10e+05, interpolate=FALSE) +
    scale_fill_manual(name="value", 
                      values = c(SUBTREE, SUBGROUND, OPTIMAL),
                      labels=c("tree cover", "ground", "optimal"),
                      na.translate=F)+  
    labs(title=title))
}

#### functions ####

SimToFactor <- function(simImg){
  #convert a numeric simulation image raster to a factor raster
  #values are 0, 100. 0 means bare ground, 100 means tree cover
  #ASSUMPTIONS
  # 100 layers in simImg, each a unique stochastic simulation
  # 0 -> ground
  # 100 -> tree cover
  simLevels <- lapply(1:100, function(x) data.frame(ID=c(0, 100), cover=c("ground", "tree-cover")))
  
  simImgFac <- as.factor(simImg)
  levels(simImgFac) <- simLevels
  names(simImgFac) <- 1:100
  return(simImgFac)
}

OptToFactor <- function(optImg){
  #convert a numeric optimal image raster to a factor raster
  #where optimal images has values -1, 0, 1
  #which correspond to "tree", "bare", "optimal", respectively
  numLayers <- nlyr(optImg)
  optLevels <- lapply(1:numLayers, function(x) data.frame(ID=c(-1, 0, 1), cover=c("tree", "bare", "optimal")))
  
  optImgFac <- as.factor(optImg)
  levels(optImgFac) <- optLevels
  names(optImgFac) <- 1:numLayers
  return(optImgFac)
}

NorthLines <- function(img, radius=15, n=1){
  #n is the number of neighbors to include on either side of the
  #northLine (or south line, depending on how you look at it)
  circleMat <- GetCircleMat(img, radius)
  circleMat[,] <- 0
  centerRow <-ceiling(nrow(circleMat)/2)
  sourthernCols <- (ceiling(ncol(circleMat)/2)+1):ncol(circleMat)
  circleMat[(centerRow-n):(centerRow+n), sourthernCols] <- 1
  return(circleMat)
}

NorthLine <- function(img, radius=15){
  #convenient to get matrix of correct size no matter what
  #resolution
  #assumes cirlceMat has dimensions that are odd numbers
  #which happens because focalMat
  circleMat <- GetCircleMat(img, radius)
  circleMat[,] <- 0
  circleMat[ceiling(nrow(circleMat)/2), (ceiling(ncol(circleMat)/2)+1):ncol(circleMat) ] <- 1
  return(circleMat)
}
CenterCell <- function(mat){
  #assumes nrow == ncol
  nRow <- nrow(mat)
  #for an odd sized square matrix represented at a vector, going from top left to right, then down by column,
  #the central cell is equivalent to:
  CENTER = nRow * floor(nRow/2) + ceiling(nRow/2) #11 columns, go to the 6th row, go in by 6 columns to get
  return(CENTER)
}
OnlyNorth <- function(circleMat){
  circleMat[,1:(ncol(circleMat)-1)/2] = 0
  return(circleMat)
}
GetCircleMat <- function(img, radius=15){
  #for use in focal function
  m <- focalMat(img, radius, "circle")
  center = ceiling(nrow(m)/2)
  m[center,center] = 0
  m[m>0] = 1
  return(m)
}

PlotRastAsMat <- function(img){
  imgMat <- rast(as.matrix(img, wide=TRUE)) #strips coordinate info
  imgMat <- as.factor(imgMat)
  ggplot() +
    geom_spatraster(data=imgMat) +
    scale_fill_manual(name="value", 
                      values = c(SUBTREE, SUBGROUND, OPTIMAL),
                      labels=c("tree cover", "ground", "optimal"),
                      na.translate=F)+
    coord_fixed() +
    scale_y_continuous(breaks=seq(1,nrow(imgMat),by=1)) + 
    scale_x_continuous(breaks=seq(1,ncol(imgMat),by=1)) +
    theme(
      panel.background = element_rect(fill = NA),
      panel.grid.major = element_line(linewidth=0.3, colour = "grey"),
      panel.ontop = TRUE,
      panel.grid.minor = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
    )
  
    
}

PlotFocalMat <- function(mat){
  ggplot() + 
    geom_spatraster(data=rast(mat)) + 
    coord_fixed() + 
    scale_y_continuous(breaks=seq(1,10,by=1)) + 
    scale_x_continuous(breaks=seq(1,10,by=1)) +
    theme(
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE
    )
}

IsOptimal <- function(y, na.rm, CENTER, circleMat, numCells=1){
  #TODO: differentiate between sub optimal area due to tree-interception
  #and sub-optimal due to bare ground
  #to be used with focal
  # center is the middle of the wiehgts matrix supplied to focal
  # circleMat has the same dimensions as the wieghts matrix and only 1s defining
  # a filled in circle, with the center empty (0)
  nRows = sqrt(length(y))
  if(!is.na(y[CENTER]) & y[CENTER] == 0){
    #cell values are either 0 or 100
    # if any of the non central cells forming a rough (pixelated) circle are 100
    # the sum will be greater than 0
    return(ifelse(sum(matrix(y, nrow=nRows, ncol=nRows)*circleMat, na.rm=na.rm) >= numCells*100, 1, 0))
  }
  else {
    return(-1) #this is the case that center is a tree.
  }
}

CrossTabSummary <- function(img, lyrName){
  #ASSUMPTIONS:
  # img has two layers: cover and err. Cover comes first
  print(names(img)[1])
  crossed <- crosstab(img, long=TRUE)
  result <- crossed %>% 
    #not sure how to not hardcode "err" and "cover" below
    mutate(intermediateTotal=err*n) %>% 
    group_by(across(colnames(crossed)[1])) %>% #by the first layer (cover)
    summarize(errTotal=sum(intermediateTotal), errAvg = sum(err*n) / sum(n), errMax=max(err)) %>%
    mutate(layer=lyrName) %>% #gets sums and averages of err for cover values
    rename(value=cover) # to match freqAll table
  return(result)
}
# in the process of generalizing to any whole number division of 30x30m cell!
MonteCarloImg <- function(img, subCellSize){
  #make an empty image with same extent, crs, and higher resolution
  # by randomly generating a plausable 10x10 grid for each single cell in img
  #ASSUMPTIONS:
  # img has resolution of 30x30
  # img values range from 0,100
  
  #copy the input image but at a higher resolution
  assert_that(res(img)[1] %% subCellSize == 0) # subCellSize must fit evenly into the image resolution
  numSubPerSide <- res(img)[1] / subCellSize #along one direction, number of sub cells per one large cell
  samp <- terra::rast(ext(img), resolution=c(subCellSize, subCellSize))
  crs(samp) <- crs(img)
  
  iterI <- nrow(img)
  iterJ <- ncol(img)
  
  rRng = c(low=1, high=numSubPerSide)
  cRng = c(low=1, high=numSubPerSide)
  for (i in 1:iterI){
    for (j in 1:iterJ){
      value = img[i, j][[1]]
      if (is.na(value)){
        samp[rRng["low"]:rRng["high"], cRng["low"]:cRng["high"]] = NA
      }
      else{
        samp[rRng["low"]:rRng["high"], cRng["low"]:cRng["high"]] = MonteCarloMat(value, numSubPerSide)
      }
      cRng <- cRng + numSubPerSide
    }
    cRng <- c(low=1, high=numSubPerSide)
    rRng <- rRng + numSubPerSide
  }
  return(samp)
}

MonteCarloMat <- function(cover, numSub){
  #generate a possible distribution of 100% cover 6x6m sub-cells so that the % cover
  #of the entire 5x5 matrix is equal to cover
  #INPUT:
  # cover has range 0-100 integer
  #OUPUT:
  # 5x5 matrix where each value is 0 or 100
  
  numCell <- numSub * numSub
  maxVal <-100 #value to represent 100% cover in the new matrix
  numFilled <- round(cover * numCell / 100)
  if (numFilled==0){
    return(as.data.frame(replicate(numCell, 0))) #spatRaster wants a data frame
  }
  else{
    vec <- c(replicate(numFilled, maxVal), replicate(numCell-numFilled, 0))
    return(as.data.frame(sample(vec, size=numCell)))
  }
}


Identifier <- function(year, path, row, suffix=""){
  return(paste("p", path, "r", row, "_TC_", toString(year), suffix, sep=""))
}

YearFileIntermediate <- function(year, path, row, suffix){
  #useful because have file with same name as folder
  identifier <- Identifier(year, path, row)
  file_path <- file.path(".",
                         "data", 
                         paste("GFCC30TC_", toString(year), sep=""),
                         paste("GFCC30TC_", identifier, suffix, sep=""))
  return(file_path)
}

YearFile <- function(year, path, row, type="", suffix=".tif"){
  # type can be "", "_err" or "_idx"
  identifier <- Identifier(year, path, row, type)
  file_path <- file.path(YearFileIntermediate(year, path, row, suffix=""),
                         paste(identifier, suffix, sep=""))
  return(file_path)
}

MergedRaster <- function(year, type=""){
  
  p035r035 <- rast(YearFile(year, "035", "035", type))
  p035r036 <- rast(YearFile(year, "035", "036", type))
  
  final <- merge(p035r035, p035r036)
  time(final) <- year
  set.names(final, paste("yr", toString(year), sep=""))
  return(final)
}

GetRangeDateTime <- function(yr){
  # wrapper to make sure both images composing the zuni mountains have the same
  # datetime range
  rangeA <- RangeDateTime(YearFileIntermediate(yr, "035", "036", suffix=".zip.xml"))
  rangeB <- RangeDateTime(YearFileIntermediate(yr, "035", "035", suffix=".zip.xml"))
  assert_that(all(rangeA == rangeB))
  return(rangeA)
}

RangeDateTime <- function(path){
  xmlData <- xmlParse(read_xml(path))
  dtimeRange <- xmlToDataFrame(nodes=getNodeSet(xmlData, "//RangeDateTime"))
  endDT <- as.POSIXct(paste(dtimeRange$RangeEndingDate, dtimeRange$RangeEndingTime))
  startDT <- as.POSIXct(paste(dtimeRange$RangeBeginningDate, dtimeRange$RangeBeginningTime))
  
  out <- c(startDT, endDT)
  out <- setNames(out, c("start", "end"))
  return(out)
}

OnlyTreeCover <- function(treeCover){
  #From https://lpdaac.usgs.gov/documents/1371/GFCC_User_Guide_V1.pdf
  # 0-100: percent tree cover
  # 200: water
  # 210: cloud
  # 211: shadow
  # 220: fill value
  replace_mat = cbind(id=c(200, 210, 211, 220), v=c(NaN, NaN, NaN, NaN))
  treeCover <- classify(treeCover, replace_mat)
  return(treeCover)
  
}


ClassifiedImage <- function(cover){
  #create an image where optimal pixels have value 100, no optimal have value 0
  m <- c(0, 10, 0,
         11, 100, 100)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  terra::classify(cover, rclmat, right=NA)
}

DateNotIn <- function(date, ranges){
  #return True if the date does not fall in any of the datetime ranges
  #INPUT:
  # date: posixct datetime
  # ranges: tibble of dates
  val = TRUE
  for (dtRange in ranges){
    val = val & !(date >= dtRange["start"] & date <= dtRange["end"])
  }
  return(unname(val))
}
