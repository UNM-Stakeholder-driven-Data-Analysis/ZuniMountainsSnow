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

SubOptDistinguish <- function(sim, opt){
  #NOTE: in the future this function should go away, as it makes more sense
  #to distinguish betweeen suboptimal types during application of the focal
  # function IsOptimal
  treeNeg <- subst(sim, 100, -1)
  #opt has values 0=suboptimal, 1= optimal
  #and should always have 0 where trees occcured
  #so summing with trees as -1 will result in 
  # 0=suboptimal ground, -1=suboptimal tree, 1=optimal
  return(sum(treeNeg, opt))
  
}

GetCircleMat <- function(img, radius=15){
  #for use in focal function
  m <- focalMat(img, radius, "circle")
  center = ceiling(nrow(m)/2)
  m[center,center] = 0
  m[m>0] = 1
  return(m)
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

IsOptimal <- function(y, na.rm, CENTER, circleMat){
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
    return(ifelse(sum(matrix(y, nrow=nRows, ncol=nRows)*circleMat, na.rm=na.rm) > 0, 1, 0))
  }
  else {
    return(0)
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
MonteCarloImg <- function(img){
  #make an empty image with same extent, crs, and higher resolution
  # by randomly generating a plausable 10x10 grid for each single cell in img
  #ASSUMPTIONS:
  # img has resolution of 30x30
  # img values range from 0,100
  
  #copy the input image but at a higher resolution
  samp <- terra::rast(ext(img), resolution=c(3,3))
  crs(samp) <- crs(img)
  
  iterI <- nrow(img)
  iterJ <- ncol(img)
  
  rRng = c(low=1, high=10)
  cRng = c(low=1, high=10)
  for (i in 1:iterI){
    for (j in 1:iterJ){
      value = img[i, j][[1]]
      if (is.na(value)){
        samp[rRng["low"]:rRng["high"], cRng["low"]:cRng["high"]] = NA
      }
      else{
        #print(value)
        samp[rRng["low"]:rRng["high"], cRng["low"]:cRng["high"]] = MonteCarloMat(value)
      }
      cRng <- cRng + 10
    }
    cRng <- c(low=1, high=10)
    rRng <- rRng + 10
  }
  return(samp)
}

MonteCarloMat6x6 <- function(cover){
  #generate a possible distribution of 100% cover 6x6m sub-cells so that the % cover
  #of the entire 5x5 matrix is equal to cover
  #INPUT:
  # cover has range 0-100 integer
  #OUPUT:
  # 5x5 matrix where each value is 0 or 100
  
  numCell <- 25 # 6x6 subcell results in 25 sub cells per one 30x30m cell
  maxVal <-100 #value to represent 100% cover in the new matrix
  if (cover==0){
    return(as.data.frame(replicate(25, 0))) #spatRaster wants a data frame
  }
  else{
    numFilled <- round(cover * numCell / 100)
    vec <- c(replicate(numFilled, maxVal), replicate(numCell-numFilled, 0))
    return(as.data.frame(sample(vec, size=numCell)))
  }
}

MonteCarloMat <- function(cover){
  #generate a possible distribution of 100% cover sub-cells so that the % cover
  #of the entire 10x10 matrix is equal to cover
  #INPUT:
  # cover has range 0-100 integer
  #OUPUT:
  # 10x10 matrix where each value is 0 or 100
  
  maxCover <- 100 #maximum value that "cover" can take
  maxVal <-100 #value to represent 100% cover in the new matrix
  if (cover==0){
    return(as.data.frame(replicate(100, 0))) #spatRaster wants a data frame
  }
  else{
    vec <- c(replicate(cover, maxVal), replicate(maxCover-cover, 0))
    return(as.data.frame(sample(vec, size=100)))
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
  out <- set_names(out, c("start", "end"))
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
