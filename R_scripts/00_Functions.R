#### functions ####
CrossTabSummary <- function(img, lyrName){
  #ASSUMPTIONS:
  # img has two layers: cover and err. Cover comes first
  print(names(img)[1])
  crossed <- crosstab(img, long=TRUE)
  result <- crossed %>% 
    #not sure how to not hardcode "err" and "cover" below
    mutate(intermediateTotal=err*Freq) %>% 
    group_by(across(colnames(crossed)[1])) %>% #by the first layer (cover)
    summarize(errTotal=sum(intermediateTotal), errAvg = sum(err*Freq) / sum(Freq), errMax=max(err)) %>%
    mutate(layer=lyrName) %>% #gets sums and averages of err for cover values
    rename(value=cover) # to match freqAll table
  return(result)
}
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