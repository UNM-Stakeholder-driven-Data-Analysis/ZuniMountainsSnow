# very simple example to see waht cross tabulate does
#it lets you see how may cells of one layer correspond to how many cells of a 
# particular value in another layer
library(terra)
library(dplyr)
r <- rast(nc=2, nr=2)
values(r) <- c(0, 4, 4, 4)
set.names(r, "r")
s <- rast(nc=2, nr=2)
values(s) <- c(17, 3, 3, 10)
set.names(s, "s")
x <- c(r, s)
plot(x)
crossed <- crosstab(x, long=TRUE)
crossed
CrossTabSummary <- function(img){
  #ASSUMPTIONS:
  # img has two layers: cover and err. Cover comes first
  print(names(img)[1])
  crossed <- crosstab(img, long=TRUE)
  result <- crossed %>% 
    #not sure how to not hardcode "err" below
    mutate(intermediateTotal=err*Freq) %>% 
    group_by(across(colnames(crossed)[1])) %>% #by the first layer (cover)
    summarize(total=sum(intermediateTotal), avg = sum(s*Freq) / sum(Freq)) 
  return(result)
}
#this is the sort of operation we want to do to sum the error values for a particular 
#class of pixels
result <- CrossTabSummary(x)
result
