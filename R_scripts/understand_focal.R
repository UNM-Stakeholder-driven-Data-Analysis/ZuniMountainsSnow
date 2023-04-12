library(terra)
library(ggplot2)
library(cowplot)
library(tidyterra)
source("./R_Scripts/00_Functions.R")



#make arandom image of tree cover
N = 40
img <- rast(matrix(rbinom(N*N, 1, 0.5), nrow=N, ncol=N))
img <- img *100
#set the cetnral cells to no tree cover
img[11:30, 11:30] <- 0


c <- focalMat(img, 5, "circle")
center = ceiling(nrow(c)/2)
c[center,center] = 0
c[c>0] = 1


p1 <- ggplot() + 
  geom_spatraster(data=img) + 
  coord_fixed() +
  theme(
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE
  )



m <- matrix(data=1, nrow=11, ncol=11)


nRows = 11
CENTER = nRows * 5 + 6 #11 columns, go to the 6th row, go in by 6 columns to get

result <- focal(img, w=m, fun=function(x, na.rm) IsOptimal(x, na.rm, CENTER, circleMat), na.policy="omit", silent=FALSE, na.rm=TRUE)

p2 <- ggplot() + 
  geom_spatraster(data=result) + 
  coord_fixed() +
  #scale_y_continuous(breaks=seq(1,10,by=1)) + 
  #scale_x_continuous(breaks=seq(1,10,by=1)) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE
  )

plot_grid(p1, p2, labels = c('Tree Cover', 'Is Optimal'), label_size = 12)


#### Buffer Version of Same ####
#buf_result <- buffer(img, 4)

