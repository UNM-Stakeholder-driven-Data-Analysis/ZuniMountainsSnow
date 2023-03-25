library(random)

# X is cover of cell
X <- 20
x1 <- round(runif(1, 0, min(4*X, 100)))
x2 <- round(runif(1, 0, min(4*X-x1, 100)))
x3 <- round(runif(1, 0, min(4*X-x1-x2, 100)))
x4 <- 4*X - x1 - x2 -x3

X_check <- 1/4 * ( x1 + x2 + x3 + x4)


sub_cells <- function(cover){
  #for setting the lower bound we know that the minimal condition
  #exists when all the other unkown cells are as large as possible (100%)
  # or zero
  
  #The upper bound is at most all of the cover available concentrated in one cell
  # or 100
  x1 <- round(runif(1, max(4*cover-300, 0), min(4*cover, 100)))
  x2 <- round(runif(1, max(4*cover-x1-200, 0), min(4*cover-x1, 100)))
  x3 <- round(runif(1, max(4*cover-x1-x2-100, 0), min(4*cover-x1-x2, 100)))
  x4 <- 4*cover - x1 - x2 -x3
  #TODO: consider changing to an assertion for the sake of safety
  #TODO: consider adding assertions for 0-100 for x1-x4 for the sake of safety
  #(possibly at the expense of speed)
  print(sum(x1, x2, x3, x4)*.25)
  return(c(x1, x2, x3, x4))
}