# code snippet to generate random cover distribution

MonteCarloMat <- function(cover){
  #generate a possible distribution of 100% cover sub-cells so that the % cover
  #of the entire 10x10 matrix is equal to cover
  #INPUT:
  # cover has range 0-100 integer
  #OUPUT:
  # 10x10 matrix where each value is 0 or 100
  
  maxCover <- 100 #maximum value that "cover" can take
  maxVal <-100 #value to represent 100% cover in the new matrix
  vec <- c(replicate(cover, maxVal), replicate(maxCover-cover, 0))
  return(matrix(sample(vec, size=100), nrow=10, ncol=10 ))
}