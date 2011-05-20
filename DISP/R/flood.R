flood <-
function(lambda = 10000, xSize = detector$xDetRange,
                  ySize = detector$yDetRange){
 
  ## creates a uniform flood field of an expected number n X-ray photons in a rectangular area (on the
  ## detector face) of size |xSize| * |ySize|.
  
  # how many photons?
  n <- rpois(n = 1, lambda = lambda)
  
  # coordinates
  x <- runif(n, min = xSize[1], max = xSize[2])
  y <- runif(n, min = ySize[1], max = ySize[2])
  
  output <- cbind(x, y)
  if(length(output) == 0) return(NULL)
  return(output)
}

