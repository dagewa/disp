spot <-
function(lambda = 100000, x = mean(detector$xDetRange), y = mean(detector$yDetRange),
                 sdx = 0.1, sdy = 0.1, rho = 0.3){
  
  ## creates a spot of an expectation value of lambda Xph  where the spot is located
  ## at x, y (mm) on the detector surface and shaped using a bivariate gaussian profile
  
  require(mvtnorm)
  
  # how many photons?
  if(lambda < 0) return(NULL) # for negative expected intensity, don't bother going any further
  n <- rpois(n = 1, lambda = lambda)  
  if(n < 1) return(NULL) # for zero or negative intensity this instance, don't bother going any further
  
  # calculate the variance-covariance matrix that determines the shape and tilt angle
  varcov <- matrix(data = c(sdx^2, rho*sdx*sdy, rho*sdx*sdy, sdy^2), ncol = 2, nrow = 2)
  
  # generate coordinates
  output<- rmvnorm(n, c(x, y), sigma = varcov)
  return(output)
}

