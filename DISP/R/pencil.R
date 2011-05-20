pencil <-
function(lambda = 100000, x = mean(detector$xDetRange), y = mean(detector$yDetRange)){

  ## creates a pencil beam of a mean value of lambda photons located at x,y mm
  
  # how many photons?  
  n <- rpois(n = 1, lambda = lambda)
  if(n == 0) return(NULL)
  
  # generate coordinates
  output<- cbind(rep(x, n), rep(y, n))
  return(output)  
}

