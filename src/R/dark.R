dark <-
function(accumulationTime = detector$darkAccumulationTime, darkRate = detector$darkRate){

  ## creates a matrix for thermally generated electrons in CCD pixels
  ##
  
  matrix(data = rpois(n = detector$xPxNum * detector$yPxNum,
         lambda = darkRate * accumulationTime),
         ncol = detector$yPxNum, nrow = detector$xPxNum)
}

