exposeSpots <-
function(intensityList, ...){

  ## given a matrix with three columns, the first containing an integer intensity in Xph, the
  ## second an x position in mm and the third a y position in mm, this function goes through  
  ## each line in the input and exposes a spot of that intensity at x, y
  
  # check the input is kosher
  if(ncol(intensityList) != 3) stop("exposeSpots: the supplied intensityList does not have 3 columns")
  
  # May take some time, so keep track
  time1 <- Sys.time()
  
  cat("exposing an image containing spots located at positions and intensity described by the input\n")
  
  output <- accumulate() #return image of zeroes
  for (n in 1:nrow(intensityList)){
    if (.Platform$OS.type == "windows") flush.console()
    lambda = intensityList[n,1]
    if(lambda < 0) warning("intensityList element ",n," has supplied intensity less than zero")
    spotHere <- spot(lambda = lambda, x = intensityList[n,2],
                     y = intensityList[n,3], ...)
    temp <- expose(spotHere)
    output <- output + temp
  }
  
  # how long did it all take?
  time2 <- Sys.time()
  print(difftime(time2,time1))
  
  return(output)
}

