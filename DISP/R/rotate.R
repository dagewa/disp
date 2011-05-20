rotate <-
function(imageMatrix){

  ## Rotates an image by 90 degrees
  ##

  # check the input is kosher
  if(length(dim(imageMatrix)) != 2) stop("rotate: the supplied imageMatrix is not a 2D matrix")
  
  trans <- t(imageMatrix)
  rows <- nrow(trans)
  out <- trans[rows:1,]
  return(out)
}

