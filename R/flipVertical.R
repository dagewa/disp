flipVertical <-
function(imageMatrix){
  
  ## Flips an image around the X axis
  ##
  
  nc <- ncol(imageMatrix)
  return(imageMatrix[,nc:1])  
}

