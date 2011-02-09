flipHorizontal <-
function(imageMatrix){
  
  ## Flips an image around the Y axis
  ##
  
  nr <- nrow(imageMatrix)
  return(imageMatrix[nr:1,])
}

