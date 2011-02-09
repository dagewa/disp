pxVar <-
function (imageSet){

  ## Takes a set of images and calculates the pixel by pixel variance, returning it
  ## as the 'variance image' of the set
  
  # check if imageSet is a 3D matrix, as expected
  if(length(dim(imageSet)) != 3) stop("pxVar: requires a 3D array as an imageSet")
  
  # get image size
  xPx <- dim(imageSet)[1]
  yPx <- dim(imageSet)[2]
  
  # setup the output array
  output <- array(data = NA, dim = c(xPx, yPx))
  
  # go through all pixels
  for(j in 1:yPx) for(i in 1:xPx){
    output[i,j] <- suppressWarnings(var(x = imageSet[i,j,], use="all.obs"))
  }
  return(output)
}

