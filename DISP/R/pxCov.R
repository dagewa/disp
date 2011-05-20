pxCov <-
function (imageSet, covMatrixSize = 3){
  
  ## Takes a set of images and calculates covariances between all pixels and their 
  ## neighbours up to 3 deep.

  # check if imageSet is a 3D matrix, as expected
  if(length(dim(imageSet)) != 3) stop("pxCov: requires a 3D array as an imageSet")

  #use shorthand to make following code look cleaner
  cms <- covMatrixSize

  # get image size
  xPx <- dim(imageSet)[1]
  yPx <- dim(imageSet)[2]

  # setup the output array
  output <- array(data=NA, dim=c(xPx, yPx, 2*cms + 1, 2*cms + 1))
  
  # go through all image pixels
  for(j in 1:yPx) for(i in 1:xPx) {
  
    # go through every covariance to be calculated
    for(v in 1:(2*cms+1)) for(u in 1:(2*cms+1)){
      
      # check position relative to the centre of the covariance array
      p <- u - cms - 1
      q <- v - cms - 1
      
      # is this position in-bounds? If so calculate correlation between it and the i,j pixel
      if(i+p >= 1 && j+q >= 1 && i+p <= xPx && j+q <= yPx){
        output[i,j,u,v] <- suppressWarnings(cov(x = imageSet[i,j,], y = imageSet[i+p,j+q,],
                           use="complete.obs",method="pearson"))
      }
    } 
  }
  return(output)
}

