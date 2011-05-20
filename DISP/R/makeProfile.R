makeProfile <-
function(imageSet, NX, NY, NRX, NRY, NC,
                 xPx = ceiling(NX/2), yPx = ceiling(NY/2), bias = processing$newBias){

  ## Takes a set of images and integration parameters and calculates a spot profile based on the mean
  ## This is not a robust profile forming algorithm like that of Mosflm!
  
  # check if imageSet is a 3D matrix, as expected
  if(length(dim(imageSet)) != 3) stop("pxMean: requires a 3D array as an imageSet")
  
  # make sure measurement box has odd length sides (to ensure a central pixel)
  stopifnot(NX %% 2 == 1, NY %% 2 == 1)
  xSide <- floor(NX/2)
  ySide <- floor(NY/2)
  
  numImages <- dim(imageSet)[[3]]
  
  # for each image in the set find the bias- and background-corrected spot profile
  profileSet <- array(data = 0, dim = c(NX, NY, numImages))
  for(n in 1:numImages){
    profileSet[,,n] <- pxIntegrate(imageSet[,,n], NX=NX, NY=NY, NRX=NRX, NRY=NRY, NC=NC,
                                   xPx=xPx, yPx=yPx, bias=bias, makeProfile=TRUE)
  }
  
  # take the mean of the profiles and round to nearest integer
  profile <- pxMean(profileSet)
  profile <- round(profile)
  return(profile)
}

