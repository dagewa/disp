read <-
function (imageMatrix, noise = detector$readNoise, gain = detector$ADCgain) {
  
  ## Simulates CCD read out, by adding noise taken from a normal distribution to a 
  ## matrix representing numbers of electrons in the CCD chip wells. The resulting
  ## numbers are converted to ADUs using the detector gain and a bias added to 
  ## ensure (hopefully) that the pixel values are positive.

  # check the input is kosher
  if(length(dim(imageMatrix)) != 2) stop("read: the supplied imageMatrix is not a 2D matrix")
  
  xPx <- dim(imageMatrix)[1]
  yPx <- dim(imageMatrix)[2]
  readNoise <- matrix (data = rnorm(n = xPx * yPx, sd = noise),
                       ncol = yPx, nrow = xPx)
  imageMatrix <- imageMatrix + readNoise + dark()
  
  # convert to ADUs and add bias
  imageMatrix <- round(imageMatrix * gain)
  imageMatrix <- imageMatrix + detector$bias
  
  # ensure the ADC dynamic range maximum is respected
  imageMatrix[imageMatrix >= 2^detector$ADCbits] <- 2^detector$ADCbits

  return(imageMatrix)
}

