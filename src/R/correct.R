correct <-
function(imageMatrix, bias = processing$newBias){

  ## given a raw image, performs all corrections with the current detector setup
  ## to produce a final image
  
  # check inputs are kosher
  if(length(dim(imageMatrix)) != 2) stop("correct: the supplied imageMatrix is not a 2D matrix")  
  if (typeof(bias) == "NULL") stop("bias has not been properly set")
  
  # apply flatfield and geometric distortion corrections
  out <- normaliseImage(imageMatrix)
  out <- undistortImage(out)
  out <- out + bias
  
  # round to nearest integer and truncate to overload value
  out <- round(out)
  if (max(out >= 2^detector$ADCbits)) warning("Some pixels are outside the ADC dynamic range and will be rounded down")
  out[out >= 2^detector$ADCbits] <- 2^detector$ADCbits
  
  #check for negative valued pixels
  if (min(out) < 0) warning("Some pixels are negative valued")
  
  return(out)
}

