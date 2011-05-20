makeVOMimage <-
function(imageSet, bias = processing$newBias){

  ## Creates a variance/mean image for the image set defined by the pattern. Because
  ## the mean is affected by the bias offset (whereas the variance isn't) the bias
  ## offset is subtracted from the mean image before dividing the variance image

  if (typeof(bias) == "NULL") stop("the bias has not been properly set")
  
  # check if imageSet is a 3D matrix, as expected
  if(length(dim(imageSet)) != 3) stop("makeVOMimage: requires a 3D array as an imageSet")
  
  v <- pxVar(imageSet)
  m <- pxMean(imageSet)
  
  # correct mean for bias offset
  m <- m - bias
  
  #ensure no Inf
  output<- v/m
  output[is.infinite(output)]<-NaN
  
  return(output)
}

