makeFlatfield <-
function(xRange, yRange, imageSet, norm = processing$normaliseArray, dark = processing$darkRaw){
  
  ## Normalises a uniform flood image to correct for the area distortion introduced by the FOT
  ##
  ## For uniformity correction of data images we need to know the non-uniformity of response
  ## across the detector surface. This is found by collecting uniform flood field images.
  ## The mean image is found then normalised by its own mean value so it can be used as a 
  ## scaling factor for data images in non-uniformity correction, as implemented by normaliseImage()
  ## This function is a step to completing the "processing" list for a detector

  # check the input is kosher
  if (length(xRange) != 2) stop("makeFlatfield: the supplied xRange does not have a length of 2")
  if (length(yRange) != 2) stop("makeFlatfield: the supplied yRange does not have a length of 2")
  if (typeof(norm) == "NULL") stop("makeFlatfield: the normaliseArray has not been properly set")
  if (typeof(dark) == "NULL") stop("makeFlatfield: the raw dark image has not been properly set")
  
  floodMean <- pxMean(imageSet)
  
  # remove the dark background, then apply the area correction
  floodMean <- floodMean - dark
  floodMean <- floodMean * norm
  
  # normalise by the mean flood value. Because of the FOT distortion, some pixels contain no
  # data originating from the phosphor. These should not contribute to the mean so we calculate
  # the mean value only in the central (exposed) portion of the image.
  meanValue <- mean(floodMean[seq(from = xRange[1], to = xRange[2]),seq(from = yRange[1], to = yRange[2])])
  floodMean <- (floodMean / meanValue)
  
  # pixels at the edges, where the FOT doesn't meet the chip, may have very low or zero values, 
  # which can cause infinities when dividing a data image by the flatfield. As a fix, set all 
  # values 0.8 > x > 1.2 equal to 1.0
  print.noquote(paste("maximum value in normalised flatfield is ",max(floodMean)))
  print.noquote(paste("minimum value in normalised flatfield is ",min(floodMean)))
  print.noquote(paste(length(floodMean[floodMean < 0.8]),"pixels with value < 0.8 and ",
                length(floodMean[floodMean > 1.2]),"pixels with value > 1.2 will be set to 1.0"))
  floodMean[floodMean < 0.8] <- 1.0
  floodMean[floodMean > 1.2] <- 1.0
  
  return(floodMean)
}

