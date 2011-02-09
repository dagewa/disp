makeProcessingList <-
function(xRange = NULL, yRange = NULL, floodRawFilePattern = NULL,
                                typeString = "edit as appropriate"){
  
  ## perform steps required to fill the processing list
  ##

  cat("detector:\n")
  str(detector)
  
  processing <- NULL
  processing$darkRaw <- read(dark())
  processing$normaliseArray <- makeNormaliseArray()
  processing$undistortArray <- makeUndistortArray()
  if(is.null(floodRawFilePattern)){ # if no raw flood images supplied, use perfect flatfield (just matrix of '1's)
    processing$flatfield <- matrix(data = 1, nrow = detector$xPxNum, ncol = detector$yPxNum)
  } else {
    rawFloodSet <- makeImageSet(string = floodRawFilePattern)
    processing$flatfield <- makeFlatfield(xRange = xRange, yRange = yRange, imageSet = rawFloodSet,
	                                      norm = processing$normaliseArray, dark = processing$darkRaw)   
  }
  processing$newBias <- 100
  processing$type <- typeString
  
  cat("you should now save the processing object to disk\n")
  return(processing)
}

