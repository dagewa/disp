makeIntensityList <-
function(mtz, batch = 1, numXDET = 4096){
   
   ## takes a data frame read by readMTZ, selects one batch, finds columns containing I, XDET and YDET values, 
   ## and converts to format suitable for exposeSpots.
   ## yPx = numXDET + 1 - XDET
   ## xPx = YDET
   
  mtz <- subset(mtz, mtz$BATCH == batch)
  output <- cbind(mtz$I,mtz$YDET,mtz$XDET) # because XDET is along yPx, YDET is along xPx
  output[,3] <- numXDET + 1 - output[,3] # because yPx is antiparallel to XDET
  
  # convert to mm at centre of each pixel
  output[,2] <- (output[,2] - 0.5) * detector$xPxSizeFace
  output[,3] <- (output[,3] - 0.5) * detector$yPxSizeFace
  
  # truncate negative intensities
  output[,1][output[,1] < 0] <- 0
  
  return(output)
}

