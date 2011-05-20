batchGenerate <-
function(start = 1, end = 10, directory = "./data/temp/",
                          prefix = "spot",
                          spotLambda = NULL,
                          spotx = mean(detector$xDetRange),
                          spoty = mean(detector$yDetRange),
                          spotsdx = 0.1, spotsdy = 0.1, spotrho = 0.3,
                          floodFlux = NULL,
                          floodExtent = NULL, # size to generate flood around spot
                          correct = FALSE,
                          saveRawRegionX = NULL,
                          saveRawRegionY = NULL,
                          saveCorrectedRegionX = NULL,
                          saveCorrectedRegionY = NULL,                                                    
                          messages = TRUE){

  ## A simple function to control generation of large numbers of replicate images which are written to disk

  # may take a while, so keep track of time
  time1 <- Sys.time()
  
  #get empty image
  blank <- accumulate()
  
  #define flood generation limits
  if(is.null(floodExtent)){
    floodx <- detector$xDetRange
    floody <- detector$yDetRange
  } else {
    floodx <- c(spotx - floodExtent, spotx + floodExtent)
    floody <- c(spoty - floodExtent, spoty + floodExtent)
  }
  
  #define region for image write out 
  if(is.null(saveRawRegionX)) saveRawRegionX <- c(1, dim(blank)[1])
  if(is.null(saveRawRegionY)) saveRawRegionY <- c(1, dim(blank)[2])  
  if(is.null(saveCorrectedRegionX)) saveCorrectedRegionX <- c(1, dim(blank)[1])
  if(is.null(saveCorrectedRegionY)) saveCorrectedRegionY <- c(1, dim(blank)[2])
  
  for(i in start:end){
    if (.Platform$OS.type == "windows") flush.console()
    
    j <- sprintf("%05d",i)
    filenameRaw <- paste(prefix, "Raw", j, sep="")
    filenameCorrected <- paste(prefix, "Corrected", j, sep="")
    if(messages)   print.noquote(paste("generating image", i))
    
    temp <- blank
    if(!is.null(spotLambda)) temp <- temp + expose(spot(spotLambda, spotx, spoty, spotsdx, spotsdy, spotrho))
    if(!is.null(floodFlux)) temp <- temp + exposeFlood(floodFlux, xRange = floodx, yRange = floody) 
    raw <- read(temp)
    if(correct) corrected <- correct(raw)
    
    #write out images    
    writeImage(raw[saveRawRegionX[[1]]:saveRawRegionX[[2]], saveRawRegionY[[1]]:saveRawRegionY[[2]]],
      file=paste(directory, filenameRaw, sep="/"))
    if(correct) writeImage(corrected[saveCorrectedRegionX[[1]]:saveCorrectedRegionX[[2]],
        saveCorrectedRegionY[[1]]:saveCorrectedRegionY[[2]]],
      file=paste(directory, filenameCorrected, sep="/"))
  }
  
  time2 <- Sys.time()
  if(messages) print.noquote(difftime(time2,time1))
  
  return()
}

