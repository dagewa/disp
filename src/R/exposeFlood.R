exposeFlood <-
function(flux = 20000, block = 500000, tempName = "exposeFloodTemp",
                         xRange = detector$xDetRange, yRange = detector$yDetRange, partialImage = NULL){

  ## Creates a raw flood image with an expected number of 'flux' Xph / pixel.
  ## NB a good flood image for flatfield correction should have many Xph/pixel to
  ## ensure good even coverage of the detector's phosphor surface. Poisson
  ## statistics dictate that 20'000 Xph/pixel => shot noise of 0.7%
  
  ## 27/11/08. No longer read()s the image, this has to be done separately afterwards!
  
  ## If there are errors such as "cannot allocate vector of size..." try
  ## decreasing the value for 'block'
  
  ## For long exposures, keep track of how many cycles have been completed and store
  ## it as an attribute, so we can carry on from where we left off if necessary.
  
  # May take some time, so keep track
  time1 <- Sys.time()
  
  # check inputs are kosher
  stopifnot(length(flux) == 1, length(block) == 1, length(xRange) == 2, length(yRange) == 2)
  
  # make temporary file with unique name
  tempExt <- paste(Sys.info()[[4]], "_", Sys.getpid(), proc.time()[[3]], sep="")
  tempExt <- gsub('[[:space:].]', '', tempExt)
  tempName <- paste(tempName, tempExt, sep="")
  tempName <- paste(tempName, ".RData", sep="")
  fileWritten <- FALSE

  # how many photons over whole exposed region?
  xPx <- (xRange[2] - xRange[1]) / detector$xPxSizeFace
  yPx <- (yRange[2] - yRange[1]) / detector$yPxSizeFace
  expected <- flux * xPx * yPx
  
  #print.noquote(paste("exposing a flood image with an expected value of",
  #              expected / (xPx * yPx),"Xph per pixel"))
                
  # to keep the data frame of Xph positions at a max of 800'000 entries, perform
  # the exposure in stages
  cycles <- expected %/% block
  
  # report messages now instead of waiting for .buffered output
  if (.Platform$OS.type == "windows") flush.console() 
  
  # Are we restarting from an interrupted run?
  if (is.null(partialImage)){
    # do the part that remains after all cycles
    remainder <- expected - (cycles * block)
    floodRaw <- expose(flood(lambda = remainder, xSize = xRange, ySize = yRange))
    attr(floodRaw, "cycle") <- 0
    start <- 1
  } else {
    floodRaw <- partialImage
    start <- attr(floodRaw, "cycle")
  }
  
  # now do the cycles
  if (cycles > 0) for(i in start:cycles){
    time2 <- Sys.time()
    print.noquote(paste(format(difftime(time2,time1)),"exposeFlood: cycle number",i,"of",cycles))
    if (.Platform$OS.type == "windows") flush.console()
    temp <- expose(flood(lambda = block, xSize = xRange, ySize = yRange))
    floodRaw <- floodRaw + temp
    attr(floodRaw, "cycle") <- i
    save(floodRaw, file = tempName)
    fileWritten <- TRUE
  }
  
  # now finished, strip the "cycle" attribute
  attr(floodRaw, "cycle") <- NULL
  
  # how long did it all take?
  time2 <- Sys.time()
  print(difftime(time2,time1))

  # remove temporary file if it was made
  if(fileWritten) file.remove(tempName)
  
  return(floodRaw)
}

