findSpot <-
function(imageMatrix, startX = NULL, startY = NULL, searchBoxSize = 20, movAvLength = 15, display = TRUE, messages=TRUE){

  ## finds the strongest pixel on an image and fits a measurement box to it using an algorithm similar to
  ## Mosflm's (based on Lehman & Larsen, maximising I/sig(I))

  # check input is kosher
  if(length(dim(imageMatrix)) != 2) stop("findSpot: the supplied imageMatrix is not a 2D matrix")
  if(length(movAvLength) != 1) stop("findSpot: movAvLength is not a single number")
  
  # determine start and end points
  if(is.null(startX)){
    pts <- NULL
    cat("Please click on the spot\n")
    if (.Platform$OS.type == "windows") flush.console()
    try(pts <- locator(n=1, type="o", col="red"))
    if(is.null(pts)){
      stop("findSpot: no suitable position to start search from")
    }else{
      startX <- round(pts$x[[1]])
      startY <- round(pts$y[[1]])
    }
  }
  
  imSlice <- imageMatrix[(startX - searchBoxSize):(startX + searchBoxSize),
                         (startY - searchBoxSize):(startY + searchBoxSize)]
  maxPx <- which(imSlice == max(imSlice), arr.ind=TRUE)
  
  xPx <- startX - searchBoxSize - 1 + maxPx[[1]]
  yPx <- startY - searchBoxSize - 1 + maxPx[[2]]
  
  if(messages){
    print.noquote("The first occurrence of the most intense value is at pixel")
    print.noquote(paste("X =",xPx))
    print.noquote(paste("Y =",yPx))
  }

  # determine NX and NY around spot
  # first find background value by inspecting moving average values away from the peak where the moving
  # average is taken in windows either side of the centre of the peak
  xSlice <- vector(mode="numeric", length=movAvLength)
  ySlice <- vector(mode="numeric", length=movAvLength)
  for (i in 1:movAvLength){
    xSlice[i] <- mean(c(imageMatrix[(xPx + i):(xPx + i + 1), (yPx - 2):(yPx + 2)],
                        imageMatrix[(xPx - i - 1):(xPx - i), (yPx - 2):(yPx + 2)]))
    ySlice[i] <- mean(c(imageMatrix[(xPx - 2):(xPx + 2), (yPx + i):(yPx + i + 1)],
                        imageMatrix[(xPx - 2):(xPx + 2), (yPx - i - 1):(yPx - i)]))
  }
  minPxX <- which.min(xSlice)
  xBackBox <- c(imageMatrix[(xPx + minPxX):(xPx + minPxX + 2), (yPx - 2):(yPx + 2)],
                imageMatrix[(xPx - minPxX):(xPx - minPxX + 2), (yPx - 2):(yPx + 2)])
  xBackEst <- mean(xBackBox)
  xBackTolerance <- sqrt(var(xBackBox))
  
  minPxY <- which.min(ySlice)
  yBackBox <- c(imageMatrix[(xPx - 2):(xPx + 2), (yPx + minPxY):(yPx + minPxY + 2)],
                imageMatrix[(xPx - 2):(xPx + 2), (yPx - minPxY):(yPx - minPxY + 2)])
  yBackEst <- mean(yBackBox)
  yBackTolerance <- sqrt(var(yBackBox))                
  # set NX and NY to be where the moving average away from the peak is first within 1 sd of the  background estimate
  # with a safety rim of 4 pixels either side
  NXside <- match("TRUE", xSlice < xBackEst + xBackTolerance) + 4
  NX <- 2 * NXside + 1
  NYside <- match("TRUE", ySlice < yBackEst + yBackTolerance) + 4
  NY <- 2 * NYside + 1
  
  # now find the optimum measurement box using algorithm similar to Mosflm's
  optNRX <- NULL
  for (NRX in 1:((NX - 5)%/%2)){
    temp <- pxIntegrate(imageMatrix, "mosflm", NX, NY, NRX, 1, 1, xPx, yPx, bias = 0, gain = 1, messages = FALSE)
    optNRX <- rbind(optNRX, temp, deparse.level=0)
  }
  maxI <- max(optNRX[,1])
  SigIoverI <- sqrt(abs(optNRX[,2]))/optNRX[,1]
  NRX <- which.min(SigIoverI)
  
  while(optNRX[[NRX,1]] < 0.99 * maxI) NRX <- NRX - 1
  NRX <- NRX + 1 #while condition has overstepped NRX by one position, so put back
  
  optNRY <- NULL
  for (NRY in 1:((NY - 5)%/%2)){
    temp <- pxIntegrate(imageMatrix, "mosflm", NX, NY, NRX, NRY, 1, xPx, yPx, bias = 0, gain = 1, messages = FALSE)
    optNRY <- rbind(optNRY, temp, deparse.level=0)
  }
  maxI <- max(optNRY[,1])
  SigIoverI <- sqrt(abs(optNRY[,2])) / optNRY[,1]
  NRY <- which.min(SigIoverI)

  while(optNRY[[NRY,1]] < 0.99 * maxI) NRY <- NRY - 1
  NRY <- NRY + 1
  
  optNC  <- NULL
  for (NC in min(c(NRX, NRY)):min(c(NX-2, NY-2))){
    temp <- pxIntegrate(imageMatrix, "mosflm", NX, NY, NRX, NRY, NC, xPx, yPx, bias = 0, gain = 1, messages = FALSE)
    optNC <- rbind(optNC, temp, deparse.level=0)
  }
  maxI <- max(optNC[,1])
  SigIoverI <- sqrt(abs(optNC[,2])) / optNC[,1]
  index <- which.min(SigIoverI)
  NC <- index + min(c(NRX, NRY)) - 1
  if (NC < 0) NC <- 1
  
  while(optNC[[index,1]]< 0.99 * maxI & index > 0){
    NC <- NC - 1
    index <- index - 1
  }
  NC <- NC + 1
  
  if(display) dispBox(imageMatrix, NX, NY, NRX, NRY, NC, xPx, yPx)
  cat("suggested parameters for integration: NX=",NX,", NY=",NY,", NRX=",NRX,", NRY=",
       NRY,", NC=",NC,", xPx=",xPx,", yPx=",yPx,"\n", sep="")
  return(list(NX=NX, NY=NY, NRX=NRX, NRY=NRY, NC=NC, xPx=xPx,yPx=yPx))
}