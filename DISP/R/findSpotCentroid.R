findSpotCentroid <-
function(imageMatrix, NX, NY, NRX, NRY, NC, xPx, yPx, messages = TRUE){

  ## given measurement box parameters, then calculates the centre of gravity of that box, after a background subtraction.
  ## The background is estimated in the same way as pxIntegrate

  # check input is kosher
  if(length(dim(imageMatrix)) != 2) stop("findSpotCentroid: the supplied imageMatrix is not a 2D matrix")
  
  # make sure measurement box has odd length sides (to ensure a central pixel)
  stopifnot(NX %% 2 == 1, NY %% 2 == 1)
  
  xSide <- floor(NX/2)
  ySide <- floor(NY/2)
  midX <- ceiling(NX / 2)
  midY <- ceiling(NY / 2)  
  numPix <- NX * NY
  
  # define spotmask and backmask using NX, NY, NRX, NRY and NC as in Mosflm to ensure mm symmetry
  spotMask <- .makeSpotMask(NX, NY, NRX, NRY, NC)
  backMask <- !spotMask  
  
  # number of peak and background pixels
  #nPeak <- length(spotMask[spotMask])
  #nBack <- length(spotMask[backMask])

  # take just that part of the image under the measurement box
  imageSlice <- imageMatrix[(xPx-xSide):(xPx + xSide), (yPx-ySide):(yPx+ySide)]  
  
  # determine the background plane and subtract it
  backplane <- .background(NX, NY, spotMask, imageSlice)
  imageSlice <- imageSlice - backplane
  
  #shift pixel values to make minimum zero (having negative 'masses' breaks COG calculation)
  imageSlice <- imageSlice - min(imageSlice)
  
  #obtain measurement box coordinates (p,q)
  p <- rep(1:NX, NY)
  dim(p) <- c(NX, NY)
  p <- p - midX
  q <- rep(1:NY, NX)
  dim(q) <- c(NY, NX)
  q <- t(q)
  q <- q - midY

  #calc centre of gravity in p and q
  cogX <- sum(p * imageSlice) / sum(imageSlice)
  cogY <- sum(q * imageSlice) / sum(imageSlice)
  
  if(messages) cat("findSpotCentroid: xPx = ", xPx + cogX," yPx = ", yPx + cogY,"\n")
  
  return(c(cogX,cogY))
}

