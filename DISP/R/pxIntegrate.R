pxIntegrate <-
function (imageMatrix, type = "mosflm", NX, NY, NRX, NRY, NC, xPx = ceiling(NX/2),
                         yPx = ceiling(NY/2), bias = processing$newBias, gain = NULL,
                         profile = NULL, #if passed, performs profile fitting integration as well as summ. int.
                         corArray = NULL, VOMimage = NULL, #these arguments used if type = "corArray"
                         covArray = NULL, #this argument used if type = "covArray"
                         Mobs = NULL,  #this argument used if type = "full"
                         makeProfile = FALSE, #used if called from makeProfile()
                         messages = TRUE){

  ## Performs  integration of a region of imageMatrix defined by placing the
  ## origin of a logical matrix spotMask at pixel xPx, yPx of imageMatrix. Values of
  ## 'TRUE' in spotMask correspond to pixels in the peak region. The parameters NX, NY,
  ## NRX, NRY and NC define a measurement box with mm symmetry, in the same way as
  ## Mosflm. Following Mosflm conventions, spotMask has sides of odd length and
  ## coordinates (p,q) refer to a position w.r.t the centre of the box.
  ##
  ## The standard error for summation integration is calculated by summing the covariances between pixels,
  ## treating peak and background pixels separately. In the case of "mosflm" type integration, Mobs
  ## is a diagonal matrix consisting only of variances calculated assuming Poission statistics
  ## (the expression for error then coincides with equation 11 in A. Leslie's 1999 Mosflm paper).
  ## As well as assuming Poisson statistics, the variance estimates are  also calculated 
  ## using the cascade and pixel noise model in a separate calculation.
  ## For the other types of integration some off-diagonal covariance elements are also included.
  ##
  ## If a spot profile is passed, pxIntegrate also performs profile fitting integration by least squares minimisation
  ## The full variance-covariance array (in units of ADU^2) for the measurement box is either passed in as Mobs (type = "full"),
  ## expanded from a precalculated covArray in the compressed form (type = "covArray")
  ## or calculated using Mosflm assumptions of Poisson statistics (type = "mosflm"), or calculated using a corArray, the image
  ## values and a VOMimage (option type = "corArray").
  
  # check input is kosher
  if(length(dim(imageMatrix)) != 2) stop("pxIntegrate: the supplied imageMatrix is not a 2D matrix")
  if (typeof(bias) == "NULL") stop("pxIntegrate: the bias has not been properly set. Check the 'processing' list")
  stopifnot(length(type) == 1, length(NX) == 1, length(NY) == 1, length(NRX) == 1, length(NRY) == 1,
            length(NC) == 1, length(xPx) == 1, length(yPx) == 1, length(bias) == 1)
  if (typeof(profile) != "NULL"){
    if(ncol(profile) != NY) stop("pxIntegrate: the profile does not match NY")
    if(nrow(profile) != NX) stop("pxIntegrate: the profile does not match NX")    
  }

  # make sure measurement box has odd length sides (to ensure a central pixel)
  stopifnot(NX %% 2 == 1, NY %% 2 == 1)
  xSide <- floor(NX/2)
  ySide <- floor(NY/2)
  numPix <- NX * NY
  
  # calculate the detector gain (#ADUs per *absorbed* Xph) if not passed in
  if (is.null(gain))  gain <- detector$phosphorAmplification * detector$transmission  * detector$ADCgain
  
  # define spotmask and backmask using NX, NY, NRX, NRY and NC as in Mosflm to ensure mm symmetry
  spotMask <- .makeSpotMask(NX, NY, NRX, NRY, NC)
  backMask <- !spotMask  
  
  # number of peak and background pixels
  nPeak <- length(spotMask[spotMask])
  nBack <- length(spotMask[backMask])

  # take just that part of the image under the measurement box
  imageSlice <- imageMatrix[(xPx-xSide):(xPx + xSide), (yPx-ySide):(yPx+ySide)]

  # remove offset. 
  imageSlice <- imageSlice - bias
  
  # determine the background plane and the total background under peak
  backplane <- .background(NX, NY, spotMask, imageSlice)
  peakBack <- sum(backplane[spotMask])
  if(messages) cat("pxIntegrate: after bias subtraction, average determined background level under peak region is",
                   peakBack/nPeak,"\n\n")

  # if pxIntegrate is being called from makeProfile, just return the bias and background corrected spot profile now
  if(makeProfile) return(imageSlice - backplane)
  
  # Negative errors are not allowed, but subtracting the bias may have introduced
  # negative balues in the image, which could propagate to the errors, so make a copy
  # truncated at zero for error estimation  
  imageSlicePos <- imageSlice
  imageSlicePos[imageSlicePos < 0] <- 0
  
  #determine the method for error estimation and make full variance-covariance matrices, Mobs, in units of ADU^2
  typeCase <- pmatch(type, c("mosflm", "covArray", "corArray", "full"))
  if(is.na(typeCase)) stop("Bad parameter for type") # stop if doesn't match
  if(typeCase == 1){ #mosflm
    Mobs <- gain * diag(imageSlicePos[1:(NX*NY)])
  }
  if(typeCase == 2) { #covArray
    if (typeof(covArray) == "NULL") stop("pxIntegrate: No covArray supplied")    
    dummy <- matrix(data = 1, ncol = NY, nrow = NX)
    Mobs <- flatten(dummy, dummy, covArray[(xPx-xSide):(xPx + xSide), (yPx-ySide):(yPx+ySide),,])
  }
  if(typeCase == 3) { #corArray
    if (typeof(corArray) == "NULL") stop("pxIntegrate: No corArray supplied")  
    if (typeof(VOMimage) == "NULL") stop("pxIntegrate: No VOMimage supplied to accompany corArray")
    Mobs <- flatten(imageSlicePos,  VOMimage[(xPx-xSide):(xPx + xSide), (yPx-ySide):(yPx+ySide)],
                    corArray[(xPx-xSide):(xPx + xSide), (yPx-ySide):(yPx+ySide),,])
  }
  if(typeCase == 4){ #full
    if (typeof(Mobs) == "NULL") stop("pxIntegrate: No Mobs array supplied")  
  }
  
  ## *Summation integration*
  ##

  peakTotal <- sum(imageSlice[spotMask])
  Is <- peakTotal - peakBack
  if(messages) cat("pxIntegrate: summation integrated intensity in ADUs is", Is, "\n\n")
  
  ## Standard error calculation - summation integration
  ##

  # Calculate covariances between pixels only in the peak by copying the complete variance-covariance
  # matrix then deleting rows and columns corresponding to background pixels
  peakMobs <- Mobs
  for (i in seq_along(backMask)) if(backMask[i]){
    peakMobs[i,]<-0
    peakMobs[,i]<-0
  }
  
  # and vice versa for background pixels
  backMobs <- Mobs
  for (i in seq_along(spotMask)) if(spotMask[i]){
    backMobs[i,]<-0
    backMobs[,i]<-0
  }  

  # Sum of covariances for background and peak regions
  backSumCov <- sum(backMobs)
  backSumCov <- (nPeak / nBack)^2 * backSumCov
  peakSumCov <- sum(peakMobs)
  
  varIs <- peakSumCov + backSumCov
  
  ## Standard error calculation - summation integration with cascade model for detector
  ##
  
  varIsCascade <- NA #Give varIsCascade a return value now in case it is not being calculated
  #Do we inflate the variance by the non-Poisson detector error? Should only do this if type = "mosflm" because
  #supplying a covArray, corArray or full Mobs array implies that covariances have been calculated using a large
  #set of suitable replicate images, thus already incorporate the true detector response  
  if(typeCase == 1){ #mosflm
    cascadeFactor <- 1 +
       (detector$phosphorNoise/detector$phosphorAmplification)^2 + 
       (1 - detector$transmission)/(detector$phosphorAmplification*detector$transmission)
    MobsCascade <- Mobs * cascadeFactor
    # add in read noise and digitisation error contribution to variance in ADU^2 for each pixel
    pixelNoise <-  (detector$readNoise)^2 + 1/(12*detector$ADCgain^2)
    pixelNoise <- pixelNoise / (detector$phosphorAmplification*detector$transmission)^2
    diag(MobsCascade) <- diag(MobsCascade) + pixelNoise

    # Calculate covariances between pixels only in the peak by copying the complete variance-covariance
    # matrix then deleting rows and columns corresponding to background pixels
    peakMobsCascade <- MobsCascade
    for (i in seq_along(backMask)) if(backMask[i]){
      peakMobsCascade[i,]<-0
      peakMobsCascade[,i]<-0
    }

    # and vice versa for background pixels
    backMobsCascade <- MobsCascade
    for (i in seq_along(spotMask)) if(spotMask[i]){
      backMobsCascade[i,]<-0
      backMobsCascade[,i]<-0
    }  

    # Sum of covariances for background and peak regions
    backSumCovCascade <- sum(backMobsCascade)
    backSumCovCascade <- (nPeak / nBack)^2 * backSumCovCascade
    peakSumCovCascade <- sum(peakMobsCascade)

    varIsCascade <- peakSumCovCascade + backSumCovCascade
  }
  
  if(messages){
    cat("pxIntegrate: variance of peak in ADU^2 using",type,"method is", peakSumCov,"\n")
    cat("pxIntegrate: variance of background under peak in ADU^2 using",type,"method is", backSumCov,"\n")
    cat("pxIntegrate: total variance in ADU^2 using",type,"method is", varIs,"\n\n")
    if(typeCase == 1){ #mosflm
      cat("pxIntegrate: Poisson excess 'cascadeFactor' for this detector is", cascadeFactor,"\n")
      cat("pxIntegrate: read and digitisation noise contribution to variance per pixel is", pixelNoise, "ADU^2\n")    
      cat("pxIntegrate: variance of peak in ADU^2 inflating the error due to the cascade and pixel noise model is", peakSumCovCascade,"\n")
      cat("pxIntegrate: variance of background under peak in ADU^2 inflating the error due to the cascade and pixel noise model is", backSumCovCascade,"\n")
      cat("pxIntegrate: total variance in ADU^2 inflating the error due to the cascade and pixel noise model is", varIsCascade,"\n\n")      
    }
  }
  
  # Give Ip and varIp return values now in case profile fitting is not being performed
  Ip <- NA
  varIp <- NA
  
  ## *Profile fitting integration*
  ##
  if (typeof(profile) != "NULL"){    
    # if estimating Mobs from the pixel values ("mosflm" type), calculate using the supplied profile,
    # rather than from each pixel value as used in summation integration
    if(typeCase == 1){ #mosflm
      # determine scale factor J (see equation 25 in AL's paper)
      profSum <- sum(profile)
      J <- Is / profSum
      
      # make the variance image (see equation 24 in AL's paper). This variance assumes Poisson statistics
      # and is in units of ADU^2
      varImage <- (backplane + J * profile)
      # we shouldn't have negative variance, but this is possible if background is very low because some
      # of the profile values can be negative
      if(min(varImage) < 0) {
        cat("pxIntegrate WARNING: the background appears to be very low leading to some negative",
        "values in the calculation of pixel variance for profile fitting, assuming Poisson stats.\n")
        print.noquote(paste("min(varImage) =",min(varImage)))
      }
      
      # expand varImage to the full variance matrix Mobs
      index <- matrix(data = seq(1, numPix), ncol = NY, nrow = NX)  
      Mobs <- matrix(data = 0, ncol = numPix, nrow = numPix)
      for(j in 1:NY) for(i in 1:NX){
        # find the relevant row and column of varcov and write variance value in
        pixelij <- index[i,j]
        Mobs[pixelij, pixelij] <- varImage[i,j]
      }      
    }
    
    #find the weights matrix, Mobs^-1
    MobsInv <- solve(Mobs)

    #make vector of observed values
    Obs <- imageSlice
    dim(Obs) <- NULL

    #make matrices p and q of co-ordinates in the measurement box
    midX <- xSide + 1
    midY <- ySide + 1
    p <- rep(1:NX, NY)
    dim(p) <- c(NX, NY)
    p <- p - midX
    q <- rep(1:NY, NX)
    dim(q) <- c(NY, NX)
    q <- t(q)
    q <- q - midY

    # make the design matrix A
    A <- cbind(profile[1:numPix], p[1:numPix], q[1:numPix], rep(1, numPix))

    # form the Normal Equations
    B <- t(A) %*% MobsInv %*% A
    D <- t(A) %*% MobsInv %*% Obs

    # solve for Xhat, the best estimate of the parameters K, a, b and c from least squares fitting
    Binv <- solve(B)
    Xhat <- Binv %*% D
    K <- Xhat[1]

    if(messages) cat("pxIntegrate: Best estimate of parameters vector Xhat (K, a, b, c) =",Xhat[1],Xhat[2],Xhat[3],Xhat[4],"\n")

    # Calculate profile fitted intensity
    Ip <- K * sum(profile)

    # if estimating Mobs, get estimate of the scale factor for var-cov matrix for 
    # parameters, Mx, and observations, Mobs
    Kv <- 1
    if(typeCase == 1){ #mosflm
      V <- Obs - A %*% Xhat
      Kv <- t(V) %*% MobsInv %*% V / (numPix - 4)
      dim(Kv) <- NULL
      if(messages) cat("pxIntegrate: Kv scale factor = ",Kv,"\n")
    }
    
    # find the var-cov matrix for parameters, Mx
    Mx <- Kv * Binv

    varIp <- Mx[1,1] * sum(profile)^2

    if(messages){
      cat("pxIntegrate: profile fitting integrated intensity in ADU is =", Ip,"\n\n")
      cat("pxIntegrate: profile fitting variance estimate in ADU^2 is =", varIp,"\n")
    }           
  }
  if(messages) cat("pxIntegrate: return values: Is, varIs, varIsCascade, Ip, varIp\n")
  return(c(Is, varIs, varIsCascade, Ip, varIp))
}

