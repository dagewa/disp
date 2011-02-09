flatten <-
function(imageSlice, VOMimage, corArray){

  ## Given an NX*NY section of an image and the corresponding NX*NY VoM array and NX*NY*n*n 4D corArray (ie. a local 
  ## n*n correlation array for each NX*NY pixels of an image), create the full 2D variance-covariance matrix

  # check input is kosher
  if(length(dim(imageSlice)) != 2) stop("flatten: the supplied imageSlice is not a 2D matrix")
  if(length(dim(VOMimage)) != 2) stop("flatten: the supplied VOMimage is not a 2D matrix")
  if(length(dim(corArray)) != 4) stop("flatten: the supplied corArray is not a 4D array")

  # get size of imageSlice
  xPx <- nrow(imageSlice)
  yPx <- ncol(imageSlice)
  # find extent of covariance matrix in depth of neighbouring pixels
  corMatrixSize <- (dim(corArray)[3] - 1) / 2
  # shorthand for cleaner looking code
  cms <- corMatrixSize
  
  # make index array
  index <- matrix(data = seq(1, xPx*yPx), ncol = yPx, nrow = xPx)
  
  # set up empty var-cov matrix
  varcov <- matrix(data = 0, ncol = xPx*yPx, nrow = xPx*yPx)
  
  #make the variance image
  varImage <- imageSlice * VOMimage
  
  #can't allow negative values for the variance, which could occur due to read noise
  varImage[varImage < 0] <- 0
  
  # go through every pixel 
  for(j in 1:yPx) for(i in 1:xPx){
    
    # find the relevant row of varcov
    pixelij <- index[i,j]
    
    # go through every stored correlation
    for(v in 1:(2*cms+1)) for(u in 1:(2*cms+1)){
      
      # find correct column to write into varcov
      p <- u - cms - 1
      q <- v - cms - 1
      
      pixeluv <- 0
      
      if(i+p >= 1 && j+q >= 1 && i+p <= xPx && j+q <= yPx) pixeluv <- index[[i+p,j+q]]

      # check pixeluv is not out of bounds then if okay, write the correlation between 
      # pixel i,j and u,v into the correct position in varcov and convert correlations to covariances
      if(pixeluv){
        varcov[pixelij, pixeluv] <- corArray[[i,j,u,v]] * sqrt(varImage[[i,j]] * varImage[[i+p,j+q]])
      }
    }  
  }
  return(varcov)
}

