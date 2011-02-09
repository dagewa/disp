makeOverlapArray <-
function(){

  require(gpclib)

  # This may take some time, so keep track 
  time1 <- Sys.time()
  print.noquote("making the overlap array for current detector parameters")
  print.noquote("by default this array should be called overlapArray and stored in the list called processing")
  if (.Platform$OS.type == "windows") flush.console()
  
  # find the centroids of all the virtual pixels at the detector face
  centroidX <- rep(seq(from = detector$xDetRange[[1]] + 0.5 * detector$xPxSizeFace,
                         to = detector$xDetRange[[2]] - 0.5 * detector$xPxSizeFace,
                         by = detector$xPxSizeFace), times = detector$yPxNum)
  dim(centroidX) <- c(detector$xPxNum, detector$yPxNum)
  
  centroidY <- rep(seq(from = detector$yDetRange[[1]] + 0.5 * detector$yPxSizeFace,
                         to = detector$yDetRange[[2]] - 0.5 * detector$yPxSizeFace,
                         by = detector$xPxSizeFace), times = detector$xPxNum)
  dim(centroidY) <- c(detector$yPxNum, detector$xPxNum)
  centroidY <- t(centroidY)
  
  # distort the positions to their equivalent at the CCD by mapping through the taper
  distCentroids <- .distortPoints(matrix(data=c(centroidX, centroidY), ncol=2))
 
  #convert the coordinates to enclosing pixels of the CCD chip
  mappedPixelX <- distCentroids[, 1] - detector$ccdX
  mappedPixelX <- mappedPixelX/detector$xPxSize
  mappedPixelX <- ceiling(mappedPixelX)
  dim(mappedPixelX) <- c(detector$xPxNum, detector$yPxNum)

  mappedPixelY <- distCentroids[, 2] - detector$ccdY
  mappedPixelY <- mappedPixelY/detector$yPxSize
  mappedPixelY <- ceiling(mappedPixelY)
  dim(mappedPixelY) <- c(detector$xPxNum, detector$yPxNum)
  
  output <- vector(mode="list", length = detector$xPxNum * detector$yPxNum)
  dim(output) <- c(detector$xPxNum, detector$yPxNum)
  
  # for every virtual pixel at the detector face
  for (j in 1:detector$yPxNum) for (i in 1:detector$xPxNum){
  
    # prepare the 3*3 matrix in the output element
    output[[i, j]] <- matrix(data=0, ncol=3, nrow=3)
    
    # get the GPC poly of the virtual pixel i,j
    centroid <- c(centroidX[[i,j]], centroidY[[i,j]])
    pts <- cbind(centroid[[1]] + c(-0.5, 0.5, 0.5, -0.5) * detector$xPxSizeFace,
                 centroid[[2]] + c(-0.5, -0.5, 0.5, 0.5) * detector$yPxSizeFace)
    virtualPixel <- as(pts, "gpc.poly")
  
    #get coordinates of the cluster of CCD pixels that may overlap
    seqx <- -1:1 + mappedPixelX[[i,j]]
    seqx <- seqx[seqx >= 1 & seqx <= detector$xPxNum]
    seqy <- -1:1 + mappedPixelY[[i,j]]
    seqy <- seqy[seqy >= 1 & seqy <= detector$yPxNum]
    
    #for each of these CCD pixels that potentially overlap virtual pixel i,j
    for (y in seqy) for (x in seqx){
   
      # get the GPC poly of the CCD pixel transformed back to the detector face
      xc <- detector$ccdX + (x - 0.5) * detector$xPxSize
      yc <- detector$ccdY + (y - 0.5) * detector$yPxSize
      pts <- .undistortPixel(c(xc,yc))
      undistortedCCDpixel <- as(pts, "gpc.poly")

      # we want to know what fraction of the virtual pixel i,j area is overlapped by this CCD pixel
      overlap <- area.poly(intersect(virtualPixel, undistortedCCDpixel)) / area.poly(virtualPixel)
    
      # we also want to know what fractions of the undistorted CCD pixel area overlap the final image
      # pixels in the local vicinity. This is precalculated for the distortion correction, in undistortArray
      info <- processing$undistortArray[[x, y]]
      
      # info$matrix is centred on info$pixel. We need to recentre the matrix on the pixel i,j if this is not
      # equal to info$pixel
      mat <- matrix(data = 0, nrow=7, ncol=7)
      shift <- info$pixel - c(i,j)
      mat[3:5 + shift[[1]], 3:5 + shift[[2]]] <- info$matrix
      mat <- mat[3:5, 3:5]
      
      # how much is signal spread from virtual pixel area i,j through this particular overlap to the pixels in 
      # the vicinity of i,j? The total signal that is to be spread is equal to the signal in the region
      # i,j multiplied by the fraction of i,j that overlaps CCD pixel x,y. That is the quantity defined
      # as 'overlap' above. This signal is then distributed amongst the local pixels on the final image
      # according to the fractions in mat.
      output[[i, j]] <- output[[i, j]] + overlap * mat
    }
  }
  # How long did it all take?
  time2 <- Sys.time()
  print(difftime(time2,time1))
  return(output)
}
