makeNormaliseArray <-
function(){
  
  ## Creates a matrix the same size as an image containing the ratio of each undistorted pixel's area
  ## with the mean undistorted pixel area, so that a uniform flood field image can be normalised by 
  ## makeFlatfield(). Used to fill in the "processing" list for a detector
  
  # May take some time, so keep track
  time1 <- Sys.time()
  print.noquote("making the area normalisation array for floodfield correction using current detector parameters")
  print.noquote("by default this array should be called normaliseArray and stored in the list called processing")
  if (.Platform$OS.type == "windows") flush.console()
  
  # make an array to hold the centroid positions of each pixel
  centroid<-.findCentroids()
  
  makeOut <- function(coords){
    # for the CCD pixel with these coords, calculate its area when 'undistorted' into a
    # quadrilateral on the face of the detector
    pts <- .undistortPixel(coords[1:2])
    quad <- as(pts, "gpc.poly")
    out <- area.poly(quad)
    return(out)
  }
  
  out <- apply(centroid, MARGIN = 1, FUN = makeOut)
  dim(out) <- c(detector$xPxNum, detector$yPxNum)
  
  meanArea <- mean(out)
  
  # output the inverse ratio of each pixel's area to the mean area
  out <- meanArea / out
  
  time2 <- Sys.time()
  print(difftime(time2,time1))
  
  return(out)
}

