makeUndistortArray <-
function(){

  ## 08/07/09 Changed to use apply(). Not much speed improvement though on single processor 
  ## (apply() internally uses a loop)

  ## 28/11/08 Changed to define the 1,1 pixel origin on the detector face at detector$xDetFace[1],
  ## detector$yDetFace[1]

  ## Creates an object with dimensions xPx, yPx in which at each pixel position is stored
  ## a list with two elements. The first element 'pixel' gives the pixel coordinate of the
  ## final image to which the centroid of the relevant pixel on the start image maps. The
  ## second element 'matrix' contains a 3 * 3 matrix containing the fractional overlap of
  ## the pixel on the start image with pixels in the vicinity of where its centroid mapped
  
  # This function depends on gpclib being loaded. Check this is so
  require(gpclib)
  
  # This may take some time, so keep track
  
  time1 <- Sys.time()
  print.noquote("making the undistortion arrays for current detector parameters")
  print.noquote("by default this array should be called undistortArray and stored in the list called processing")
  if (.Platform$OS.type == "windows") flush.console()
  
  # make an array to hold the centroid positions of each pixel
  centroid <- .findCentroids()
  
  #undistort the centroid array to the detector face
  undistCentroid <- .undistortPoints(centroid)
  
  # convert to whole pixel numbers on the detector face
  origin <- c(detector$xDetRange[[1]], detector$yDetRange[[1]])
  undistCentroid <- undistCentroid - origin
  undistCentroid[,1] <- ceiling(undistCentroid[,1] / detector$xPxSizeFace)
  undistCentroid[,2] <- ceiling(undistCentroid[,2] / detector$yPxSizeFace)
  
  # Store the information I need, that is the x,y coords of CCD pixel centroids and equivalent whole pixel
  # coordinates at the detector face, in rows of a matrix, to process with apply()
  coords <- cbind(centroid, undistCentroid)
   
  makeOut <- function(coords){
    
    # get the GPC poly of the CCD pixel transformed back to the detector face
    pts <- .undistortPixel(coords[1:2])
    quad <- as(pts, "gpc.poly")
    
    ## get the positions in mm of the vertices of the pixels of the 3*3 reapportioning matrix
    ## on the face of the detector:
    ## 
    ##   y4 +--+--+--+
    ##   y3 +--+--+--+
    ##   y2 +--+--+--+
    ##   y1 +--+--+--+
    ##      x1 x2 x3 x4
    
    x3 <- origin[1] + coords[[3]] * detector$xPxSizeFace
    y3 <- origin[2] + coords[[4]] * detector$yPxSizeFace

    x4 <- x3 + detector$xPxSizeFace
    y4 <- y3 + detector$yPxSizeFace

    x2 <- x3 - detector$xPxSizeFace
    y2 <- y3 - detector$yPxSizeFace

    x1 <- x2 - detector$xPxSizeFace
    y1 <- y2 - detector$yPxSizeFace

    ## make the pixel polygons, using this convention for numbering in the grid:
    ## 
    ##   3 6 9                   (3,1)(3,2)(3,3)
    ##   2 5 8   equivalent to   (2,1)(2,2)(2,3)
    ##   1 4 7                   (1,1)(1,2)(1,3)
    
    poly1 <- as(rbind(c(x1,y1),c(x2,y1),c(x2,y2),c(x1,y2)), "gpc.poly")
    poly2 <- as(rbind(c(x2,y1),c(x3,y1),c(x3,y2),c(x2,y2)), "gpc.poly")
    poly3 <- as(rbind(c(x3,y1),c(x4,y1),c(x4,y2),c(x3,y2)), "gpc.poly")
    poly4 <- as(rbind(c(x1,y2),c(x2,y2),c(x2,y3),c(x1,y3)), "gpc.poly")
    poly5 <- as(rbind(c(x2,y2),c(x3,y2),c(x3,y3),c(x2,y3)), "gpc.poly")
    poly6 <- as(rbind(c(x3,y2),c(x4,y2),c(x4,y3),c(x3,y3)), "gpc.poly")
    poly7 <- as(rbind(c(x1,y3),c(x2,y3),c(x2,y4),c(x1,y4)), "gpc.poly")
    poly8 <- as(rbind(c(x2,y3),c(x3,y3),c(x3,y4),c(x2,y4)), "gpc.poly")
    poly9 <- as(rbind(c(x3,y3),c(x4,y3),c(x4,y4),c(x3,y4)), "gpc.poly")

    # get area of undistorted pixel
    quad_area <- area.poly(quad)
    
    # get areas of all overlaps
    area1 <- area.poly(intersect(poly1,quad))
    area2 <- area.poly(intersect(poly2,quad))
    area3 <- area.poly(intersect(poly3,quad))
    area4 <- area.poly(intersect(poly4,quad))
    area5 <- area.poly(intersect(poly5,quad))
    area6 <- area.poly(intersect(poly6,quad))
    area7 <- area.poly(intersect(poly7,quad))
    area8 <- area.poly(intersect(poly8,quad))
    area9 <- area.poly(intersect(poly9,quad))
    
    # fill reapportioning matrix with fractional areas
    reapportioningMatrix <- matrix(data=0, ncol=3, nrow=3)
    reapportioningMatrix[1,1] <- area1 / quad_area
    reapportioningMatrix[2,1] <- area2 / quad_area
    reapportioningMatrix[3,1] <- area3 / quad_area
    reapportioningMatrix[1,2] <- area4 / quad_area
    reapportioningMatrix[2,2] <- area5 / quad_area
    reapportioningMatrix[3,2] <- area6 / quad_area
    reapportioningMatrix[1,3] <- area7 / quad_area
    reapportioningMatrix[2,3] <- area8 / quad_area
    reapportioningMatrix[3,3] <- area9 / quad_area
    
    # check the undistorted pixel is not bigger than the 3x3 matrix by ensuring the sum of areas = 1.
    # Here used 0.9999 for 1 to avoid rounding errors in the sum making the expression true 
    if (sum(reapportioningMatrix) < 0.9999){
      stop("CCD pixel with centroid",coords[1:2]," is bigger than the 3*3 final pixels it should map to")
    }    
    return(list(pixel = coords[3:4], matrix = reapportioningMatrix))
  }
  
  out <- apply(coords, MARGIN = 1, FUN = makeOut)
  dim(out) <- c(detector$xPxNum, detector$yPxNum)
  
  # How long did it all take?
  time2 <- Sys.time()
  print(difftime(time2,time1))

  return(out)
}

