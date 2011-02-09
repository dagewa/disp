makePolyGrids <-
function(normaliseArray = TRUE, detectorFace = TRUE, CCD = TRUE,
                          xRange = detector$xDetRange, yRange = detector$yDetRange){
                         
  ## form the polygon grids of the CCD pseudopixels mapped back onto the detector surface back along the FOT
  ## (if normaliseArray = TRUE), the orthogonal grid at the detector face (if detectorFace = TRUE)
  ## and the orthogonal grid of CCD pixels (if CCD = TRUE). These can then be displayed with poly plot methods.
  ## This can be quite memory hungry for large detectors, so by default each is saved and removed from memory.
  ## The Range variables will throw away polygons that fall outside the intended range, useful to save memory
  
  time1 <- Sys.time()
  # some message here
  if (.Platform$OS.type == "windows") flush.console()
  
  if(xRange[1] < detector$xDetRange[1] || xRange[2] > detector$xDetRange[2]){
    stop("xRange outside detector$xDetRange")
  }
  if(yRange[1] < detector$yDetRange[1] || yRange[2] > detector$yDetRange[2]){
    stop("yRange outside detector$yDetRange")
  }  
  
  # make an array to hold the centroid positions of each CCD pixel
  centroid <- .findCentroids()
  
  undistGrid <- NULL
  detFaceGrid <- NULL
  CCDgrid <- NULL
  
  if(normaliseArray){
    centroidNormArray <- centroid
    # remove pixels that will be outside the limits when coords are transformed back up the taper
    lowEdge <- taper(cbind(xRange[[1]], yRange[[1]]) + 0.001)
    highEdge <- taper(cbind(xRange[[2]], yRange[[2]]) - 0.001)
    indexX <- centroidNormArray[,1] >= lowEdge[[1]] & centroidNormArray[,1] <= highEdge[[1]]
    indexY <- centroidNormArray[,2] >= lowEdge[[2]] & centroidNormArray[,2] <= highEdge[[2]]
    centroidNormArray <- subset(centroidNormArray, indexX & indexY)
    # for every CCD pixel, find its quadrilateral when 'undistorted' on to the face of the detector
    undistortPixelAsList <- function(coords){
      pts <- .undistortPixel(coords)
      pts$hole = FALSE
	  return(pts)
    }
    ptsList <- apply(centroidNormArray, MARGIN = 1, FUN = undistortPixelAsList)
    undistGrid <- new("gpc.poly", pts=ptsList)
    save(undistGrid,file="undistGrid.RData")
    rm(undistGrid)
  }
  
  if(detectorFace){
    # for every pseudo pixel on the detector face, find its x and y index
    rowIndex <- row(matrix(ncol = detector$yPxNum, nrow = detector$xPxNum))
    colIndex <- col(matrix(ncol = detector$yPxNum, nrow = detector$xPxNum))
    # replace these pixel indices with their pixel centroids
    origin <- c(detector$xDetRange[[1]], detector$yDetRange[[1]])    
    rowIndex <- (rowIndex - 0.5) * detector$xPxSizeFace + origin[1]
    colIndex <- (colIndex - 0.5) * detector$yPxSizeFace + origin[2]
    # make 2D matrix of centroids
    dim(rowIndex) <- NULL
    dim(colIndex) <- NULL
    centroidFace <- cbind(rowIndex, colIndex)
    # remove pixels outside the limits
    indexX <- centroidFace[,1] >= xRange[[1]] & centroidFace[,1] <= xRange[[2]]
    indexY <- centroidFace[,2] >= yRange[[1]] & centroidFace[,2] <= yRange[[2]]
    centroidFace <- subset(centroidFace, indexX & indexY)
    # function to expand each centroid to its pixel quadrilateral
    expandPixel <- function(coords){
      xc <- coords[[1]]
      yc <- coords[[2]]
      # calculate the vertices of the pixel at the face
      pt1 <- c(xc - detector$xPxSizeFace/2, yc - detector$yPxSizeFace/2)
      pt2 <- pt1 + c(0, detector$yPxSizeFace)
      pt3 <- pt2 + c(detector$xPxSizeFace, 0)
      pt4 <- pt3 + c(0, -detector$yPxSizeFace)
      pts <- rbind(pt1, pt2, pt3, pt4)
      return(list(x=pts[,1],y=pts[,2], hole=FALSE))    
    }
    ptsList <- apply(centroidFace, MARGIN = 1, FUN = expandPixel)
    detFaceGrid <- new("gpc.poly", pts=ptsList)
    save(detFaceGrid,file="detFaceGrid.RData")
    rm(detFaceGrid)    
  }
  
  if(CCD){
    # remove pixels outside the limits
    indexX <- centroid[,1] >= xRange[[1]] & centroid[,1] <= xRange[[2]]
    indexY <- centroid[,2] >= yRange[[1]] & centroid[,2] <= yRange[[2]]
    centroid <- subset(centroid, indexX & indexY)    
    # function to expand CCD centroids to pixel quadrilaterals
    expandPixel <- function(coords){
      xc <- coords[[1]]
      yc <- coords[[2]]
      # calculate the vertices of the CCD pixel
      pt1 <- c(xc - detector$xPxSize/2, yc - detector$yPxSize/2)
      pt2 <- pt1 + c(0, detector$yPxSize)
      pt3 <- pt2 + c(detector$xPxSize, 0)
      pt4 <- pt3 + c(0, -detector$yPxSize)
      pts <- rbind(pt1, pt2, pt3, pt4)
      return(list(x=pts[,1],y=pts[,2], hole=FALSE))    
    }
    ptsList <- apply(centroid, MARGIN = 1, FUN = expandPixel)
    CCDgrid <- new("gpc.poly", pts=ptsList)
    save(CCDgrid,file="CCDgrid.RData")
    rm(CCDgrid)      
  }
   
  # How long did it all take?
  time2 <- Sys.time()
  print(difftime(time2,time1))  
  
  return(NULL)
}

