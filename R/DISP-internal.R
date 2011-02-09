.background <-
function(NX, NY, spotMask, imageSlice){
  
  ## Fits a plane described by ap + bq + c to the background intensities in imageSlice
  ## (p,q) are pixel coordinates within the measurement box where (0,0) is the centre.
  ## Function is hidden by the period convention (ls() will not list it by default)
  
  midX <- ceiling(NX / 2)
  midY <- ceiling(NY / 2)
  
  # find elts of 3*3 matrix used to solve background plane constants
  # see Rossmann J. Appl. Cryst. 1979 12, 230
  p <- rep(1:NX, NY)
  dim(p) <- c(NX, NY)
  p <- p - midX
  q <- rep(1:NY, NX)
  dim(q) <- c(NY, NX)
  q <- t(q)
  q <- q - midY
  background <- !spotMask
  pMask <- background * p
  qMask <- background * q
  pS <- sum(pMask)
  qS <- sum(qMask)
  ppS <- sum(pMask * pMask)
  qqS <- sum(qMask * qMask)
  pqS <- sum(pMask * qMask)
  pRhoS <- sum(pMask * imageSlice)
  qRhoS <- sum(qMask * imageSlice)
  RhoS <- sum(background * imageSlice)
  n <- sum(background)
  
  # construct and solve a system of linear equations
  A <- matrix(data = c(ppS, pqS, pS, pqS, qqS, qS, pS, qS, n),
              ncol = 3, nrow = 3)
  B <- c(pRhoS, qRhoS, RhoS)
  plane <- solve(A, B)
  a <- plane[1]
  b <- plane[2]
  c <- plane[3]

  return(a * p + b * q + c)
}

.distortPoints <-
function(input){

  ## takes input X,Y positions in mm (where 0,0 mm is the bottom left corner, 
  ## when viewed with disp(), of the module) and distorts them to positions
  ## on the small end of the FOT, by performing the taper distortion
  
  # check input is kosher
  if (is.null(ncol(input)) && length(input) == 2) dim(input) <- c(1,2)
  if (is.null(ncol(input)) || ncol(input) != 2) stop(".distortPoints: the supplied input does not have 2 columns (for x and y coordinates)")  

  # recentre coordinate system
  x <- input[,1] - detector$xCentre
  y <- input[,2] - detector$yCentre
  
  # find the polar radius
  r <- sqrt( (x)^2 + (y)^2)
  
  # perform radial distortion
  r2 <- detector$taperA * r + detector$taperB * r^2 + detector$taperC * r^3
  
  # find linear scaling ratios
  r2 <- r2 / r
  
  # scale x and y by r2 and shift coordinate system back
  x <- r2 * x + detector$xCentre
  y <- r2 * y + detector$yCentre
  
  # return
  output <- cbind(x,y)
  return(output)
}

.findCentroids <-
function(){

  ## makes an array containing the centroid position of each pseudopixel on the chip
  ## used by makeUndistortArray() and makeNormaliseArray()
  ## hidden by the period convention (ls() will not list it by default)
  
  centroid <- array(data = 0, dim = c(detector$xPxNum, detector$yPxNum, 2))
  
  #go through all pixels
  for(j in 1:detector$yPxNum) for(i in 1:detector$xPxNum) {
    centroid[i,j,1] <- detector$ccdX + i * detector$xPxSize - (detector$xPxSize / 2)
    centroid[i,j,2] <- detector$ccdY + j * detector$yPxSize - (detector$yPxSize / 2)
  }
  
  # change its dimension to a 2D matrix, where rows are the x,y coordinates (easier to use)
  dim(centroid) <- c(length(centroid)/2, 2)
  return(centroid)
}

.makeSpotMask <-
function(NX, NY, NRX, NRY, NC){

  ## Given the parameters NX, NY, NRX, NRY and NC, this function makes a logical
  ## matrix measurement box with 'TRUE' in the peak region
  
  # make sure measurement box has odd length sides (to ensure a central pixel)
  stopifnot(NX %% 2 == 1, NY %% 2 == 1)
  
  spotMask <- matrix(data = TRUE, nrow = NX, ncol = NY)
  
  # corner cutoff
  for(j in 1:NY) for(i in 1:NX) {
    if((i + j) < (NC + 2)) spotMask[i,j] <- FALSE
    if((i + j) > NX + NY - NC) spotMask[i,j] <- FALSE
    if((i - j) < (NC - NY + 1)) spotMask[i,j] <- FALSE
    if((i - j) > (NX - NC - 1))  spotMask[i,j] <- FALSE
  }
  
  # rims
  spotMask[1:NRX,] <- FALSE
  spotMask[(NX - NRX + 1):NX,] <- FALSE
  spotMask[,1:NRY] <- FALSE
  spotMask[,(NY - NRY + 1):NY] <- FALSE
  return(spotMask)
}

.undistortPixel <-
function(coords){

  ## given the coordinates of the centroid of a pseudopixel on the chip, return
  ## the a matrix of x and y components corresponding to vertices of that pixel mapped 
  ## onto the face of the detector back through the FOT. This can be readily converted to a gpc.poly object
  
  # check input is kosher - this function should only take a single x,y coordinate pair!
  if(length(coords) != 2) stop(".undistortPixel: input is not a single x,y coordinate pair")
  
  xc <- coords[[1]]
  yc <- coords[[2]]
  
  # calculate the vertices of the CCD pixel
  pt1 <- c(xc - detector$xPxSize/2, yc - detector$yPxSize/2)
  pt2 <- pt1 + c(0, detector$yPxSize)
  pt3 <- pt2 + c(detector$xPxSize, 0)
  pt4 <- pt3 + c(0, -detector$yPxSize)
  
  #calculate the vertices of the undistorted pixel
  pts <- rbind(pt1, pt2, pt3, pt4)
  pts <- .undistortPoints(pts)
  
  return(pts)
}

.undistortPoints <-
function(input){

  ## takes input X,Y positions in mm (where 0,0 mm is the bottom left corner, 
  ## when viewed with disp(), of the module) and undistorts them to positions
  ## on the face of the module, in effect by travelling backwards through the taper
  ## hidden by the period convention (ls() will not list it by default)
  
  # check input is kosher
  if (is.null(ncol(input)) && length(input) == 2) dim(input) <- c(1,2)
  if (is.null(ncol(input)) || ncol(input) != 2) stop(".undistortPoints: the supplied input does not have 2 columns (for x and y coordinates)")  
  
  # recentre coordinate system
  x <- input[,1] - detector$xCentre
  y <- input[,2] - detector$yCentre
  
  # find the polar radius
  r2 <- sqrt((x)^2 + (y)^2)
  
  # perform radial undistortion
  chooseSolution <- function(r) max(Re(polyroot(c(-r, detector$taperA, detector$taperB, detector$taperC))))
  r <- sapply(r2, chooseSolution)
  
  #find linear scaling ratios to transform x and y.
  #if x and y are zero, r2 will be zero and r3 will be NaN, so avoid dividing in these cases
  r3 <- rep(0, length(r))
  r3[r2 > 0] <- r[r2 > 0] / r2[r2 > 0]
  
  #scale x and y by r3 and shift coordinate system back
  x <- r3*x + detector$xCentre
  y <- r3*y + detector$yCentre
  
  #write into output matrix
  output <- cbind(x, y)
  return(output)
}



