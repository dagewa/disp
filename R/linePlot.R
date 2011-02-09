linePlot <- function (imageMatrix, px1 = NULL, px2 = NULL, fwxm = NULL, add = FALSE, newCol = "blue", ...){
  
  ## Displays a plot of the values taken from a line through an image, by default using
  ## interactively chosen start and end points. The parameter fwxm can be used to annotate
  ## the plot with FWHM, FW10%M etc for the highest peak present using linear interpolation. The values
  ## of these annotations (or NULL) is returned invisibly by the function.  

  # check input is kosher  
  if (length(dim(imageMatrix)) != 2) 
    stop("linePlot: the supplied imageMatrix is not a 2D matrix")
  
  # determine start and end points
  if(is.null(px1)){
    pts <- NULL
    try(pts <- locator(n=2, type="o", col="red"))
    if(is.null(pts)){
      px1 <- c(1, 1);
      px2 <- dim(imageMatrix)
    }else{
      px1 <- c(round(pts$x[[1]]), round(pts$y[[1]]))
      px2 <- c(round(pts$x[[2]]), round(pts$y[[2]]))
    }
  }
  if (length(px1) != 2 || length(px2) != 2) 
    stop("linePlot: pixel coordinates not correctly supplied")

  # the distance in x and y crossed by the line from the centre of px1 to the centre of px2, in pixels
  xLength <- px2[1] - px1[1]
  yLength <- px2[2] - px1[2]
  
  # how many pixels in the output list
  lineLength <- max(abs(xLength), abs(yLength))
  
  output <- NULL
  for (n in 0:lineLength) {
    i <- ceiling((px1[1] - 0.5) + n * xLength/lineLength)
    j <- ceiling((px1[2] - 0.5) + n * yLength/lineLength)
    output[n + 1] <- imageMatrix[i, j]
  }

  if(add){
    points(output, col = newCol)
    lines(output, col = newCol)
  } else plot(output, type = "o", xlab = "pixels", ...)
  
  maxm <- max(output)
  maxmPos <- which.max(output)
  fwxmOut <- NULL
  for (x in fwxm) {
    xm <- x * maxm
    
    #step along either side of peak until less than xm, i.e. overstep the position
    lpos <- maxmPos
    rpos <- maxmPos
    while (output[lpos] > xm) lpos <- lpos - 1
    while (output[rpos] > xm) rpos <- rpos + 1
    
    #use linear interpolation to go back to the correct position between the points
    wholeStep <- output[lpos + 1] - output[lpos]
    fracStep <- xm - output[lpos]
    dl <- fracStep/wholeStep
    lpos <- lpos + dl
    
    wholeStep <- output[rpos - 1] - output[rpos]
    fracStep <- xm - output[rpos]
    dr <- fracStep/wholeStep
    rpos <- rpos - dr
    
    fwxmHere <- rpos - lpos
    
    segments(lpos, xm, rpos, xm, col = "red")
    text(rpos + 1.5, xm, labels = paste("FW", x * 100, "%M = ", 
       round(fwxmHere, 2), sep = ""), adj = 0)
    fwxmOut <- append(fwxmOut, fwxmHere)
  }
  invisible(fwxmOut)
}