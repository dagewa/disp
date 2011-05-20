dispContour <-
function(imageMatrix, origin = c(1,1)){
  
  ## Displays an image as a contour map. Used by dispBox()
  ##

  # origin= is used to specify which pixel is at the bottom left corner of the plot. It is useful
  # when imageMatrix is a subset selected with the '[,]' operator of some larger image but you want
  # to retain the axis labels relevant for the larger image. It is given in pixel indices   
  # check the input is kosher

  if(length(dim(imageMatrix)) != 2) stop("dispContour: the supplied imageMatrix is not a 2D matrix")
  
  xPx <- dim(imageMatrix)[1]
  yPx <- dim(imageMatrix)[2]
  levels <- c(0, 2, 5, 10,20,50,100,200,400,800,1600,3200,6400,12800,25600)
  levels <-  levels + min(imageMatrix)
  #levels <- signif(levels, digits=2)
  x <- 1:xPx + (origin[[1]] - 1)
  y <- 1:yPx + (origin[[2]] - 1)
  contour(x = x, y = y, z=imageMatrix, levels = levels, xlab = "X", ylab = "Y", asp = 1, axes = FALSE, frame.plot = FALSE)
  axis(1, pos = min(x) - 1, at = seq.int(from = min(x) - 1, to = max(x)))
  axis(2, pos = min(y) - 1, at = seq.int(from = min(y) - 1, to = max(y)))
}
