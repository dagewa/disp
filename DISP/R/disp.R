disp <-
function(imageMatrix, white = NULL, black = NULL, units = "pixels", origin = c(1,1),
                 box = TRUE, asp=1, raster = "auto") {

  ## Graphical display of an image matrix in 1024 grayshades
  ## Attempts to find sensible default values for 'white' and 'black'
  
  ## N.B. The new raster method depends on the grid package and is much faster for large images
  
  # origin= is used to specify which pixel is at the bottom left corner of the plot. It is useful
  # when imageMatrix is a subset selected with the '[,]' operator of some larger image but you want
  # to retain the axis labels relevant for the larger image. It is given in pixel indices whatever
  # the choice for units=

  # check the input is kosher
  if(length(dim(imageMatrix)) != 2) stop("disp: the supplied imageMatrix is not a 2D matrix")
  unitCase <- pmatch(units, c("pixels", "mm"))
  if(is.na(unitCase)) stop("Bad parameter for unit") # stop if doesn't match

  xPx <- dim(imageMatrix)[1]
  yPx <- dim(imageMatrix)[2]
  
  rasterCase <- pmatch(raster, c("auto", "yes", TRUE, "no", FALSE))
  if(is.na(rasterCase)) stop("Bad parameter for raster") # stop if doesn't match
  if(rasterCase == 1) raster <- if(xPx * yPx > 512^2 - 1) TRUE else FALSE
  if(rasterCase == 2) raster <- TRUE
  if(rasterCase == 3) raster <- TRUE
  if(rasterCase == 4) raster <- FALSE
  if(rasterCase == 5) raster <- FALSE
  
  minIm <- min(imageMatrix, na.rm = TRUE)
  medIm <- mean(imageMatrix, na.rm = TRUE)
  maxIm <- max(imageMatrix, na.rm=TRUE)
  
  if (is.null(white)) white <- minIm
  
  # this method is not always perfect. Need robust estimators rather than min and max to specify the range
  if (is.null(black)) black <- min(medIm + 3 * (medIm - minIm), maxIm)
  dynRange <- black - white
  
  #truncate image at max scale
  imageMatrix[imageMatrix > black] <- black
  #reverse scale to start at black = 0
  imageMatrix <- black - imageMatrix
  #now truncate the high values (white) at the dynamic range limit
  imageMatrix[imageMatrix > dynRange] <- dynRange
  slab <- seq(from = 0, to = dynRange, length.out = 1026)
  
  grayScale <- gray((0:1024)/1024)
  
  if(unitCase == 1){ #pixels
    xPxSize <- 1
    yPxSize <- 1
    width <- xPxSize * xPx
    height <- yPxSize * yPx
    
    #make sequences with the positions of the pixel centroids in x and y (for the image() method) and
    #get limits of the box in which to display the data (for the rasterImage() method).
    if(raster){
      xleft <- origin[[1]] - 0.5
      ybottom <- origin[[2]] - 0.5
    } else {
      xpos <- origin[[1]] - 1 + 1:xPx
      ypos <- origin[[2]] - 1 + 1:yPx 
    }
    
    #set up plot limits and labels
    xlab <- "X (pixels)"
    ylab <- "Y (pixels)"
    xlim <- c(origin[[1]] - 0.5, origin[[1]] - 0.5 + width)
    ylim <- c(origin[[2]] - 0.5, origin[[2]] - 0.5 + height)
  }

  if(unitCase == 2){ #mm
    xPxSize <- detector$xPxSizeFace
    yPxSize <- detector$yPxSizeFace
    width <-  xPxSize * xPx
    height <- yPxSize * yPx
    
    #make sequences with the positions of the pixel centroids in x and y (for the image() method) and
    #get limits of the box in which to display the data (for the rasterImage() method).
    if(raster){
      xleft <- (origin[[1]] - 1) * xPxSize 
      ybottom <- (origin[[2]] - 1) * yPxSize 
    } else {
      xpos <- xPxSize * (origin[[1]] - 1.5 + 1:xPx)
      ypos <- yPxSize * (origin[[2]] - 1.5 + 1:yPx)
    }

    #set up plot limits and labels
    xlab <- "X (mm)"
    ylab <- "Y (mm)"
    xlim <- c((origin[[1]] - 1) * xPxSize, (origin[[1]] - 1) * xPxSize + width)
    ylim <- c((origin[[2]] - 1) * yPxSize, (origin[[2]] - 1) * yPxSize + height)
  }
  
  #display
  if(raster){ #try to use high level rasterImage function for simplicity
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, asp = 1, xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab)
    #I experienced issues with image rotation using the angle= parameter of rasterImage() on Windows. For that reason
    #rotate the imageMatrix first before forming the bitmap
    imageMatrix <- rotate(imageMatrix)
    imageMatrix <- as.raster(imageMatrix, max = dynRange)
    rasterImage(imageMatrix, xleft = xleft, ybottom = ybottom, xright = xleft + width,
                ytop = ybottom + height, interpolate = FALSE)
    title(xlab=xlab, ylab=ylab)
  } else {
    image(x = xpos, y = ypos, z = imageMatrix, col = grayScale, breaks = slab, xlab = xlab, ylab = ylab, 
          asp = asp,
          xlim = xlim, ylim = ylim,
          xaxt = 'n', yaxt = 'n',
          bty = 'n')
  }
  axis(1, pos=ylim[[1]])
  axis(2, pos=xlim[[1]])
  #use rect() for plot box, because the box() function doesn't do what I want
  if(box) rect(xlim[[1]], ylim[[1]], xlim[[2]], ylim[[2]])
}

