dispBox <-
function(imageMatrix, NX, NY, NRX, NRY, NC, xPx = ceiling(NX/2), yPx = ceiling(NY/2)){

  ## display a spot bounded by the measurement box
  ## using a contour plot

  # check input is kosher
  if(length(dim(imageMatrix)) != 2) stop("dispBox: the supplied imageMatrix is not a 2D matrix")
  stopifnot(length(NX) == 1, length(NY) == 1, length(NRX) == 1, length(NRY) == 1,
            length(NC) == 1, length(xPx) == 1, length(yPx) == 1)
  
  # make sure measurement box has odd length sides (to ensure a central pixel)
  stopifnot(NX %% 2 == 1, NY %% 2 == 1)
  xSide <- floor(NX/2)
  ySide <- floor(NY/2)

  imageSlice <- imageMatrix[(xPx-xSide):(xPx + xSide), (yPx-ySide):(yPx+ySide)]
    
  dispContour(imageSlice)

  addBox(NX, NY, NRX, NRY, NC)
}

