addBox <-
function(NX, NY, NRX, NRY, NC, xPx = ceiling(NX/2), yPx = ceiling(NY/2)){

  ## add a measurement box to the current plot (as long as X and Y are measured in pixels not mm)

  # check input is kosher
  stopifnot(length(NX) == 1, length(NY) == 1, length(NRX) == 1, length(NRY) == 1,
            length(NC) == 1, length(xPx) == 1, length(yPx) == 1)
  
  # make sure measurement box has odd length sides (to ensure a central pixel)
  stopifnot(NX %% 2 == 1, NY %% 2 == 1)
  xSide <- floor(NX/2)
  ySide <- floor(NY/2)

  # change xPx, yPx from the centre of the measurement box to its bottom left pixel to recentre coordinates
  xPx <- xPx - xSide
  yPx <- yPx - ySide
         
  rect(xleft = xPx - 0.5 + NRX, ybottom = yPx - 0.5,
       xright = xPx - 0.5 + NX - NRX, ytop = yPx - 0.5 + NY, border="green")
  rect(xleft= xPx - 0.5, ybottom = yPx - 0.5 + NRY,
       xright = xPx - 0.5 + NX, ytop = yPx - 0.5 + NY - NRY, border="green")  
 
  f<-seq(from = 0.5, to = NC + 0.5)
  b<-seq(from = NC + 0.5, to = 0.5)
  
  segments(f+xPx-1, b+yPx-1, f+xPx, b+yPx-1, col='green')
  segments(f+xPx-1, b+yPx, f+xPx-1, b+yPx-1, col='green')
  
  segments(xPx+NX-NC-2+f, yPx+f-1, xPx+NX-NC+f-1, yPx+f-1, col='green')
  segments(xPx+NX-NC-1+f, yPx+f-1, xPx+NX-NC-1+f, yPx+f, col='green')
  
  segments(xPx+f-1, yPx+NY-NC+f-1, xPx+f, yPx+NY-NC+f-1, col='green')
  segments(xPx+f-1, yPx+NY-NC+f-2, xPx+f-1, yPx+NY-NC+f-1, col='green')

  segments(xPx+NX-NC+f-2, yPx+NY-NC+b-1, xPx+NX-NC+f-1, yPx+NY-NC+b-1, col='green')
  segments(xPx+NX-NC+f-1, yPx+NY-NC+b-1, xPx+NX-NC+f-1, yPx+NY-NC+b-2, col='green')  
  
  rect(xleft=xPx-0.5, ybottom=yPx-0.5, xright=xPx-0.5+NX, ytop=yPx-0.5+NY,border="red")  
}

