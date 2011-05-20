accumulate <-
function(input = NULL){

  ## Maps a 2 column matrix of input x,y photon positions to pixels on a grid to make a matrix image 
  ## of photoelectron counts. Currently it is assumed that each photon always produces exactly 1 electron
  ## detector$ccdX and detector$ccdY give the position of the bottom left corner (when viewed with 
  ## disp()) of the (1,1) pixel in mm
  
  # start with blank output
  output <- matrix(data = 0, nrow = detector$xPxNum, ncol = detector$yPxNum)
  
  # check the input is kosher and  if there are no photons, return an empty image now
  if(is.null(input)) return(output)
  cols <- ncol(input)
  if(is.null(cols) || cols != 2) stop("accumulate: the supplied input does not have 2 columns (for x and y photon coordinates)")
  
  # shift the input to the origin of the chip
  input[,1] <- input[,1] - detector$ccdX
  input[,2] <- input[,2] - detector$ccdY
  
  # find fractional coordinates in numbers of pixels
  input[,1] <- input[,1] / detector$xPxSize
  input[,2] <- input[,2] / detector$yPxSize
  
  # round up to give integer pixel numbers
  input[,1]<-ceiling(input[,1])
  input[,2]<-ceiling(input[,2])
  
  # write values only for allowed pixel indices (negatives and too high values 'miss the detector')
  # assume 1 e- generated per light photon
  input <- subset(input, input[,1] <= detector$xPxNum & input[,1] > 0
                  & input[,2] <= detector$yPxNum & input[,2] > 0)
  
  # make unique pixel identifier matrix
  px <- matrix(data = 1:(detector$xPxNum * detector$yPxNum) , nrow = detector$xPxNum, ncol = detector$yPxNum)
  
  # count repetition of rows (counts on pixels) in input
  f <- factor(px[input])
  counts <- as.numeric(table(f))
  
  # get the index of pixels that have counts
  index <- as.numeric(levels(f))
  
  #write the counts into the relevant output pixels
  output[index] <- counts
  
  return(output)
}

