readImage <-
function(filename, type="integer"){

  ## reads in an ascii file as a matrix image
  ##
  
  # check input is rectangular
  lineLengths <- count.fields(filename)
  xLength <- lineLengths[[1]]
  yLength <- length(lineLengths)
  if(!all(lineLengths == xLength)) stop("readImage: input ",filename," is not a rectangular matrix")
  
  typeCase <- pmatch(type, c("integer", "double"))
  if(is.na(typeCase)) stop("Bad parameter for type") # stop if doesn't match
  if(typeCase == 1) what <-  integer(0)
  if(typeCase == 2) what <-  double(0)
  
  output <- scan(filename, what = what)
  output <- matrix(output, nrow = xLength, ncol = yLength, byrow = FALSE, dimnames = NULL)
  output <- output[,yLength:1] # flip in second dimension (y) to get origin at bottom left
  return(output)
  
  output <- as.matrix(read.table(filename))
  output <- t(output) # want x (first dimension) increasing along rows not columns
  cols <- ncol(output)
  output <- output[,cols:1] # have y increasing from bottom of rows in input file, not from top
  
  dimnames(output) <- NULL
  return(output)
}

