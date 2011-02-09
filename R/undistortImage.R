undistortImage <-
function(imageMatrix, undistortArray = processing$undistortArray, bits = detector$ADCbits){

  ## takes a matrix image and the tables for pixel reapportioning and performs distortion correction
  ##

  # check the input is kosher
  if(length(dim(imageMatrix)) != 2) stop("undistortImage: the supplied imageMatrix is not a 2D matrix")
  if(typeof(undistortArray) == "NULL") stop("the undistortArray has not been properly set")
  
  # set up the output image, padded by a 100 pixel border all round,
  # to handle final pixels with negative or too-high indices
  out <- matrix(data = 0, ncol = detector$yPxNum + 200, nrow = detector$xPxNum + 200)
  
  # go through all pixels in the orginal image
  for(j in 1:detector$yPxNum) for(i in 1:detector$xPxNum) {
    # make a temporary 3x3 matrix with the correct pixel values to write to the final image
    temp <- undistortArray[[i,j]]$matrix * imageMatrix[[i,j]]
    
    # Any final pixels overlapping an overload on original image are given the overload value.
    # This may lead to pixel values greater than the overload value after the loop through all
    # pixels is complete but these will be capped inside the function read().
    if (imageMatrix[i,j] >= 2^bits){  
      temp[temp > 0] <- 2^bits
    }
    
    # find the relevant centre pixel on the output image
    xPx <- undistortArray[[i,j]]$pixel[1] + 100
    yPx <- undistortArray[[i,j]]$pixel[2] + 100
    
    # write the temporary matrix at the centre position
    out[(xPx - 1):(xPx + 1), (yPx - 1):(yPx + 1)] <- out[(xPx - 1):(xPx + 1), (yPx - 1):(yPx + 1)] + temp
  }
  
  # strip borders off
  out <- out[(101):(detector$xPxNum + 100),(101):(detector$yPxNum + 100)]
  return(out)
}

