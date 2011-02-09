normaliseImage <-
function(imageMatrix, dark = processing$darkRaw, normaliser = processing$flatfield){
  
  ## non-unformity correction. A raw image is read in, a raw dark subtracted from it, and then it is
  ## scaled using a suitable normalisation array, i.e. a flood image that has been corrected for non-
  ## uniformity by makeFlatfield(). N.B, the resulting image has no bias offset and contains real,
  ## rather than integer values. This function is intended to be an intermediate step in overall corrections!

  # check the input is kosher  
  if (length(dim(imageMatrix)) != 2) stop("normaliseImage: the supplied imageMatrix is not a 2D matrix")  
  if (typeof(dark) == "NULL") stop("normaliseImage: the raw dark image has not been properly set")  
  if (typeof(normaliser) == "NULL") stop("normaliseImage: the flatfield has not been properly set")

  # subtract a dark (bias) image
  out <- imageMatrix - dark
   
  # perform non-uniformity correction here.
  return(out / normaliser)
}

