makeImageSet <-
function(string, xRange = NULL, yRange = NULL, type = "ASCII"){

  ## Accessory function for image statistics functions, takes images set stored
  ## in memory or on disk with names matching pattern### (where # is a digit) and
  ## makes a 3D array with each layer containing an image

  # check input is kosher
  if (typeof(string) != "character" || length(string) != 1){
    stop("makeImageSet: the supplied string is not a single character string")
  }
  if (!is.null(xRange)){
    if(length(xRange) != 2) stop("makeImageSet: the supplied xRange is not in the form c(x1, x2)")
  }
  if (!is.null(yRange)){
    if(length(yRange) != 2) stop("makeImageSet: the supplied yRange is not in the form c(y1, y2)")
  }  
  
  string <- getFilePattern(string)
  directory <- string[[1]]
  pattern <- string[[2]]
  imageListSearchList <- apropos(what = pattern, mode = "numeric")
  imageListFileSystem <- dir(path = directory, pattern = pattern)
  imageList <- NULL
  fileSystem <- FALSE
  if(length(imageListSearchList) == 0){
    imageList <- imageListFileSystem
    fileSystem <- TRUE
  }
  if(length(imageListFileSystem) == 0) imageList <- imageListSearchList

  if(is.null(imageList)) stop("makeImageSet: the supplied string matches images in both the search path and on the filesystem")
  
  nImages <- length(imageList) 
  if (nImages == 0) stop("No objects matched by the pattern")
  
  print.noquote(paste("makeImageSet:",nImages,"objects are matched by the pattern and will be combined into a set"))
  
  # set read function for either filesystem or from memory
  typeCase <- pmatch(type, c("ASCII", "Mar", "SMV", "miniCBF"))
  if(is.na(typeCase)) stop("Bad parameter for type") # stop if doesn't match
  
  readThisImage <- if(fileSystem && typeCase == 1){ #as ASCII files
    function(imageName, directory) readImage(paste(directory, imageName, sep=""))
  }else if(fileSystem && typeCase ==2){ #as Mar formatted files 
    function(imageName, directory) readMar(paste(directory, imageName, sep=""))
  }else if(fileSystem && typeCase ==3){ #as SMV files
    function(imageName, directory) readSMV(paste(directory, imageName, sep=""))
  }else if(fileSystem && typeCase ==4){ #as Pilatus mini CBF files
    function(imageName, directory) readCBF(paste(directory, imageName, sep=""))    
  }else{ #from the search patch
    function(imageName, directory) eval(as.name(imageName))
  }
  
  # get first image to find size
  firstImage <- readThisImage(imageList[[1]], directory)
  xPx <- dim(firstImage)[[1]]
  yPx <- dim(firstImage)[[2]]
  
  if(is.null(xRange)) xRange <- c(1, xPx)
  if(is.null(yRange)) yRange <- c(1, xPx)  
  
  xSeq <- xRange[[1]]:xRange[[2]]
  ySeq <- yRange[[1]]:yRange[[2]]  
  
  # set up array to store nImages image subsets in the specified range
  imageSet <- array(data = 0, dim = c(length(xSeq), length(ySeq), nImages))
  imageSet[,,1] <- firstImage[xSeq,ySeq]
  
  # go through each of nImages layers of imageSet and write the corresponding image
  if(nImages > 1) for(n in 2:nImages){
    temp <- readThisImage(imageList[n], directory)
    # check this image is the same size as the first
    stopifnot(dim(temp) == c(xPx, yPx))
    imageSet[,,n] <- temp[xSeq, ySeq]
  }
  return(imageSet)
}

