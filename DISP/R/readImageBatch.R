readImageBatch <-
function(string, envir = .GlobalEnv, type="ASCII"){

  ## reads in multiple files matching pattern### as matrix images
  ## keeping their filenames as the object names
  
  # choose type of image reading function
  typeCase <- pmatch(type, c("ASCII", "Mar", "SMV", "P6M"))
  if(is.na(typeCase)) stop("Bad parameter for type") # stop if doesn't match   
  if(typeCase == 1) readThisImage <- readImage
  if(typeCase == 2) readThisImage <- readMar
  if(typeCase == 3) readThisImage <- readSMV
  if(typeCase == 4) readThisImage <- readP6M
  
  string <- getFilePattern(string)
  directory <- string[1]
  pattern <- string[2]

  list <- dir(path = directory, pattern = pattern)
  nImages <- length(list)
  if(nImages == 0) stop(paste("No images match filename pattern", pattern, "in", directory))
  print.noquote(paste("Reading",nImages,"images matching filename  pattern", pattern, "in", directory))
  for(i in 1:nImages){
    thisImage <- readThisImage(paste(directory, as.name(list[i]), sep=""))
    assign(list[i], thisImage, envir = envir)
  }
}

