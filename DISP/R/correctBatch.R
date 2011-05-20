correctBatch <-
function(stringIn, patternOut, envir = .GlobalEnv){

  ## applies corrections to a batch of images that match 'stringIn' (which takes the form
  ## 'name###') and writes them with names according to 'patternOut' (which takes the form 'name')
  
  # This may take some time, so keep track 
  time1 <- Sys.time()
  
  # check inputs are kosher
  if(typeof(stringIn) != "character" || length(stringIn) != 1){
    stop("stringIn is not a single character string")
  }
  if(typeof(patternOut) != "character" || length(patternOut) != 1){
    stop("patternOut is not a single character string")
  }

  stringIn <- getFilePattern(stringIn)
  patternIn <- stringIn[2]
  imageList <- ls(name = envir, pattern = patternIn)
  nImages <- length(imageList)
  digits = nchar(nImages)

  for (n in 1:nImages){
    cat("correcting image",n,"\n")
    if (.Platform$OS.type == "windows") flush.console()
    number <- formatC(n, width=digits, flag="0")
    imageName <- paste(patternOut, number, sep="")
    assign(imageName, correct(eval(as.name(imageList[n]))), envir = envir)
  }
  
  # how long did it all take?
  time2 <- Sys.time()
  print(difftime(time2,time1))
}

