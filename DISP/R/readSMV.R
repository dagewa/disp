readSMV <-
function(filename){

  ## reads in an image in the "SMV" format (as used by ADSC CCD detectors) described at
  ## http://strucbio.biologie.uni-konstanz.de/ccp4wiki/index.php/SMV_file_format
  ## the header list is stored as an attribute of the object

  # find header size
  head <- readChar(filename, 50) #read in first 50 characters
  pattern <- "HEADER_BYTES=[[:blank:]]*[[:digit:]]"
  RE <- regexpr(pattern,head)
  pos1<- RE + attr(RE, "match.length") - 1
  RE2 <- regexpr(paste(RE, "*;", sep=""),head)
  pos2 <- RE2 + attr(RE2, "match.length") - 2
  headerSize <- as.integer(substr(head, pos1, pos2))
  
  # read full header  
  f <- file(filename, open="rb")
  header <- readChar(f, headerSize, useBytes=TRUE)
  header <- strsplit(header, ";\n")
  size1 <- as.integer(sub("[[:alnum:]]*=","",header[[1]][grep("^SIZE1=", header[[1]])])) #one-line convenience, tho looks complex!
  size2 <- as.integer(sub("[[:alnum:]]*=","",header[[1]][grep("^SIZE2=", header[[1]])]))
  type <- sub("[[:alnum:]]*=","",header[[1]][grep("^TYPE=", header[[1]])])
  if(type != "unsigned_short")stop(paste("readADSC: pixel record type",type,"is unsupported")) # check pixels are 2 bytes
  
  # read image data
  imageData <- readBin(f, "integer", n=size1*size2, size=2, signed=FALSE)
  dim(imageData) <- c(size1, size2)
  imageData <- imageData[,size2:1]
  close(f)
  
  #attach header as attribute
  attr(imageData, "header") <- header
  return(imageData)
}

