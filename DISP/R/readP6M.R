readP6M <-
function(filename){

  ## reads an image in the miniCBF format used by the Pilatus 6M detector installed on I24 at Diamond Light Source.
  ## This is not a universal mini-CBF file reader! It assumes the image has the byte-offset compression scheme
  ## described in http://www.bernstein-plus-sons.com/software/CBF/doc/cbf_definition_rev.html
  
  byteLen <- file.info(filename)$size
  f <- file(filename, open="rb")
  
  # check the magic number
  header <- list()
  header$magicNumber <- readChar(f, 15)
  if (header$magicNumber != "###CBF: VERSION") stop("readP6M: file not recognised")
  header$CBFversion <- readLines(f, 1, ok=FALSE)
 
  #now read the rest of the header up to the binary data
  endofheader <- FALSE
  headertext <- NULL
  strlength <- 0
  START_OF_BIN <- rawToChar(as.raw(c(12,26,4)), multiple = TRUE) #CBF start of binary section identifier
  while(!endofheader){
    headertext[strlength + 1] <- readChar(f,1)
    strlength <- strlength + 1
    test <- headertext[max(1 ,strlength - 2):strlength]
    if(identical(test, START_OF_BIN)) endofheader <- TRUE
  if(seek(f) >= byteLen) stop("readP6M: reached end of file without finding a binary section")
  }
  headertext <- paste(headertext, collapse="")
  headertext <- strsplit(headertext, split="[\r\n]+")[[1]]
  stopifnot(identical(readBin(f, "integer", 1, size=1, signed = FALSE), as.integer(213))) #check the "binary flag character"
  
  #extract required information about the binary from the header
  binarySize <- headertext[grepl("X-Binary-Size:", headertext, fixed=TRUE)]
  binarySize <- as.integer(sub(pattern="X-Binary-Size:", replacement="", binarySize, fixed=TRUE))
  nFast <- headertext[grepl("X-Binary-Size-Fastest-Dimension:", headertext, fixed=TRUE)]
  nFast <- as.integer(sub(pattern="X-Binary-Size-Fastest-Dimension:", replacement="", nFast, fixed=TRUE))
  nSlow <- headertext[grepl("X-Binary-Size-Second-Dimension:", headertext, fixed=TRUE)]
  nSlow <- as.integer(sub(pattern="X-Binary-Size-Second-Dimension:", replacement="", nSlow, fixed=TRUE))
  
  #Read the binary data now
  binaryRaw <- readBin(f, what = "raw", n = binarySize, endian = "little") #get endianness from the header...
  #Now readLines to get the back end of the text section
  headerEnd <- readLines(f)
  close(f)
  
  headerFull <- c(headertext, headerEnd)
  
  #dyn.load(paste("readP6M_decompress", .Platform$dynlib.ext, sep=""))
  decomp <- function(binaryRaw, binarySize){
    decompress <- .C("decompress", as.raw(binaryRaw), as.integer(binarySize), pixelArray = integer(binarySize),
		errCode = integer(1), PACKAGE="DISP")
    if(decompress$errCode == 1) stop("unable to fit all pixel values in an integer vector")
    return(decompress$pixelArray)
  }
  
  pixelArray <- (decomp(binaryRaw, binarySize))
  #strip off binary padding, if present
  pixelArray <- pixelArray[1:(nFast*nSlow)]
  dim(pixelArray) <- c(nFast, nSlow)
  
  #attach header as attribute
  attr(pixelArray, "header") <- headerFull
  
  return(pixelArray)
  
}

