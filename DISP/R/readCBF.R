readCBF <-
function(filename){

  ## reads an image in the miniCBF format used by the Pilatus 6M detector installed on I24 at Diamond Light Source.
  ## This is not a universal mini-CBF file reader! It assumes the image has the byte-offset compression scheme
  ## described in http://www.bernstein-plus-sons.com/software/CBF/doc/cbf_definition_rev.html
  
  byteLen <- file.info(filename)$size
  f <- file(filename, open="rb")
  
  # check the magic number
  header <- list()
  header$magicNumber <- readChar(f, 15)
  if (header$magicNumber != "###CBF: VERSION") stop("readCBF: file not recognised")
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
  if(seek(f) >= byteLen) stop("readCBF: reached end of file without finding a binary section")
  }
  headertext <- paste(headertext, collapse="")
  headertext <- strsplit(headertext, split="[\r\n]+")[[1]]
  stopifnot(identical(readBin(f, "integer", 1, size=1, signed = FALSE), as.integer(213))) #check the "binary flag character"
  
  #extract required information about the binary from the header
  binarySize <- headertext[grepl("X-Binary-Size:", headertext, fixed=TRUE)]
  binarySize <- as.integer(sub(pattern="X-Binary-Size:", replacement="", binarySize, fixed=TRUE))
  nElem <- headertext[grepl("X-Binary-Number-of-Elements:", headertext, fixed=TRUE)]
  nElem <- as.integer(sub(pattern="X-Binary-Number-of-Elements:", replacement="", nElem, fixed=TRUE))
  nFast <- headertext[grepl("X-Binary-Size-Fastest-Dimension:", headertext, fixed=TRUE)]
  nFast <- as.integer(sub(pattern="X-Binary-Size-Fastest-Dimension:", replacement="", nFast, fixed=TRUE))
  nSlow <- headertext[grepl("X-Binary-Size-Second-Dimension:", headertext, fixed=TRUE)]
  nSlow <- as.integer(sub(pattern="X-Binary-Size-Second-Dimension:", replacement="", nSlow, fixed=TRUE))
  
  #extract some more meta data; useful for a file header if we want to write the image later
    #assume pixel size is given by a line of the form "# Pixel_size 172e-6 m x 172e-6 m"
  pixSize <- headertext[grepl("# Pixel_size ", headertext, fixed=TRUE)]
  pixSize <- sub(pattern="# Pixel_size ", replacement ="",pixSize)
  pixSize <- strsplit(pixSize, "x", fixed=TRUE)[[1]]
  pixSize <- sub(pattern="m", replacement ="",pixSize)
  pixSize <- as.numeric(pixSize)
    #assume exposure time is given by a line of the form "# Exposure_time 0.0975000 s"
  exposureTime <- headertext[grepl("# Exposure_time ", headertext, fixed=TRUE)]
  patt <- regexpr(pattern="[[:digit:].]+", exposureTime)
  exposureTime <- substr(exposureTime, patt, patt + attr(patt,"match.length") - 1)
  exposureTime <- as.numeric(exposureTime)
    #assume exposure time is given by a line of the form "Exposure_period 0.1000000 s"
  exposurePeriod <- headertext[grepl("# Exposure_period ", headertext, fixed=TRUE)]
  patt <- regexpr(pattern="[[:digit:].]+", exposurePeriod)
  exposurePeriod <- substr(exposurePeriod, patt, patt + attr(patt,"match.length") - 1)
  exposurePeriod <- as.numeric(exposurePeriod)
    #assume count cutoff is given by a line of the form "# Count_cutoff 244849 counts"
  overload <- headertext[grepl("# Count_cutoff ", headertext, fixed=TRUE)]
  patt <- regexpr(pattern="[[:digit:].]+", overload)
  overload <- substr(overload, patt, patt + attr(patt,"match.length") - 1)
  overload <- as.integer(overload)
    #assume wavelength is given by a line of the form "# Wavelength 0.97780 A"
  wavelength <- headertext[grepl("# Wavelength ", headertext, fixed=TRUE)]
  patt <- regexpr(pattern="[[:digit:].]+", wavelength)
  wavelength <- substr(wavelength, patt, patt + attr(patt,"match.length") - 1)
  wavelength <- as.numeric(wavelength)
    #assume detector distance is given by a line of the form "# Detector_distance 0.20000 m"
  detectorDistance <- headertext[grepl("# Detector_distance ", headertext, fixed=TRUE)]
  patt <- regexpr(pattern="[[:digit:].]+", detectorDistance)
  detectorDistance <- substr(detectorDistance, patt, patt + attr(patt,"match.length") - 1)
  detectorDistance <- as.numeric(detectorDistance)
    #assume beam_xy is given by a line of the form "# Beam_xy (1268.41, 1288.50) pixels"
  beam_xy <- headertext[grepl("# Beam_xy ", headertext, fixed=TRUE)]
  beam_xy <- strsplit(beam_xy, ",", fixed=TRUE)[[1]]
  patt <- regexpr(pattern="[[:digit:].]+", beam_xy)
  beam_xy <- substr(beam_xy, patt, patt + attr(patt,"match.length") - 1)
  beam_xy <- as.numeric(beam_xy)
    #assume start angle is given by a line of the form "# Start_angle 45.0000 deg."
  phiStart <- headertext[grepl("# Start_angle ", headertext, fixed=TRUE)]
  patt <- regexpr(pattern="[[:digit:].]+", phiStart)
  phiStart <- substr(phiStart, patt, patt + attr(patt,"match.length") - 1)
  phiStart <- as.numeric(phiStart)
    #assume phi increment is given by a line of the form "# Angle_increment 0.1000 deg."
  phiIncrement <- headertext[grepl("# Angle_increment ", headertext, fixed=TRUE)]
  patt <- regexpr(pattern="[[:digit:].]+", phiIncrement)
  phiIncrement <- substr(phiIncrement, patt, patt + attr(patt,"match.length") - 1)
  phiIncrement <- as.numeric(phiIncrement)
    #make the list
  metadata <- list(xPxSize=pixSize[1],yPxSize=pixSize[2],exposureTime=exposureTime,exposurePeriod=exposurePeriod,
    overload=overload,wavelength=wavelength,detectorDistance=detectorDistance,
    beamX=beam_xy[1],beamY=beam_xy[2],phiStart=phiStart,phiIncrement=phiIncrement)
  
  #Read the binary data now
  binaryRaw <- readBin(f, what = "raw", n = binarySize, endian = "little") #only supporting little endian files so far
  
  #Now readLines to get the back end of the text section
  headerEnd <- readLines(f)
  close(f)
  
  headerFull <- c(headertext, headerEnd)
  
  #dyn.load(paste("readCBF_decompress", .Platform$dynlib.ext, sep=""))
  decomp <- function(binaryRaw, binarySize){
    decompress <- .C("decompress", as.raw(binaryRaw), as.integer(binarySize), pixelArray = integer(nElem),
		errCode = integer(1), PACKAGE="DISP")
    if(decompress$errCode == 1) stop("unable to fit all pixel values in an integer vector")
    return(decompress$pixelArray)
  }
  
  pixelArray <- (decomp(binaryRaw, binarySize))
  
  #set dimensions, if provided. Will warn if not
  pixelArray <- pixelArray[1:(nFast*nSlow)]
  try(dim(pixelArray) <- c(nFast, nSlow))
  
  #attach header and metadata as attributes
  attr(pixelArray, "header") <- headerFull
  attr(pixelArray, "metadata") <- metadata
  
  return(pixelArray)
  
}

