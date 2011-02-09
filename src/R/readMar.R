readMar <-
function(filename){

  ## reads an image in the format described in the MarCCD header documentation (valid for v.0.17.1 at least)
  ## the frame_header is attached as an attribute of the object
  
  f <- file(filename, open="rb")
  
  # TIFF header
  byteOrder <- readChar(f, 2) #should be "II" on little endian, "MM" on big endian
  fortyTwo <- readBin(f, "integer", n=1, size=2) #check this is 42
  if(fortyTwo != 42) stop("readMar: file not recognised or wrong byte order")
  seek(f, where = 1024) #skip rest of TIFF header to get to MarCCD frame_header
  
  # frame_header
  header <- list()
  # format parameters (256 bytes)
  header$magicNumber <- readBin(f, "integer", n=1, size=4)
  header$name <- readChar(f, 16)
  remainingFormatParams <- readBin(f, "integer", n=59, size=4)
  header$nfast <- remainingFormatParams[[16]]
  header$nslow <- remainingFormatParams[[17]]
  header$depth <- remainingFormatParams[[18]]
  # data statistics (128 bytes)
  header$dataStats <- readBin(f, "integer", n=32, size=4)
  # more statistics (256 bytes)
  seek(f, where = 256, origin = "current")
  # goniostat params (128 bytes)
  header$gonioParams <- readBin(f, "integer", n=32, size=4)
  # detector params (128 bytes)
  header$detParams <- readBin(f, "integer", n=32, size=4)
  seek(f, where = 4096)
  
  # image data
  imageData <- readBin(f, "integer", n = header$nfast*header$nslow, size = header$depth, signed=FALSE)
  dim(imageData) <- c(header$nfast,header$nslow)
  imageData <- imageData[,header$nslow:1]
  close(f)

  #attach header as attribute
  attr(imageData, "header") <- header  
  return(imageData)
}

