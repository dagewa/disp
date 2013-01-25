writeCBF <-
function(imageMatrix, filename, binLenMultiplier = 4, xPxSize=NULL,yPxSize=NULL,exposureTime=NULL,exposurePeriod=NULL,
    overload=NULL,wavelength=NULL,detectorDistance=NULL,beamX=NULL,beamY=NULL,phiStart=NULL,phiIncrement=NULL){

  ## writes an image in a kludged miniCBF format similar to that used by the Pilatus 6M detector installed on I24 at Diamond Light Source.
  ## This is not a universal mini-CBF file writer! It assumes the image has the byte-offset compression scheme
  ## described in http://www.bernstein-plus-sons.com/software/CBF/doc/cbf_definition_rev.html
  
  #prepare compressed data
  vecLen <- as.integer(length(imageMatrix))
  binLen <- as.integer(max(100, binLenMultiplier*vecLen))
  
  compress <- .C("compress", vec=as.integer(imageMatrix), vecLen=vecLen,
	binArr=raw(binLen), binLen=binLen, comprLen=integer(1),
	errCode=integer(1), PACKAGE="DISP")
  
  #check for errors
  if(compress$errCode == 1) stop("error compressing data. A difference between adjacent pixel values is outside the supported range.")
  if(compress$errCode == 2) stop("error compressing data. Try increasing binLenMultiplier.")
  if(compress$errCode != 0) stop("error compressing data.")

  #truncate binArr to the right length
  binArr <- compress$binArr[1:compress$comprLen]
  
  #get some data for the header
  nFast <- nrow(imageMatrix)
  nSlow <- ncol(imageMatrix)

  #do we have image metadata?
  metadata <- attr(imageMatrix,"metadata")
  if(!is.null(metadata)){
    if(is.null(xPxSize)) xPxSize <- metadata$xPxSize 
    if(is.null(yPxSize)) yPxSize <- metadata$yPxSize 
    if(is.null(exposureTime)) exposureTime <- metadata$exposureTime 
    if(is.null(exposurePeriod)) exposurePeriod <- metadata$exposurePeriod 
    if(is.null(overload)) overload <- metadata$overload 
    if(is.null(wavelength)) wavelength <- metadata$wavelength 
    if(is.null(detectorDistance)) detectorDistance <- metadata$detectorDistance 
    if(is.null(beamX)) beamX <- metadata$beamX 
    if(is.null(beamY)) beamY <- metadata$beamY 
    if(is.null(phiStart)) phiStart <- metadata$phiStart 
    if(is.null(phiIncrement)) phiIncrement <- metadata$phiIncrement 
  }
  #now provide some made up values if anything is left undefined (dangerous! - alternatively I could make these values ridiculous)
  if(is.null(xPxSize)) xPxSize <- 0.000172
  if(is.null(yPxSize)) yPxSize <- 0.000172
  if(is.null(exposureTime)) exposureTime <- 1.0
  if(is.null(exposurePeriod)) exposurePeriod <- 1.0
  if(is.null(overload)) overload <- 1000000
  if(is.null(wavelength)) wavelength <- 1.0
  if(is.null(detectorDistance)) detectorDistance <- 1.0
  if(is.null(beamX)) beamX <- nFast/2
  if(is.null(beamY)) beamY <- nSlow/2
  if(is.null(phiStart)) phiStart <- 0.0
  if(is.null(phiIncrement)) phiIncrement <- 0.1
  
  #open file for writing - do this before the call to normalizePath below
  f <- file(filename, open="wb") 
  
  header <- paste("###CBF:                this is not a SLS/DECTRIS PILATUS detector, it was written by DISP v",packageDescription("DISP", fields = "Version"),"\r\n",
	"\r\n",
	"data_DISP_imageMatrix\r\n",
	"\r\n",
	'_array_data.header_convention "UNDEFINED"\r\n',
	"_array_data.header_contents\r\n",
	";\r\n",
	"# Detector: DISP\r\n",
	"# ",date(),"\r\n",
	"# Pixel_size ",1e6*xPxSize,"e-6 m x ",1e6*yPxSize,"e-6 m\r\n",
	"# Exposure_time ",exposureTime," s\r\n",
	"# Exposure_period ",exposurePeriod," s\r\n",
	"# Count_cutoff ",sprintf("%i",overload)," counts\r\n",
	"# Image_path: ",normalizePath(filename),"\r\n",
	"# Wavelength ",wavelength," A\r\n",
	"# Detector_distance ",detectorDistance," m\r\n",
	"# Beam_xy (",beamX,", ",beamY,") pixels\r\n",
	"# Start_angle ",phiStart," deg.\r\n",
	"# Angle_increment ",phiIncrement," deg.\r\n",
	";\r\n",
	"\r\n",
	"_array_data.data\r\n",
	";\r\n",
	"--CIF-BINARY-FORMAT-SECTION--\r\n",
	"Content-Type: application/octet-stream;\r\n",
	'     conversions="x-CBF_BYTE_OFFSET"\r\n',
	"Content-Transfer-Encoding: BINARY\r\n",
	"X-Binary-Size: ",sprintf("%i",compress$comprLen),"\r\n",
	"X-Binary-ID: 1\r\n",
	'X-Binary-Element-Type: "signed 32-bit integer"\r\n',
	"X-Binary-Element-Byte-Order: LITTLE_ENDIAN\r\n",
	"X-Binary-Number-of-Elements: ",sprintf("%i",vecLen),"\r\n",
	"X-Binary-Size-Fastest-Dimension: ",sprintf("%i",nFast),"\r\n",
	"X-Binary-Size-Second-Dimension: ",sprintf("%i",nSlow),"\r\n",
	"X-Binary-Size-Padding: 4095\r\n",
	"\r\n", sep="")
  
  #define the footer
  footer <- "\r\n--CIF-BINARY-FORMAT-SECTION----\r\n;\r\n"
  
  #write the header and start-of-bin bytes
  writeChar(header, f, eos=NULL)
  writeBin(as.raw(c(0x0c, 0x1a, 0x04, 0xd5)), f)

  #write the data
  writeBin(binArr, f)
  
  #write the padding
  writeBin(as.raw(rep.int(0,4095)), f)
  
  #write the footer
  writeChar(footer, f, eos=NULL)
  
  close(f)
}
