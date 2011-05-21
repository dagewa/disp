\name{makeProcessingList}
\alias{makeProcessingList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform steps required to fill the processing list
}
\description{
	Performs the steps required to calculate the calibration data for all
	image corrections, which is normally stored in a list called \code{processing}.
}
\usage{
makeProcessingList(xRange = NULL, yRange = NULL, floodRawFilePattern = NULL, typeString = "edit as appropriate")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xRange}{
	See \code{\link{makeFlatfield}} for details
}
  \item{yRange}{
	See \code{\link{makeFlatfield}} for details
}
  \item{floodRawFilePattern}{
	A character string specifying a set of raw floodfield images to use in calculating
	the flatfield correction. See \code{\link{makeImageSet}} for details.
}
  \item{typeString}{
	A character string that might be used to identify the detector for which the \code{processing}
	list was calculated. This could be anything, for example \dQuote{Rayonix MX type, 2*2 binning, 1024*1024 px}.
	No code in \pkg{DISP} relies on this being set to anything sensible, it is simply for convenience.
}
}
\details{
	The list \code{processing} contains calibration data for the corrections to be performed on raw images.
	If a set of raw floodfield images has been generated, this function automates the steps required to
	build the full \code{processing} list.
	
	This function requires the list of detector parameters, \code{detector}, to have already been set up.
}
\value{
   \item{darkRaw}{A raw dark image for this detector.}
   \item{normaliseArray}{An array of pixel normalisation factors used to correct for non-uniform pixel areas
   			caused by image distortion in the fibre optic taper.}
   \item{undistortArray}{A table describing the one-to-many mappings for distributing pixel values
   			used to perform the distortion correction.}
   \item{flatfield}{An array of pixel normalisation factors describing the non-uniformity correction. This
   		is calculated from a set of floodfield images that have been preprocessed by the \code{normaliseArray}.}
   \item{newBias}{The offset (set to 100) to add to corrected image pixel values to avoid negative numbers.}
   \item{type}{A string describing or naming the detector.}
}
\references{
\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
David G. Waterman.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{detector}},
	\code{\link{processing}},
	\code{\link{makeNormaliseArray}},
	\code{\link{makeUndistortArray}},
	\code{\link{makeFlatfield}}.
}
\examples{
## Not run.
## WARNING: This may take a long time to compute, and writes objects to disk.
\dontrun{
data(detector)

# make 25 flood images with 1500 X-ray photons per pixel on average
flux <- 1500
for(i in 1:25){
  if (.Platform$OS.type == "windows") flush.console() 
  j <- sprintf("%03d",i)
  filenameRaw <- paste("floodRaw", j, sep="")	  
  print.noquote(paste("generating image", i))	  
  raw <- exposeFlood(flux)
  raw <- read(raw)
  assign(eval(filenameRaw), raw)
  writeImage(eval(as.name(filenameRaw)),filename=paste("./", filenameRaw, sep=""))
}
		
processing <- makeProcessingList(c(56,969), c(56,969), "./floodRaw###", detector$type)
save(processing, file="processing.rda")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
