\name{batchGenerate}
\alias{batchGenerate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Control generation of large numbers of replicate images
}
\description{
	This is a convenience function that scripts batch generation of many replicate simulated images,
	which can contain a flat X-ray flood field background
}
\usage{
batchGenerate(start = 1, end = 10, directory = "./data/temp/",
                          prefix = "spot",
                          spotLambda = NULL,
                          spotx = mean(detector$xDetRange),
                          spoty = mean(detector$yDetRange),
                          spotsdx = 0.1, spotsdy = 0.1, spotrho = 0.3,
                          floodFlux = NULL,
                          floodExtent = NULL, # size to generate flood around spot
                          correct = FALSE,
                          saveRawRegionX = NULL,
                          saveRawRegionY = NULL,
                          saveCorrectedRegionX = NULL,
                          saveCorrectedRegionY = NULL,                                                    
                          messages = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{
	Integer giving first image number.
}
  \item{end}{
	Integer giving last image number.
}
  \item{directory}{
	String providing the path for file output.
}
  \item{prefix}{
	String to prepend to image file names.
}
  \item{spotLambda}{
	Integer expected number of X-ray photons forming the spot.
}
  \item{spotx}{
	Integer coordinate in mm for the centre of the spot in the X (horizontal) direction.
}
  \item{spoty}{
	Integer coordinate in mm for the centre of the spot in the Y (vertical) direction.
}
  \item{spotsdx}{
	Integer sigma parameter for X direction used by \code{spot}.
}
  \item{spotsdy}{
	Integer sigma parameter for X direction used by \code{spot}.
}
  \item{spotrho}{
	Integer X-Y correlation parameter used by \code{spot}.
}
  \item{floodFlux}{
	Integer expected number of X-ray photons per pixel for the flood background.
}
  \item{floodExtent}{
	Integer size in mm around the spot centre specifying a square within which to generate
	X-ray flood background photons.
}
  \item{correct}{
	Logical parameter, whether or not to calculate corrected images from the raws.
}
  \item{saveRawRegionX}{
	Integer pair defining the range of pixel coordinates in the X direction for the saved region of raw images.
}
  \item{saveRawRegionY}{
	Integer pair defining the range of pixel coordinates in the Y direction for the saved region of raw images.
}
  \item{saveCorrectedRegionX}{
	Integer pair defining the range of pixel coordinates in the X direction for the saved region of corrected images.
}
  \item{saveCorrectedRegionY}{
	Integer pair defining the range of pixel coordinates in the Y direction for the saved region of corrected images.
}
  \item{messages}{
	Logical parameter, whether or not to print log messages.
}
}
\details{
	The parameters \code{saveRawRegionX}, \code{saveRawRegionY}, \code{saveCorrectedRegionX} and
	\code{saveCorrectedRegionY} are useful for saving disk space when generating large numbers of images.
}
\value{
	NULL returned. This function is used for the side-effect of writing files to disk.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{spot}},
	\code{\link{flood}},
	\code{\link{expose}},
	\code{\link{exposeFlood}},
	\code{\link{correct}},
	\code{\link{writeImage}}.
}
\examples{
## Not run.
## WARNING: This may take a long time to compute, and requires the lists
## called detector and processing to be set up.
\dontrun{
data(detector)
data(processing)
batchGenerate(start = 1, end = 10, directory = "./data/temp/",
prefix = "spot",
spotLambda = 100,
spotx = mean(detector$xDetRange),
spoty = mean(detector$yDetRange),
spotsdx = 0.1, spotsdy = 0.1, spotrho = 0.3,
floodFlux = 10,
floodExtent = 1, # size to generate flood around spot
correct = TRUE,
saveRawRegionX = NULL,
saveRawRegionY = NULL,
saveCorrectedRegionX = NULL,
saveCorrectedRegionY = NULL,                                                    
messages = TRUE)
}
}

\keyword{file}
\keyword{datagen}

