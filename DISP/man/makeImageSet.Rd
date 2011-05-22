\name{makeImageSet}
\alias{makeImageSet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an image set
}
\description{
The function combines multiple images, which are either stored as objects in the environment or on disk,
and creates a single image ensemble object. This image set is a 3-dimensional array thus can be used to
investigate pixel values both spatially and temporally (for example).
}
\usage{
makeImageSet(string, xRange = NULL, yRange = NULL, type = "ASCII")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{
	Pattern to match filenames or objects on the search path. See \code{\link{getFilePattern}} for details.
}
  \item{xRange}{
	Integer pair giving the range of pixels in the horizontal direction, X, defining the region of interest on the images.
}
  \item{yRange}{
	Integer pair giving the range of pixels in the vertical direction, Y, defining the region of interest on the images.
}
  \item{type}{
	String specifying the format of images to be read from disk, currently either \dQuote{ASCII}, \dQuote{Mar},
	\dQuote{SMV}, or \dQuote{P6M}.
}
}
\details{
	Many functions for image statistics
	expect data as ensembles of images of the type produced by \code{makeImageSet}. See the links
	below.
	
}
\value{
	A 3-dimensional array containing pixel values from the region of interest defined by
	\code{xRange} and \code{yRange} for multiple images stacked in the third dimension.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{getFilePattern}} for details of the string pattern matching,
	\code{\link{makeProcessingList}} which uses \code{makeImageSet}, and the functions
	\code{\link{makeProfile}},
	\code{\link{makeVOMimage}},
	\code{\link{pxMean}},
	\code{\link{pxVar}},
	\code{\link{pxCov}},
	\code{\link{pxCor}} which expect an \code{imageSet} as an argument.
}
\examples{
## Not run.
## WARNING: This may take a long time to compute.
\dontrun{
data(detector)
rawIm01 <- read(expose(spot()))
rawIm02 <- read(expose(spot()))
rawIm03 <- read(expose(spot()))
ROIset <- makeImageSet("rawIm##", c(501,520), c(501,520))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{manip}
