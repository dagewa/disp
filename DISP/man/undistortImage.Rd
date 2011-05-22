\name{undistortImage}
\alias{undistortImage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Apply distortion correction to an image
}
\description{
	Takes a matrix image and the tables for pixel reapportioning and performs distortion correction
}
\usage{
	undistortImage(imageMatrix, undistortArray = processing$undistortArray, bits = detector$ADCbits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageMatrix}{
	A matrix containing image pixel values.
}
  \item{undistortArray}{
	Calibration data for the geometric distortion correction.
}
  \item{bits}{
	Bit depth of pixels to identify overloads.
}
}
\details{
	This function would not usually be called directly for normal operation of the simulated detector,
	but rather implicitly via a call to \code{correct}. This function is intended to be an intermediate step in overall corrections!
	A default object for \code{undistortArray} is supplied for the default simulated detector in the list \code{processing}.

	The image is \sQuote{undistorted} according to the calibration array \code{undistortArray}. This reapportions
	intensities of distorted image pixels amongst corrected image pixels according to their fractional overlap
	with the orthogonal corrected image pixel grid. The exception are overloaded pixels, for which the overload value
	is propagated to any corrected image pixel that overlaps, no matter what fraction of area that overlap represents.
	Note this may lead to pixel values greater than the overload value after the loop through all
    pixels is complete but usually these would be capped by the function \code{correct}.
}
\value{
	A matrix of pixel values that corresponding to an image that has been corrected for spatial distortion.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{correct}},
	\code{\link{makeUndistortArray}},
	\code{\link{processing}}.
}
\examples{
## Not run.
## WARNING: This may take a long time to compute, and requires the lists called
## detector and processing to be set up.
\dontrun{
data(detector)
data(processing)
rawIm <- read(expose(spot()))
normRawIm <- normaliseImage(rawIm)
corrIm <- undistortImage(normRawIm)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }

