\name{correct}
\alias{correct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply image corrections to a raw image
}
\description{
	If the list \code{processing} has been properly set up, this function calculates an image
	corrected for dark signal, non-uniformity and spatial distortion from a simulated raw image.
}
\usage{
	correct(imageMatrix, bias = processing$newBias)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageMatrix}{
	A matrix containing raw image pixel values.
}
  \item{bias}{
	The offset value to add to the final corrected image, to avoid negative pixel values.
}
}
\details{
	This function applies image corrections in the right order to produce a corrected image
	from a simulated raw image for the current simulated detector set up. This relies on the
	list \code{processing} being in a the search path, which contains calibration data for the
	corrections, as well as the list \code{detector}, describing the detector hardware.
	
	The order of processing steps is:
	\enumerate{
		\item{Subtract a precalculated raw dark image.}
		\item{Normalise pixel values using a precalculated flatfield image.}
		\item{Perform distortion correction using a stored calibration array.}
		\item{Add the bias value.}
		\item{Round pixel values to the nearest integer.}
		\item{Cap data at the overload value.}
	}
}
\value{
	A matrix of pixel values corresponding to a corrected image.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{detector}},
	\code{\link{processing}},
	\code{\link{makeProcessingList}},
	\code{\link{normaliseImage}},
	\code{\link{undistortImage}}.
}
\examples{
## Not run.
## WARNING: This may take a long time to compute, and requires the lists
## called detector and processing to be set up.
\dontrun{
data(detector)
data(processing)
rawIm <- read(expose(spot()))
corrIm <- correct(rawIm)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

