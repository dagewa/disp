\name{findSpotCentroid}
\alias{findSpotCentroid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Find the centre of gravity within a measurement box
}
\description{
	Given an image matrix and measurement box parameters, this function calculates the centre of
	gravity of the peak pixel values after a background subtraction. The background is estimated
	in the same way as pxIntegrate.
}
\usage{
	findSpotCentroid(imageMatrix, NX, NY, NRX, NRY, NC, xPx, yPx, messages = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageMatrix}{
	A matrix with elements corresponding to pixel values.
}
  \item{NX}{
	integer giving the size of measurement box in pixels in the X (horizontal) direction.
}
  \item{NY}{
	integer giving the size of measurement box in pixels in the Y (vertical) direction.
}
  \item{NRX}{
	integer giving the measurement box rim in pixels, describing the border between peak and background regions,
	in the X direction.
}
  \item{NRY}{
	integer giving the measurement box rim in pixels, describing the border between peak and background regions,
	in the Y direction.
}
  \item{NC}{
	integer giving the measurement box corner cut-off in pixels, describing the border between peak and background regions.
}
  \item{xPx}{
	integer giving the pixel coordinate of the centre of the measurement box in the X direction.
}
  \item{yPx}{
	integer giving the pixel coordinate of the centre of the measurement box in the Y direction.
}
  \item{messages}{
	Logical parameter, whether or not to print log messages.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	A coordinate pair giving the centre of gravity as a shift in pixels relative to \code{xPx}, \code{yPx}.
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
	\code{\link{findSpot}}.
}
\examples{
## Not run.
## WARNING: This may take a long time to compute, and requires the lists called detector and processing to be set up.
\dontrun{
data(detector)
data(processing)
rawIm <- read(expose(spot()))
corrIm <- correct(rawIm)
findSpotCentroid(corrIm, NX=19, NY=21, NRX=3, NRY=4, NC=9, xPx=513, yPx=513)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

