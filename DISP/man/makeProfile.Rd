\name{makeProfile}
\alias{makeProfile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Makes a spot profile
}
\description{
Takes a set of images and integration parameters and calculates a spot profile based on the mean pixel values over
the set. Note this is not a robust profile forming algorithm like that of Mosflm!
}
\usage{
makeProfile(imageSet, NX, NY, NRX, NRY, NC, xPx = ceiling(NX/2),
	yPx = ceiling(NY/2), bias = processing$newBias)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageSet}{
	An ensemble of diffraction spot images. See \code{\link{makeImageSet}} for details.
}
  \item{NX}{
	Integer giving the size of measurement box in pixels in the X (horizontal) direction.
}
  \item{NY}{
	Integer giving the size of measurement box in pixels in the Y (vertical) direction.
}
  \item{NRX}{
	Integer giving the measurement box rim in pixels, describing the border between peak and background regions,
	in the X direction.
}
  \item{NRY}{
	Integer giving the measurement box rim in pixels, describing the border between peak and background regions,
	in the Y direction.
}
  \item{NC}{
	Integer giving the measurement box corner cut-off in pixels, describing the border between peak and background regions.
}
  \item{xPx}{
	Integer giving the pixel coordinate of the centre of the measurement box in the X direction.
}
  \item{yPx}{
	Integer giving the pixel coordinate of the centre of the measurement box in the Y direction.
}
  \item{bias}{
	Integer giving the pixel value offset expected in the images, to be subtracted before profile construction.
}
}
\details{
In order to find suitable profiles for profile fitting integration for the spot simulations published in the referred
article, this function was written to simply combine a number of measurement boxes containing spots by finding their average
at each pixel after background and bias subtraction performed by \code{pxIntegrate}. It does no interpolation to ensure spots
are centred in the measurement boxes, hence it is not suitable for building profiles where the measurement boxes sample each
spot at different positions.
}
\value{
	A matrix of dimension \code{c(NX, NY)} representing integer pixel values resulting from rounding of the mean value
	at each pixel position over the set of supplied images, after bias and background subtraction.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{pxIntegrate}}
}
\examples{
## Not run.
## WARNING: This may take a long time to compute, and requires the lists
## called detector and processing to be set up.
\dontrun{
data(detector)
data(processing)
corrIm01 <- correct(read(expose(spot())))
corrIm02 <- correct(read(expose(spot())))
corrIm03 <- correct(read(expose(spot())))
ROIset <- makeImageSet("corrIm##", c(502,522), c(502,522))
prof <- makeProfile(ROIset, NX=21, NY=21, NRX=2, NRY=3, NC=7)
}
}

\keyword{utilities}

