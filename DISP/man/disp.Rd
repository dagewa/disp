\name{disp}
\alias{disp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Display an image in greyscale with suitable contrast
}
\description{
	Plots an image in the format that is typical for diffraction images, that is as greyscale with darker pixels
	indicating greater X-ray intensity.
}
\usage{
disp(imageMatrix, white = NULL, black = NULL, units = "pixels", origin = c(1,1), box = TRUE, asp=1, raster = "auto")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageMatrix}{
	A matrix with elements corresponding to pixel values.
}
  \item{white}{
	Pixel value threshold, at which and below pixels are displayed in white.
}
  \item{black}{
	Pixel value threshold, at which and above pixels are displayed in black.
}
  \item{units}{
	String indicating the choice between \code{"pixels"} and \code{"mm"} as the units for the plot axes.
}
  \item{origin}{
	Integer coodinate pair indicating the index of the pixel at the bottom left of the plot.
}
  \item{box}{
	Logical, whether to draw a box around the plot or not.
}
  \item{asp}{
	Aspect ratio parameter, passed to \code{image}.
}
  \item{raster}{
	String indicating a choice between \code{"auto"}, \code{"yes"} and \code{"no"} as to whether
	or not to use \code{rasterImage} (bitmap) or \code{image} (vector graphics) to draw the plot.
}
}
\details{
	The matrix passed in is typically composed of positive integer values, i.e. ADUs or \dQuote{counts}
	but the function will accept floating point values as well (useful for displaying ratio images). By
	convention, the row dimension is treated as the X (horizontal) axis and the columns as the Y (vertical) axis.
	This is so that indexing an image matrix proceeds with X then Y forming the coordinates. The image is displayed
	as if it were X-ray film viewed from the exposed side (which mimics ADXV).
	
	If values for \code{white} and \code{black} are not passed the function tries to determine suitable values.
	This works fairly well for most diffraction images.
	
	Asking for units in mm implies that the image is one corrected for geometrical distortion as the pixel size at
	the face of the detector, defined in the list \code{detector} is used to calculate distances.
	
	The \code{origin} parameter is provided so that plots of regions of interest of a larger image can be produced
	by passing only the region of interest to the function and explicitly specifying the index of the bottom left pixel.

}
\value{
	Returns NULL. Used for the side-effect of plotting to the current graphics device.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{detector}}
	\code{\link{image}},
	\code{\link{rasterImage}},
	\code{\link{dispContour}},
	\code{\link{dispBox}},	
	\code{\link{flipHorizontal}},
	\code{\link{flipVertical}},
	\code{\link{rotate}}.
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
disp(corrIm[501:520,501:520], origin=c(501,501), units="mm")
}
}	
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{array}

