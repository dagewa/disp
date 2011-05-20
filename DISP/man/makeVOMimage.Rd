\name{makeVOMimage}
\alias{makeVOMimage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a Variance Over Mean image from a set of images
}
\description{
Given a set of images, for every pixel position the variance to mean ratio of the pixel values is calculated.
This quantity is also called the index of dispersion, coefficient of dispersion or similar. The results are
returned in the form of an image, with the value of this quantity at each pixel position.
}
\usage{
makeVOMimage(imageSet, bias = processing$newBias)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageSet}{
	An ensemble of images. See \code{\link{makeImageSet}} for details.
}
  \item{bias}{
	Integer offset value to be subtracted from all pixels.
}
}
\details{
	Subtraction of any offset is required to ensure the mean pixel value is only affected by data originating
	from X-ray events. In that case, the variance to mean ratio is expected to approach a constant value that
	is informative about the detector response.
	
	This function is a very simple wrapper around the functions \code{pxVar} and \code{pxMean}. Infinite values,
	which come from dividing by a mean of zero, are replaced by \code{NaN}.
}
\value{
A matrix of values corresponding to the variance over mean ratio or dispersion index at each pixel position.
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
	\code{\link{pxMean}},
	\code{\link{pxVar}},
}
\examples{
	## Not run.
	## WARNING: This may take a long time to compute, and requires the lists called detector and processing to be set up.
	\dontrun{
		data(detector)
		data(processing)
		corrIm01 <- correct(read(expose(spot())))
		corrIm02 <- correct(read(expose(spot())))
		corrIm03 <- correct(read(expose(spot())))
		corrIm04 <- correct(read(expose(spot())))
		corrIm05 <- correct(read(expose(spot())))
		corrIm06 <- correct(read(expose(spot())))
		corrIm07 <- correct(read(expose(spot())))
		corrIm08 <- correct(read(expose(spot())))
		corrIm09 <- correct(read(expose(spot())))
		corrIm10 <- correct(read(expose(spot())))
		ROIset <- makeImageSet("corrIm##", c(502,522), c(502,522))
		VOM <- makeVOMimage(ROIset)
	}
}

\keyword{ manip }
