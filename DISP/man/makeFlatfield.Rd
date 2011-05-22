\name{makeFlatfield}
\alias{makeFlatfield}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalise a uniform flood image to correct for the area distortion introduced by the FOT
}
\description{
	This function would normally not be called directly by the user, but via a call to
	\code{makeProcessingList}.

	The typical use of this function is when setting up a detector for simulation. This
	function requires calculation of \code{normaliseArray} and a raw dark image \code{darkRaw},
	usually both stored as elements of the list \code{processing} for the current detector.
	
	The purpose of this function is to produce a \emph{flat} field image from a set of \emph{flood} field images by
	correcting them for the image density variation introduced by the fibre optic taper.
}
\usage{
makeFlatfield(xRange, yRange, imageSet, norm = processing$normaliseArray,
	dark = processing$darkRaw)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xRange}{
	Integer pair giving the range of pixels in the horizontal direction, X, within which to calculate the mean pixel value for normalisation.
}
  \item{yRange}{
	Integer pair giving the range of pixels in the vertical direction, Y, within which to calculate the mean pixel value for normalisation.
}
  \item{imageSet}{
	An ensemble of raw flood field images. See \code{\link{makeImageSet}} for details.
}
  \item{norm}{
	The normalisation array. Usually an element of \code{processing}. See \code{\link{makeNormaliseArray}} for details.
}
  \item{dark}{
	A raw dark image. Usually an element of \code{processing}.
}
}
\details{
	Once the flood field images are made \sQuote{flat} and averaged to produce a single flat field image, this image is scaled
	so that the pixel values become normalisation factors for correction of data images. This scaling requires that the pixel
	values in the flat field are divided by the mean pixel value within the exposed region. Because of the FOT distortion, pixels round 
	the border of the image contain no data originating from the phosphor. These should not contribute to the mean as they are not
	exposed pixels. The arguments \code{xRange} and \code{yRange} provide a simple way to exclude the border pixels from the calculation
	of the mean. These arguments are used to specify a rectangular region in the centre of the image. A suitable size for this box should be obtained
	by inspecting the flood images.
}
\value{
	A matrix of pixel value correction factors for use by \code{normaliseImage}.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{processing}},
	\code{\link{makeProcessingList}},
	\code{\link{makeNormaliseArray}},
	\code{\link{normaliseImage}}.
}
\examples{
## None here - see examples for makeProcessingList
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }

