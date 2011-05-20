\name{statistics on image sets}
\alias{pxMean}
\alias{pxVar}
\alias{pxCov}
\alias{pxCor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate statistics on pixel values over an ensemble of images
}
\description{
	Calculate mean, variance, covariance and correlation for pixel values, which are random variates distributed over an ensemble (or set) of images.
}
\usage{
pxMean(imageSet)
pxVar(imageSet)
pxCov(imageSet, covMatrixSize = 3)
pxCor(imageSet, covMatrixSize = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageSet}{
	An ensemble of images. See \code{\link{makeImageSet}} for details.
}
  \item{covMatrixSize}{
	integer depth of neighbours for which to calculate covariance or correlation.
}
}
\details{
The paramater \code{covMatrixSize} determines the local range over which covariance or correlation calculations take place. For example, if \code{covMatrixSize = 3}
then for every pixel \code{pxCov} calculates covariance between that pixel and all others in a 7 by 7 matrix centred on the pixel of interest (3 neighbours deep in each direction).
In those elements of the local covariance or correlation array that traverse out-of-bounds of the image (at the image edges), the value \code{NA} is recorded.
}
\value{
For \code{pxMean}, a matrix of values giving the mean value over the set at each pixel position.
For \code{pxVar}, a matrix of values giving the variance over the set at each pixel position.
For \code{pxCov}, a 4 dimensional array where the first two dimensions specify the pixel position and the second two give elements of a local covariance array, of size \code{2*covMatrixSize+1}.
For \code{pxCor}, a 4 dimensional array where the first two dimensions specify the pixel position and the second two give elements of a local pearson correlation array, of size \code{2*covMatrixSize+1}.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{\code{makeImageSet}}},
	\code{\link{\code{pxIntegrate}}}
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
		meanROI <- pxMean(ROIset) #NB this mean includes the bias offset set in processing$bias
		varROI <- pxVar(ROIset)
		covArrayROI <- pxCov(ROIset)
		corArrayROI <- pxCor(ROIset)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{multivariate}

