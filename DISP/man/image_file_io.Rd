\name{image file i/o}
\alias{readImage}
\alias{readImageBatch}
\alias{readSMV}
\alias{readMar}
\alias{readCBF}
\alias{writeImage}
\alias{writeCBF}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read and write image files in various formats from/to disk
}
\description{
	Read image files in various formats and write (currently only to a text file).
}
\usage{
readImage(filename, type = "integer")
readImageBatch(string, envir = .GlobalEnv, type = "ASCII")
readSMV(filename)
readMar(filename)
readCBF(filename)
writeImage(imageMatrix, filename)
writeCBF(imageMatrix, filename, binLenMultiplier = 4, xPxSize=NULL, yPxSize=NULL, exposureTime=NULL, exposurePeriod=NULL,
    overload=NULL, wavelength=NULL, detectorDistance=NULL, beamX=NULL, beamY=NULL, phiStart=NULL, phiIncrement=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
	String determining image file name.
}
  \item{string}{
	String pattern used by \code{getFilePattern}.
}
  \item{envir}{
	Environment in which to create image matrices.
}
  \item{type}{
	For \code{readImage}, string specifying whether the pixel values are read as "integer" or "double".
	For \code{readImageBatch}, string specifying one of the known image file formats (currently "ASCII",
	"Mar", "SMV", or "CBF").
}
  \item{imageMatrix}{
	A matrix with elements corresponding to pixel values.
}
  \item{binLenMultiplier}{
	Integer parameter for \code{writeCBF} controlling the space allocated for compression of image data
	using the CBF \sQuote{byte_offset} scheme.
}
  \item{Other arguments to \code{writeCBF}}{
	Values to write to the image header. If \code{NULL} (the default), \code{writeCBF} will first try
	to extract values from a \code{"metadata"} attribute of \code{imageMatrix} and if nothing is available
	will make up values in order to produce a reasonable (looking) header. The fallback defaults are currently:
	\code{xPxSize = 0.000172,
	yPxSize = 0.000172,
	exposureTime = 1.0,
	exposurePeriod = 1.0,
	overload = 1000000,
	wavelength = 1.0,
	detectorDistance = 1.0,
	beamX = nrow(imageMatrix)/2,
	beamY = ncol(imageMatrix)/2,
	phiStart = 0.0,
	phiIncrement = 0.1}
}
}
\details{
	These functions provide piecemeal image file i/o for \pkg{DISP}. They are not guaranteed to work with your files.
	The text format used by \code{readImage} and \code{writeImage} is simply a tab separated table, oriented to match
	the display produced by \code{disp}, i.e. the origin at the lower left corner.
}
\value{
	For the read* functions, a matrix of pixel values forming an image. For write* functions, \code{NULL}.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{getFilePattern}}
}
\examples{
#None right now
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{file}
