\name{image file i/o}
\alias{readImage}
\alias{readImageBatch}
\alias{readSMV}
\alias{readMar}
\alias{readP6M}
\alias{writeImage}

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
readP6M(filename)
writeImage(imageMatrix, filename)
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
	"Mar", "SMV", or "P6M").
}
  \item{imageMatrix}{
	A matrix with elements corresponding to pixel values.
}
}
\details{
	These functions provide piecemeal image file i/o for \pkg{DISP}. They are not guaranteed to work with your files.
	The text format used by \code{readImage} and \code{writeImage} is simply a tab separated table, oriented to match
	the display produced by \code{disp}, i.e. the origin at the lower left corner.
}
\value{
	For the read* functions, a matrix of pixel values forming an image. For \code{writeImage}, NULL.
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
