\name{readMTZ}
\alias{readMTZ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read an MTZ file from disk
}
\description{
	Read an MTZ file from disk and return a data frame
}
\usage{
readMTZ(filename, messages = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
	String determining the path and filename of the MTZ.
}
  \item{messages}{
	Logical parameter, whether or not to print log messages (similar to MTZDUMP output). 
}
}
\details{
	Basic function for reading an MTZ file and returning a data frame for manipulation in R.
	Information from the MTZ header is retained and stored along with the data frame as an
	attribute.
	This function may fail for certain valid MTZ files. In particular, files with a byte order
	that differs from the current architecture. See \pkg{cRy} for more comprehensive MTZ file
	handling.
}
\value{
Data frame containing named columns and a list \code{mtzHeader} attached as an attribute.
	\item{H}{Integer column of reflection H indices}
	\item{K}{Integer column of reflection K indices}
	\item{L}{Integer column of reflection L indices}
	\item{BATCH}{Integer column of reflection BATCH numbers (if present)}
	\item{others}{Other columns present in the MTZ file, stored as "numeric"}
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{makeIntensityList}}
}
\examples{
#None right now
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{file}
