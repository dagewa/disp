\name{dark}
\alias{dark}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create thermal electron image
}
\description{
	Given an accumulation time and a dark rate, creates a matrix
	consisting of counts of thermally generated electrons at each pixel position.
}
\usage{
	dark(accumulationTime = detector$darkAccumulationTime, darkRate = detector$darkRate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{accumulationTime}{
	Number of seconds to accumulate electrons in CCD wells.
}
  \item{darkRate}{
	Dark rate in electrons per pixel per second.
}
}
\details{
	By default, the accumulation time and dark rate are taken from the list \code{detector}.
	
	The matrix returned is not a raw image. The numbers at each pixel position are in units of
	electrons, not ADUs. This function is the analogue of \code{accumulate} for thermally generated
	electrons rather than those that result from photoelectric excitation.
}
\value{
	A matrix of values corresponding to numbers of electrons at each pixel position.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{accumulate}},
	\code{\link{read}},
	\code{\link{detector}}.
}
\examples{
data(detector)
x <- dark()
}

\keyword{datagen}

