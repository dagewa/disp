\name{read}
\alias{read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate CCD read out
}
\description{
	Adds noise taken from a normal distribution to a matrix representing numbers
	of electrons in the CCD chip wells. The resulting numbers are converted to ADUs
	using the detector gain and a bias added to help ensure that the pixel values
	are positive.
}
\usage{
read(imageMatrix, noise = detector$readNoise, gain = detector$ADCgain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageMatrix}{
	A matrix containing electron counts at each CCD pixel position.
}
  \item{noise}{
	Standard deviation of the normal distribution used to model read noise (in electrons).
}
  \item{gain}{
	The ADC gain in number of ADUs per electron.
}
}
\details{
	The \code{noise} and \code{gain} parameters are usually stored for a particular simulated detector in the list \code{detector}.
}
\value{
	A matrix of values corresponding to ADUs at each CCD pixel position, i.e. a raw image.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{dark}}
	\code{\link{accumulate}},
	\code{\link{detector}}.
}

\examples{
data(detector)
x <- dark()
x <- read(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

