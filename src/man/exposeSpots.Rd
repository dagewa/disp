\name{exposeSpots}
\alias{exposeSpots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Exposes the detector with simulated diffraction spots
}
\description{
	Takes a list of incident spot positions and intensities, and exposes the detector with generated spots.
}
\usage{
exposeSpots(intensityList, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{intensityList}{
	A matrix of three columns giving the integer intensity of X-ray photons,
	a position in the X direction in mm and a position in the Y direction in mm for each spot to be generated.
}
  \item{\dots}{
	Parameters to be passed to \code{spot}.
}
}
\details{
	This is a convenience function for generating an image containing multiple simulated diffraction spots that
	have the same profile but different intensities.
}
\value{
	A matrix of values corresponding to numbers of electrons at each pixel position on the CCD chip.
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
	\code{\link{spot}},
	\code{\link{expose}}.
	
}
\examples{
	iList <- cbind(c(1000, 5000, 2000), c(10, 30, 50), c(25, 40, 25))
	spotsRaw <- read(exposeSpots(iList))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

