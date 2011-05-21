\name{exposeSpots}
\alias{exposeSpots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Expose the detector with simulated diffraction spots
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
# this example produces a logo image for the package
data(detector)
data(processing)

x <- c(1, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 7, 7, 7, 7, 7, 9, 9, 10,
   10, 10, 11, 11, 11, 12, 12, 12, 13, 13, 15, 15, 15, 15, 15, 16, 16, 17,
   17, 18, 18, 19)*0.5 + 1
y <- c(1, 2, 3, 4, 5, 1, 5, 1, 5, 1, 5, 2, 3, 4, 1, 2, 3, 4, 5, 1, 4, 1,
   3, 5, 1, 3, 5, 1, 3, 5, 2, 5, 1, 2, 3, 4, 5, 3, 5, 3, 5, 3, 5, 4)*0.5 + 1

iList <- cbind(rpois(44, 750), x, y)
im <- correct(read(exposeSpots(iList)))
logo <- im[1:161,1:70]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

