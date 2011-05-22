\name{quantum images}
\alias{flood}
\alias{spot}
\alias{spotGrid}
\alias{pencil}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a quantum image of X-ray photons
}
\description{
	Generates a list of X-ray photon positions for a either a random uniformly distributed flood field, a \sQuote{Bragg spot}, a grid of such spots, or a point source.
}
\usage{
flood(lambda = 10000, xSize = detector$xDetRange, ySize = detector$yDetRange)
spot(lambda = 1e5, x = mean(detector$xDetRange), y = mean(detector$yDetRange), sdx = 0.1, sdy = 0.1, rho = 0.3)
spotGrid(lambda = 1000, xSize = detector$xDetRange, ySize = detector$yDetRange, x_spacing = 2, y_spacing = 2, sdx = 0.2, sdy = 0.15, rho = 0.3)
pencil(lambda = 1e5, x = mean(detector$xDetRange), y = mean(detector$yDetRange))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda}{
	Expected number of X-ray photon positions to generate.
}
  \item{xSize}{
	Numeric pair giving the range in mm in the x direction within which to generate X-ray photon positions.
}
  \item{ySize}{
	Numeric pair giving the range in mm in the y direction within which to generate X-ray photon positions.
}
  \item{x}{
	Ordinate in mm for the centre of the spot or pencil beam in the X (horizontal) direction.
}
  \item{y}{
	Ordinate in mm for the centre of the spot or pencil beam in the Y (vertical) direction.
}
  \item{sdx}{
	Sigma parameter for X direction defining the spot width.
}
  \item{sdy}{
	Sigma parameter for X direction defining the spot height.
}
  \item{rho}{
	X-Y correlation parameter defining the spot elongation.
}
  \item{x_spacing}{
	Value in mm for the horizontal pitch of the grid for \code{spotGrid}.
}
  \item{y_spacing}{
	Value in mm for the vertical pitch of the grid for \code{spotGrid}.
}
}
\details{	
	The actual number of photon positions returned is chosen from a Poisson distribution with a mean value of \code{lambda}.
	The generated x, y coordinates are in mm and are assumed to be in the x-y plane at the face of the simulated detector for
	input to detector functions such as \code{expose}.
	
	For flood field or spot grid generation, default values for the parameters \code{xSize} and \code{ySize} take the
	range to be the full range of the detector face, determined in the list \code{detector}.
	
	Use of \code{spot} requires the package \pkg{mvtnorm} for generation of bivariate normal deviates.
}
\value{
	A two-column matrix giving the x and y positions in mm of X-ray photons generated at the detector face.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{detector}},
	\code{\link{phosphor}},
	\code{\link{expose}}
}
\examples{
data(detector)
a <- flood(100)
b <- spot(100)
c <- spotGrid(100)
d <- pencil(100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

