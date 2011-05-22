\name{taper}
\alias{taper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Minify a quantum image by transformation via the Fibre Optic Taper (FOT)
}
\description{
Given a quantum image, consisting of X, Y coordinates of photons, produces the equivalent positions
at the small end of the simulated FOT, which shrinks the image and introduces spatial
distortion.
}
\usage{
taper(input, taperBleed = detector$taperBleed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
	A two-column matrix giving the x and y positions in mm of light photons incident
	on the large face of the FOT.
}
  \item{taperBleed}{
	A parameter describing the FOT contribution to the PSF.
}
}
\details{
The taper parameters that control both the strength of minification as well as the spatial distortion are
stored for the simulated hardware in the list called \code{detector}.

The \code{taperBleed} parameter is used as the standard deviation of a Gaussian random deviate, which determines
the radius of \sQuote{jitter} introduced to the photon positions. The PSF broadening in the taper this causes is not
directly modelled on real observations, but the overall effect is to modify the PSF such that it better matches
observed values for the complete detector system.
}
\value{
	A two-column matrix giving the x and y positions in mm of light photons at the positions they exit the small
	face of the FOT.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{detector}},
	\code{\link{expose}},
	\code{\link{exposeSpots}},
	\code{\link{exposeFlood}}.
}
\examples{
data(detector)
x <- taper(phosphor(spot(10)))
}

\keyword{datagen}
