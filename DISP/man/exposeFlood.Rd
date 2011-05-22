\name{exposeFlood}
\alias{exposeFlood}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Simulation of the detector cascade chain for a flat field image
}
\description{
	Converts from randomly, uniformly distributed incident X-ray photon positions (a quantum image)
	at the detector face to a pixellated image of electrons stored on the CCD.
}
\usage{
exposeFlood(flux = 20000, block = 5e+05, tempName = "exposeFloodTemp",
	xRange = detector$xDetRange, yRange = detector$yDetRange,
	partialImage = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flux}{
	Expected number of X-ray photons per pixel forming the flat field image.
}
  \item{block}{
	Integer number of incident X-ray photons to process at a time.
}
  \item{tempName}{
	String filename for a temporary image.
}
  \item{xRange}{
	Integer pair giving the range of pixels in the horizontal direction, X, over which to generate the flat field.
}
  \item{yRange}{
	Integer pair giving the range of pixels in the vertical direction, Y, over which to generate the flat field.
}
  \item{partialImage}{
	Optional temporary image object from which to restart an interrupted run.
}
}
\details{
	This function provides the utility of \code{expose} for flatfield images without the need to generate a list of incident X-ray
	photon positions. This list could be very long for a flat field image and usually is only required as an intermediate result,
	hence use of this function rather than \code{expose} directly can be convenient.
	
	Generation of an intense flat field image may take a long time, so provision of a simple checkpoint system helps in case
	a run was interrupted. An intermediate result from \code{expose} is saved after each cycle of \code{block} X-ray photons
	is processed. This object stores the cycle number as an attribute, so continuation of an interrupted run is possible by supplying
	exactly the same arguments to the function plus one additional argument: the name of the intermediate result object
	(probably \code{floodRaw}) that has been loaded into the \R environent using \code{load}.
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

\seealso{
	\code{\link{detector}},
	\code{\link{expose}},
	\code{\link{flood}},
	\code{\link{accumulate}},
	\code{\link{read}}.
}
\examples{
## Not run.
## WARNING: This function may take a long time to compute. It writes temporary
## results to disk and requires the list called detector to be set up.
\dontrun{
data(detector)
electronImage <- exposeFlood(flux=10)
}
}

\keyword{datagen}

