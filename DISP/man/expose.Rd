\name{expose}
\alias{expose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation of the detector cascade chain
}
\description{
	Converts from incident X-ray photon positions (a quantum image) to a pixellated image of electrons stored on the CCD
}
\usage{
expose(Xph, block = 2e+05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xph}{
	A two-column matrix giving the x and y positions in mm of X-ray photons incident
	on the detector face.
}
  \item{block}{
	The number of rows of \code{Xph} to process at a time.
}
  \item{\dots}{
	Parameters to pass to \code{phosphor}.
}
}
\details{
	This function provides a high level interface to normal operation of the simulated detector. Within the function,
	X-ray photon positions at the detector face are processed by \code{phosphor} to generate light
	photons, which are transmitted to the chip using \code{taper} and collected to form a pixellised
	electron image at the CCD chip using \code{accumulate}. This image can then be returned from the
	detector as a raw image using \code{read}.
	
	The parameter \code{block} is useful to reduce the memory requirements for very long lists of
	X-ray photon positions by splitting the list up into blocks that are processed in turn.
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
	\code{\link{detector}},
	\code{\link{phosphor}},
	\code{\link{taper}},
	\code{\link{accumulate}},
	\code{\link{read}}.
	\code{\link{exposeFlood}}.
	\code{\link{exposeSpots}}.
	
}
\examples{
data(detector)
electronImage <- expose(spot())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }

