\name{addBox}
\alias{addBox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a measurement box to the current plot
}
\description{
	Given the parameters describing a measurement box, draws the box on the current plot
	(usually one created by \code{disp}).
}
\usage{
addBox(NX, NY, NRX, NRY, NC, xPx = ceiling(NX/2), yPx = ceiling(NY/2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NX}{
	integer giving the size of measurement box in pixels in the X (horizontal) direction.
}
  \item{NY}{
	integer giving the size of measurement box in pixels in the Y (vertical) direction.
}
  \item{NRX}{
	integer giving the measurement box rim in pixels, describing the border between peak and background regions,
	in the X direction.
}
  \item{NRY}{
	integer giving the measurement box rim in pixels, describing the border between peak and background regions,
	in the Y direction.
}
  \item{NC}{
	integer giving the measurement box corner cut-off in pixels, describing the border between peak and background regions.
}
  \item{xPx}{
	integer giving the pixel coordinate of the centre of the measurement box in the X direction.
}
  \item{yPx}{
	integer giving the pixel coordinate of the centre of the measurement box in the Y direction.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	Returns NULL. Used for the side-effect of decorating a plot with a measurement box.
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
	\code{\link{disp}},
	\code{\link{dispBox}},
	\code{\link{dispContour}}.
}
\examples{
data(detector)
data(processing)
rawIm <- read(expose(spot(lambda=750, x=25, y=25)))
corIm <- correct(rawIm)
disp(corIm[327:357,327:357],origin=c(327,327))
addBox(NX=17, NY=17, NRX=5, NRY=3, NC=10, xPx=342, yPx=342)
}

\keyword{aplot}

