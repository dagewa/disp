\name{addBox}
\alias{addBox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a measurement box to the current plot. 
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
	rawIm <- read(expose(spot()))
	disp(rawIm[491:530,491:530], origin=c(491,491))
	addBox(NX=19, NY=19, NRX=5, NRY=5, NC=11, xPx=513, yPx=513)
}

\keyword{aplot}

