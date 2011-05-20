\name{dispBox}
\alias{dispBox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Display a region of an image bounded by a measurement box
}
\description{
	Produces a contour plot within a measurement box, with the measurement box
	parameters highlighted.
}
\usage{
	dispBox(imageMatrix, NX, NY, NRX, NRY, NC, xPx = ceiling(NX/2), yPx = ceiling(NY/2))
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{imageMatrix}{
	A matrix with elements corresponding to pixel values.
}
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
	Returns NULL. Used for the side-effect of plotting a region of interest of an image
	with a measurement box.
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
	\code{\link{addBox}},
	\code{\link{dispContour}}.
}

\examples{
	data(detector)
	rawIm <- read(expose(spot()))
	dispBox(rawIm, NX=19, NY=19, NRX=5, NRY=5, NC=11, xPx=513, yPx=513)
}

\keyword{ hplot }

