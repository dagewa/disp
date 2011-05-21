\name{makePolyGrids}
\alias{makePolyGrids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Form polygon grids illustrating pixels at various points along the detector chain
}
\description{
Form various pixel grids to show their overlap and help to understand how this causes signal spread. These can
then be displayed with poly plot methods. 
}
\usage{
makePolyGrids(normaliseArray = TRUE, detectorFace = TRUE, CCD = TRUE, xRange = detector$xDetRange, yRange = detector$yDetRange)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{normaliseArray}{
Logical. Whether to form the polygon grid corresponding to CCD pixels mapped onto the detector surface back along the FOT.
}
  \item{detectorFace}{
Logical. Whether to form the orthogonal grid of final (corrected) image pixels at the detector face.
}
  \item{CCD}{
Logical. Whether to form the orthogonal grid of CCD pixels.
}
  \item{xRange}{
Integer pair giving the range of pixels in the horizontal direction, X, over which to generate and display pixel polygons.
}
  \item{yRange}{
Integer pair giving the range of pixels in the vertical direction, Y, over which to generate and display pixel polygons.
}
}
\details{
This function is useful for visualising signal spread due to pixel overlaps for the simulated detector module, as
done for the \pkg{DISP} paper. After each poly object is created, it is saved to disk and then deleted from memory. The
file names used are \file{undistGrid.RData}, \file{detFaceGrid.RData} and \file{CCDgrid.RData}.
}
\value{
Returns NULL. Used for the side-effect of saving files to disk.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Not run.
## WARNING: This function may take a long time to compute.
## and requires the list called detector to be set up.
\dontrun{
data(detector)
data(processing)
makePolyGrids()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}

