\name{linePlot}
\alias{linePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot values along a line across an image
}
\description{
Plot pixel values along a line drawn between two points on an image, and optionally calculate full
width at x% of the maximum value.
}
\usage{
linePlot(imageMatrix, px1 = NULL, px2 = NULL, fwxm = NULL,
	add = FALSE, newCol = "blue", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageMatrix}{
	A matrix with elements corresponding to pixel values.
}
  \item{px1}{
	Integer pixel coordinate pair specifying the first pixel on the line
}
  \item{px2}{
	Integer pixel coordinate pair specifying the last pixel on the line
}
  \item{fwxm}{
	Vector of fractions of the peak height for which to calculate full widths
}
  \item{add}{
	Logical. Whether to add to existing plot or make a new one
}
  \item{newCol}{
	Colour of the points if adding to an existing plot
}
  \item{\dots}{
	Parameters passed to \code{plot} if \code{add == FALSE}
}
}
\details{
If \code{px1 == NULL} (the default) then the function will try to get points interactively, by through mouse clicks on
a plot. Obviously a plot needs to be active for this to work.
}
\value{
returns NULL or invisibly returns the full width at each fraction x of the maximum value if the parameter \code{fwxm} is supplied.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{disp}}
}
\examples{
## Not run.
## WARNING: This may take a long time to compute, and requires the lists
## called detector and processing to be set up.
\dontrun{
data(detector)
data(processing)
rawIm <- read(expose(spot()))
corrIm <- correct(rawIm)
linePlot(corrIm, px1 = c(504,504), px2 = c(520,520), fwxm = c(0.5,0.1), col="blue")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
\keyword{dplot}
\keyword{hplot}
\keyword{iplot}

