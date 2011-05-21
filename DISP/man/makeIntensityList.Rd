\name{makeIntensityList}
\alias{makeIntensityList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert intensities from MTZ to a format suitable for exposeSpots
}
\description{
Takes the spot intensities and corresponding detector XDET and YDET positions for one image from a data frame
(typically resulting from integration of a dataset by MOSFLM and read into R using \code{readMTZ})
and returns the list of spot intensities in a format suitable for simulation using \code{exposeSpots}.
}
\usage{
makeIntensityList(mtz, batch = 1, numXDET = 4096)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mtz}{
	A data frame with columns \code{BATCH}, \code{XDET}, and \code{YDET}.
}
  \item{batch}{
	Integer giving the image number from which to extract spot intensities.
}
  \item{numXDET}{
	Integer number of pixels on the original image along the direction MOSFLM treats as XDET.
}
}
\details{
This function was developed for a single-use application and may not work correctly outside the scope
of that usage. 
}
\value{
	A matrix of three columns giving the integer intensity of X-ray photons,
	a position in the X direction in mm and a position in the Y direction in mm for each spot to be generated.
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

\section{Warning }{
This function was developed for a single-use application and may not work correctly outside the scope
of that usage.
}

\seealso{
	\code{\link{readMTZ}},
	\code{\link{exposeSpots}}.
}
\examples{
## Not run.
## WARNING: This function requires a data frame formed from reading a batch MTZ file into the environment, which
## is not supplied with this package.
\dontrun{
someMTZobject <- readMTZ("./someMTZfile.mtz")
x <- makeIntensityList(someMTZobject)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{datagen}
