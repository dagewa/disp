\name{makeUndistortArray}
\alias{makeUndistortArray}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates the calibration table used for geometrical distortion correction
}
\description{
	This function would normally not be called directly by the user, but via a call to
	\code{makeProcessingList}.

	The typical use of this function is when setting up a detector for simulation. This
	function relies on parameters in the list \code{detector} being defined. Using these
	parameters, the distortion correction is calculated for each CCD pixel so that the
	values recorded in a raw image can be apportioned between the pixels of a final corrected
	image. The return value	should normally be named \code{undistortArray} and stored in the
	list called \code{processing}.
	
	The calculation requires the package \pkg{gpclib}.
}
\usage{
makeUndistortArray()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	The calibration data is returned as a two-dimensional list, with one element per pixel of
	the CCD chip. Each element consists of a list of two elements, \code{pixel}, a two-dimensional
	coordinate, and \code{matrix}, a 3*3 matrix. The pixel coordinate gives the indices of the
	pixel defined at the detector face to which the centroid of the CCD pixel in question maps
	after distortion correction. The matrix gives the fractional overlaps between the distortion
	corrected CCD pixel and the 9 pixels in the cluster centred at \code{pixel}.
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
	\code{\link{processing}},
	\code{\link{makeProcessingList}}.
}
\examples{
## Not run.
## WARNING: This may take a long time to compute, and requires the list called detector to be set up.
\dontrun{
	x <- makeUndistortArray()
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

