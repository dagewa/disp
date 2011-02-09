\name{makeOverlapArray}
\alias{makeOverlapArray}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a table describing how signal is spread due to the distortion correction.
}
\description{
	This function would normally not be called directly by the user, but via a call to
	\code{makeProcessingList}.

	The typical use of this function is when setting up a detector for simulation.
	It makes a calibration table for the effect of distortion correction on signal
	spreading based on the current \code{detector} parameters.
	
	The calculation is similar to that of \code{makeUndistortArray}.
}
\usage{
makeOverlapArray()
}
%- maybe also 'usage' for other objects documented here.
\details{
	This function relies on parameters in the list \code{detector} being defined.

	The calculation requires the package \pkg{gpclib}.
	
	This function is for experimental/testing purposes in the investigation for improved
	methods for profile fitting integration.
}
\value{
	The table is returned as a two-dimensional list, with one element per pixel of the detector.
	Each element consists of a 3*3 matrix with values describing the degree to which signal 
	incident at the pixel in question is spread to its neighbours by the distortion correction.
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
	\code{\link{makeUndistortArray}}.
}
\examples{
## WARNING: This may take a long time to compute, and requires the list called detector to be set up.
	\dontrun{x <- makeOverlapArray()}
}

\keyword{utilities}
