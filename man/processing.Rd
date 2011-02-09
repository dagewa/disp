\name{processing}
\alias{processing}
\docType{data}
\title{
	Parameters describing image corrections
}
\description{
	This list is used to describe the simulated detector module image corrections and processing
	for producing non-uniformity and distortion corrected images from raw images returned by \code{read}
	for the standard simulated detector of \dQuote{Rayonix MX type, 2*2 binning, 1024*1024 px} type.
}
\usage{data(processing)}
\format{
	A list describing calibration data for corrections:
	\describe{
		\item{darkRaw}{A raw dark image used for dark subtraction}
		\item{normaliseArray}{A matrix containing factors to normalise an image (e.g. a flood field)
			to correct for image density variations introduced by fibre optic taper distortion}
		\item{undistortArray}{Calibration data for the geometric distortion correction}
		\item{flatfield}{A matrix of normalisation factors to effect non-uniformity correction}
		\item{newBias}{An offset to add to pixel values after corrections in order to avoid negative numbers}
		\item{type}{An arbitrary string naming the detector}
	}
}
\details{
	This list is required for many of the detector simulation functions in \pkg{DISP}. Default values for a simulation
	based approximately on a Rayonix MX series detector module are provided with the package in the \file{data} directory
	and can be loaded with the command \code{data(processing)}.
}
\source{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\references{
	Based on information from various sources. Please see the reference above.
}
\seealso{
	\code{\link{detector}}
	\code{\link{makeProcessingList}}.
}
\examples{
	data(processing)
}
\keyword{datasets}
