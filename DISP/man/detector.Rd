\name{detector}
\alias{detector}
\docType{data}
\title{
	Parameters describing the detector module
}
\description{
	This list is used to describe the simulated detector module hardware for creation of raw images
	for the standard simulated detector of \dQuote{Rayonix MX type, 2*2 binning, 1024*1024 px} type.
}
\usage{data(detector)}
\format{
	A list describing detector hardware:
	\describe{
		\item{type}{An arbitrary string naming the detector}
		\item{xCentre}{Horizontal position of the centre of the module in mm}
		\item{yCentre}{Vertical position of the centre of the module in mm}
		\item{ccdX}{Horizontal position of the origin corner of the CCD chip in mm}
		\item{ccdY}{Vertical position of the origin corner of the CCD chip in mm}
		\item{xPxSize}{Horizontal size of a hardware-binned CCD pixel in mm}
		\item{yPxSize}{Vertical size of a hardware-binned CCD pixel in mm}
		\item{xPxNum}{Number of hardware-binned CCD pixels in the horizontal direction}
		\item{yPxNum}{Number of hardware-binned CCD pixels in the vertical direction}
		\item{xDetRange}{Pair of values giving the range of the active region at the detector face in the horizontal direction in mm}
		\item{yDetRange}{Pair of values giving the range of the active region at the detector face in the vertical direction in mm}
		\item{xPxSizeFace}{Horizontal size of a corrected image pixel at the detector face in mm}
		\item{yPxSizeFace}{Horizontal size of a corrected image pixel at the detector face in mm}
		\item{PSF}{A two-column matrix describing the phosphor Point Spread Function in terms of radial probabilities for generated light photon positions}
		\item{taperA}{First order polynomial coefficient for radial distortion}
		\item{taperB}{Second order polynomial coefficient for radial distortion}
		\item{taperC}{Third order polynomial coefficient for radial distortion}
		\item{taperBleed}{Parameter describing Fibre Optic Taper contribution to the PSF}
		\item{bias}{ADC bias offset in ADUs}
		\item{darkRate}{Number of thermally generated electrons per pixel per second}
		\item{darAccumulationTime}{Default accumulation time used simulated images}
		\item{QDE}{The phosphor Quantum Detection Efficiency}
		\item{PHS}{A two-column matrix describing the phosphor Pulse Height Spectrum describing the intensity distribution of light output}
		\item{phosphorAmplification}{Expected number of light photons per absorbed X-ray, calculated from the PHS}
		\item{phosphorNoise}{Standard deviation of the PHS distribution}
		\item{transmission}{Total transmission probability of a light photon from phosphor to CCD well}
		\item{ADCgain}{number of ADUs per electron}
		\item{ADCbits}{Dynamic range of the ADC}
		\item{readNoise}{Standard deviation of the normal distribution used to model ADC read noise, in electrons}
	}
}

\details{
	This list is required for many of the detector simulation functions in \pkg{DISP}. Default values for a simulation
	based approximately on a Rayonix MX series detector module are provided with the package in the \file{data} directory
	and can be loaded with the command \code{data(detector)}. The default \code{PSF} and \code{PHS} data are the result
	of Monte Carlo simulations carried out by Panagiotis Liaparinos. See the referred article for details.
}
\source{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\references{
	Based on information from various sources. Please see the reference above.
}
\seealso{
	\code{\link{processing}}.
}
\examples{
	data(detector)
}
\keyword{datasets}
