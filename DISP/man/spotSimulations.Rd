\name{spotSimulations}
\alias{spotSimulations}
\docType{data}
\title{
	Various objects used during published spot simulations
}
\description{
	This dataset contains 8 objects used during the integration of simulated diffraction
	spots as published in the referred article.
}
\usage{data(spotSimulations)}
\format{
	This dataset preserves 8 objects saved from the R environment with \code{save} which were used during spot
	integration for the \pkg{DISP} paper. These objects may also be used by the examples in other documentation
	files. The objects that are restored by this dataset are:
	
	\describe{
		\item{profZoneACorrected}{Spot profile for spots incident in zone A on corrected images}
		\item{profZoneARaw}{Spot profile for spots incident in zone A on raw images}
		\item{profZoneBCorrected}{Spot profile for spots incident in zone B on corrected images}
		\item{profZoneBRaw}{Spot profile for spots incident in zone B on raw images}
		\item{varcovZoneACor}{Variance-covariance matrix for the zone A measurement box on corrected images}
		\item{varcovZoneARaw}{Variance-covariance matrix for the zone A measurement box on raw images}
		\item{varcovZoneBCor}{Variance-covariance matrix for the zone B measurement box on corrected images}
		\item{varcovZoneBRaw}{Variance-covariance matrix for the zone B measurement box on raw images}
	}
}
\details{
	These objects were supplied as paramaters to the \code{pxIntegrate} function to integrate simulated spots
	as part of the investigation published in the referred article. See the examples for code illustrating spot
	simulation and integration in \dQuote{zone A} and \dQuote{zone B} of the simulated detector.
}
\source{
	Calculated for the simulated detector.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\examples{
data(detector)
data(processing)
data(spotSimulations)

#simulate spots

rawA <- expose(spot(lambda = 10000, x = 306 * detector$xPxSizeFace,
                    y = 306 * detector$yPxSizeFace, sdx = 0.1,
                    sdy = 0.1, rho = 0.3))
rawA <- rawA + exposeFlood(flux = 20)
rawA <- read(rawA)
corrA <- correct(rawA)

rawB <- expose(spot(lambda = 10000, x = 300 * detector$xPxSizeFace,
                    y = 300 * detector$yPxSizeFace, sdx = 0.1,
                    sdy = 0.1, rho = 0.3))
rawB <- rawB + exposeFlood(flux = 20)
rawB <- read(rawB)
corrB <- correct(rawB)

#'uncompress' variance-covariance matrices using flatten

dummy <- matrix(data = 1, ncol = 23, nrow = 23)
varcovZoneACor <- flatten(dummy, dummy, varcovZoneACor)
varcovZoneBCor <- flatten(dummy, dummy, varcovZoneBCor)

dummy <- matrix(data = 1, ncol = 21, nrow = 21)
varcovZoneARaw <- flatten(dummy, dummy, varcovZoneARaw)
varcovZoneBRaw <- flatten(dummy, dummy, varcovZoneBRaw)

#perform integrations

pxIntegrate(rawA, type="mosflm", NX=21, NY=21, NRX=4, NRY=5, NC=11, xPx=332,
	yPx=332, bias=500, profile=profZoneARaw)
pxIntegrate(corrA, type="mosflm", NX=23, NY=23, NRX=5, NRY=4, NC=11, xPx=306,
	yPx=306, bias=100, profile=profZoneACorrected)                        
pxIntegrate(rawB, type = "mosflm",  NX=21, NY=21, NRX=4, NRY=5, NC=11, xPx=326,
	yPx=326, bias=500, profile=profZoneBRaw)
pxIntegrate(corrB, type="mosflm", NX=23, NY=23, NRX=5, NRY=4, NC=11, xPx=300,
	yPx=300, bias=100, profile=profZoneBCorrected)

pxIntegrate(rawA, type="full", NX=21, NY=21, NRX=4, NRY=5, NC=11, xPx=332,
	yPx=332, bias=500, profile=profZoneARaw, Mobs=varcovZoneARaw)
pxIntegrate(corrA, type="full", NX=23, NY=23, NRX=5, NRY=4, NC=11, xPx=306,
	yPx=306, bias=100, profile=profZoneACorrected, Mobs=varcovZoneACor)
pxIntegrate(rawB, type="full", NX=21, NY=21, NRX=4, NRY=5, NC=11, xPx=326,
	yPx=326, bias=500, profile=profZoneBRaw, Mobs = varcovZoneBRaw)
pxIntegrate(corrB, type="full", NX=23, NY=23, NRX=5, NRY=4, NC=11, xPx=300,
	yPx=300, bias=100, profile=profZoneBCorrected, Mobs=varcovZoneBCor)                        
}
\keyword{datasets}
