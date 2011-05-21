\name{makeNormaliseArray}
\alias{makeNormaliseArray}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an array for correction of a flood field image for pixel area non-uniformity
}
\description{
	This function would normally not be called directly by the user, but via a call to
	\code{makeProcessingList}.

	The typical use of this function is when setting up a detector for simulation. It makes a calibration
	array based on the current \code{detector} parameters consisting of pixel normalisation
	factors used to correct for non-uniform pixel areas caused by image distortion in the fibre optic taper.
}
\usage{
makeNormaliseArray()
}
%- maybe also 'usage' for other objects documented here.
\details{
	The geometric distortion caused by the fibre optic taper has a side effect of causing variations in raw
	image density, as well as spatial distortion of an image. The reason for this is that CCD pixels have different
	effective collection areas when mapped back along the taper to the detector face. In other words, the spatial distortion
	is non-area-preserving. Although application of the distortion correction \sQuote{undoes} this aspect of non-uniformity for corrected
	images, we need to explicitly correct for it when producing a raw flat field image from flood field data. Even if a flood
	field source is perfectly uniform, the recorded raw image will not be flat, due to the density variation. As the flat
	field image required for non-uniformity correction should be a raw image, and not a distortion corrected image, we need
	to normalise the flood field data to account for the density variation.
	
	For normal use of the detector simulation, this calibration array is calculated during the setting up phase of the \code{processing}
	list, and used only once in order to produce a suitable flat field image for non-uniformity correction. It is no longer
	needed as part of data image corrections, as application of the distortion correction implicitly deals with this aspect of non-unformity,
	as mentioned above.	
}
\value{
	A matrix the same size as an image containing the ratio of each undistorted pixel's area
	with the mean undistorted pixel area, so that a uniform flood field image can be normalised by 
	makeFlatfield().
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
## WARNING: This may take a long time to compute, and requires the list called detector to be set up.
\dontrun{x <- makeNormaliseArray()}
}

\keyword{utilities}

