\name{pxIntegrate}
\alias{pxIntegrate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Integration of diffraction spots
}
\description{
Performs summation integration and profile fitting of a spot within a measurement box defined
as per Mosflm. Errors are estimated using both the typical assumptions of Poisson statistics plus
pixel independence, and using the new \sQuote{cascade} model of the detector response. For profile
fitting, pixel correlations can be accounted for by including off-diagonal elements of the
variance-covariance array \code{Mobs}.
}
\usage{
pxIntegrate(imageMatrix, type = "mosflm", NX, NY, NRX, NRY, NC, xPx = ceiling(NX/2), yPx = ceiling(NY/2),
bias = processing$newBias, gain = NULL, profile = NULL, corArray = NULL, VOMimage = NULL, covArray = NULL,
Mobs = NULL, makeProfile = FALSE, messages = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageMatrix}{
	A matrix containing the image pixel values that are to be integrated.
}
  \item{type}{
	A string determining the method for obtaining the variance-covariance array \code{Mobs}.
	The default is \code{"mosflm"}. Other options are \code{"covArray"}, \code{"corArray"} or \code{"full"}.
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
	integer giving the pixel coordinate (on \code{imageMatrix}) of the centre of the measurement box in the X direction.
}
  \item{yPx}{
	integer giving the pixel coordinate (on \code{imageMatrix}) of the centre of the measurement box in the Y direction.
}
  \item{bias}{
	A constant offset to subtract from the image prior to integration.
}
  \item{gain}{
	The detector gain (in ADUs per \emph{absorbed} X-ray photon).
}
  \item{profile}{
	A matrix of dimension \code{c(NX, NY)} containing integer pixel values representing a spot
	profile for the specified measurement box.
}
  \item{corArray}{
	An NX*NY*n*n array that for each of the NX*NY pixels stores the n*n array describing local
	correlations between that pixel and its neighbours.
}
  \item{VOMimage}{
	A matrix of values corresponding to the variance over mean ratio or dispersion index at each
	pixel position, for the whole \code{imageMatrix}.
}
  \item{covArray}{
	An NX*NY*n*n array that for each of the NX*NY pixels stores the n*n array describing local
	covariances between that pixel and its neighbours.
}
  \item{Mobs}{
	A square variance-covariance matrix of dimension \code{c((NX*NY), (NX*NY))}, containing
	covariance elements for every pixel in the measurement box.
}
  \item{makeProfile}{
	Set to \code{TRUE} if \code{pxIntegrate} is called from \code{makeProfile} in order to just
	return the bias and background corrected spot profile, and not to integrate.
}
  \item{messages}{
	Logical parameter, whether or not to print log messages.
}
}
\details{
This monolitihic integration function evolved into its current form as the ideas in the referred article developed,
without a great degree of planning or design. It is long overdue a refactoring exercise. The individual tasks should
be broken apart into separate functions, which would greatly assist attempts to develop new integration protocols.
If I ever get time for future versions of \pkg{DISP}, this job takes first priority.

The parameters NX, NY, NRX, NRY and NC define a measurement box with mm symmetry, in the same way as
Mosflm. The summation integration and profile fitting algorithm follows that of Mosflm, with additional
options for error estimation, as detailed in the referred article.

The standard error for summation integration is calculated by summing the covariances between pixels,
treating peak and background pixels separately. In the case of \code{"mosflm"} type integration, Mobs
is a diagonal matrix consisting only of variances calculated assuming Poission statistics
(the expression for error then coincides with equation 11 in A. Leslie's 1999 Mosflm paper).
As well as assuming Poisson statistics, the variance estimates are  also calculated 
using the cascade and pixel noise model in a separate calculation.
For the other types of integration some off-diagonal covariance elements are also included.

If a spot profile is passed in, as well as summation integration, pxIntegrate also performs profile fitting by least squares minimisation.
The full variance-covariance array (in units of ADU^2) for the measurement box is either passed in as Mobs (\code{type = "full"}),
expanded from a precalculated \code{covArray} in the compressed form (option \code{type = "covArray"})
or calculated using Mosflm assumptions of Poisson statistics (\code{type = "mosflm"}), or calculated using a \code{corArray}, the image
values and a \code{VOMimage} (\code{type = "corArray"}).
}
\value{
A vector of five elements, the last three of which might be \code{NA} depending on options:
	\item{1}{Summation integration intensity}
	\item{2}{Summation integration intensity variance estimate (as Mosflm)}
	\item{3}{Summation integration intensity variance estimate (using the \quote{cascade model} for the current detector parameters)}
	\item{4}{Profile fitting intensity}
	\item{5}{Profile fitting intensity variance estimate (from \code{Mobs})}
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
	\code{\link{makeProfile}},
	\code{\link{makeVOMarray}},
	\code{\link{pxCor}},
	\code{\link{pxCov}},
	\code{\link{flatten}},
	\code{\link{spotSimulations}},		
}
\examples{
### None here, see ?spotSimulations
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{models}
