\name{flatten}
\alias{flatten}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create full variance-covariance matrix from data that stores only local covariance
}
\description{
	The full variance-covariance matrix for pixels in an image is calculated from the pixel values and
	precalculated objects giving the variance over mean ratio at each pixel and the correlations between
	each pixel and others in their vicinity.
}
\usage{
flatten(imageSlice, VOMimage, corArray)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageSlice}{
	A NX*NY matrix with elements corresponding to pixel values, typically within a spot measurement box.
}
  \item{VOMimage}{
	A NX*NY matrix with elements corresponding to the variance to mean ratio (also called the dispersion index) at each pixel position..
}
  \item{corArray}{
	An NX*NY*n*n array that for each of the NX*NY pixels stores the n*n array describing local correlations between that pixel and its neighbours.
}
}
\details{
	This function is not normally called directly by the user. It is part of a mechanism used by \code{pxIntegrate} that allows the variance-covariance matrix
	for all pixels within an NX*NY measurement box to be estimated from precalculated results. This is used in profile-fitting integration for
	experimental methods that take into account pixel correlations in error estimation.
	
	In \code{pxIntegrate} this function is used in two ways. Normal use estimates the variance at each pixel position by multiplying the image pixel values
	by the values stored in the VOMimage. For every pixel \eqn{i} in the image it then expands the stored n*n correlations between that pixel
	and its \eqn{j} neighbours into covariances using \eqn{\sigma_{ij} = \sigma_i \sigma_j r_{ij}}{sigma_ij = sigma_i * sigma_j * r_ij}.
	
	The second use in \code{pxIntegrate} substitutes the \code{imageSlice} and \code{VOMimage} parameters by dummy matrices with every element equal to one. The
	parameter \code{corArray} is actually used to pass in an NX*NY*n*n array containing \emph{covariance} elements rather than \emph{correlation} elements. In this case,
	the function is used to unpack the NX*NY*n*n array into a full (NX*NY)*(NX*NY) array with covariance elements for every pixel pair (many of them zero). In
	effect, \code{flatten} is being used to decompress covariance data that is stored in a format that neglects non-local pairs.
	
	Local pixel correlations and covariances can be calculated in NX*NY*n*n array format using the functions \code{pxCor} and \code{pxCov}.
}
\value{
	A square matrix of dimension NX*NY, NX*NY with elements corresponding to calculated covariance values (variance down the diagonal).
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\section{Warning}{
	For large regions of interest the variance-covariance matrix may become unmanageably large because it has \eqn{(NX*NY)^2} elements.
	Generally this function is intended for use within a region of interest defined by a spot measurement box, which typically has sides 
	of 25-35 pixels.
}

\seealso{
	\code{\link{pxIntegrate}},
	\code{\link{pxCor}},
	\code{\link{pxCov}},
	\code{\link{pxVar}},
	\code{\link{pxMean}},
	\code{\link{makeVOMimage}}.
}
\examples{
#load some data files created during spot simulations for the study
#reported in http://dx.doi.org/10.1107/S0021889810033418
data(spotSimulations)

dummy <- matrix(data = 1, ncol = 21, nrow = 21)
MobsRaw <- flatten(dummy, dummy, varcovZoneBRaw)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{manip}

