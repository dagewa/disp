\name{normaliseImage}
\alias{normaliseImage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Non-uniformity correction
}
\description{
Performs non-uniformity correction on a raw image using the known flat field response.
}
\usage{
normaliseImage(imageMatrix, dark = processing$darkRaw, normaliser = processing$flatfield)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageMatrix}{
	A matrix containing the raw image pixel values to be corrected.
}
  \item{dark}{
	A matrix continaing raw dark image pixel values.
}
  \item{normaliser}{The \sQuote{flatfield} matrix of normalisation factors to effect non-uniformity correction.
}
}
\details{
This function would not usually be called directly for normal operation of the simulated detector,
but rather implicitly via a call to \code{correct}. This function is intended to be an intermediate step in overall corrections!
Default objects for \code{dark} and \code{normaliser} are supplied for the default simulated detector in the list \code{processing}.

The procedure to apply non-unformity correction is as follows. A raw image is read in, a raw dark subtracted from it, and then it is
scaled using a suitable normalisation array, i.e. a flood image that has been corrected for non-
uniformity by \code{makeFlatfield}. Note that the resulting image has no bias offset and contains real,
rather than integer values. 
}
\value{
A matrix of pixel values that have been dark-subtracted and normalised for non-uniform detector response.
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
	\code{\link{correct}},
	\code{\link{makeFlatfield}},
	\code{\link{processing}}.
}
\examples{
## Not run.
## WARNING: This may take a long time to compute, and requires the lists called detector and processing to be set up.
\dontrun{
data(detector)
data(processing)
rawIm <- read(expose(spot()))
normRawIm <- normaliseImage(rawIm)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }

