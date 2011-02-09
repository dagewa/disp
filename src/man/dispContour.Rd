\name{dispContour}
\alias{dispContour}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Displays an image as a contour map
}
\description{
	Plots an image using a contour plot.
}
\usage{
dispContour(imageMatrix, origin = c(1,1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageMatrix}{
	A matrix with elements corresponding to pixel values.
}
  \item{origin}{
	Integer coodinate pair indicating the index of the pixel at the bottom left of the plot.
}
}
\details{
	See the documentation for \code{disp} for a description of the conventions of an image matrix and rationale
	for the \code{origin} parameter.
	
	The contour levels are currently fixed steps from the lowest level, which is the minimum value in the supplied image.
}
\value{
	Returns NULL. Used for the side-effect of plotting an image as a contour plot.
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
	\code{\link{disp}},
	\code{\link{addBox}},
	\code{\link{dispBox}}.
}
\examples{
	data(detector)
	rawIm <- read(expose(spot()))
	dispContour(rawIm[491:530,491:530], origin=c(491,491))
}

\keyword{ hplot }

