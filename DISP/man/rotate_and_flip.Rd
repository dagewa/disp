\name{Rotate and flip}

\alias{flipVertical}
\alias{flipHorizontal}
\alias{rotate}

\title{
	Rotate and flip image matrices
}
\description{
	Functions to rotate images or flip them in X or Y.
}
\usage{
flipVertical(imageMatrix)
flipHorizontal(imageMatrix)
rotate(imageMatrix)
}

\arguments{
  \item{imageMatrix}{
	A matrix with elements corresponding to pixel values.
}
}
\details{
	Rotation is by an angle of 90 degrees in an anticlockwise direction when viewing the imageMatrix by \code{disp}.
	Horizontal flip is a mirror operation around an axis parallel to the Y direction.
	Vertical flip is a mirror operation around an axis parallel to the X direction.
}
\value{
	A matrix of pixel values corresponding to the input after application of rotation or flip.
}
\references{
	\url{http://dx.doi.org/10.1107/S0021889810033418}
}
\author{
	David G. Waterman.
}

\seealso{
	\code{\link{disp}}.
}
\examples{
x <- cbind(1:10,10:1)
rotate(x)
flipVertical(x)
flipHorizontal(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{manip}
\keyword{array}

