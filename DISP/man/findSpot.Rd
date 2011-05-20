\name{findSpot}
\alias{findSpot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Find a spot on an image
}
\description{
	Finds the peak pixel in a region within an image and fits a measurement box around it,
	and optionally displays the measurement box.
}
\usage{
	findSpot(imageMatrix, startX = NULL, startY = NULL, searchBoxSize = 20, movAvLength = 15, display = TRUE, messages=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageMatrix}{
	A matrix with elements corresponding to pixel values.
}
  \item{startX}{
	Integer pixel index giving the approximate location of the spot in X.
}
  \item{startY}{
	Integer pixel index giving the approximate location of the spot in Y.
}
  \item{searchBoxSize}{
  	Integer distance in pixels either side of startX and startY to form the sides of the search box.
}
  \item{movAvLength}{
	Integer parameter used while optimising NX and NY.
}
  \item{display}{
	Logical parameter, whether to plot the measurement box or not.
}
  \item{messages}{
	Logical parameter, whether or not to print log messages.
}
}
\details{
	Used to locate a spot positions and fit a measurement box to it, using an algorithm based on MOSFLM's, which
	optimises measurement box parameters to maximise I/sig(I) of the summation integrated spot, as described by
	Lehman and Larsen \url{http://dx.doi.org/10.1107/S0567739474001379}. Experience shows that the implementation of
	this algorithm may be overly conservative in determining the size of the peak region. Manually increasing the
	peak region size prior to integration might result in a more accurate determination of the integrated intensity..
	
	startX and startY may be most conveniently determined interactively. If
	they are passed as NULL (the default), the function tries to get values from a mouse click on a displayed image
	using \code{locator}.
	
	Optional side-effects of the function are to print the location of the most intense pixel in the search box and
	to display the optimised measurement box using \code{dispBox}.
}
\value{
	\item{NX}{Horizontal length of optimised measurement box as an odd integer number of pixels}
	\item{NY}{Vertical height of optimised measurement box as an odd integer number of pixels}
	\item{NRX}{Horizontal margin between the background and peak regions in pixels}
	\item{NRY}{Vertical margin between the background and peak regions in pixels}
	\item{NC}{Corner cut off parameter in pixels giving diagonal margins between background and peak regions}
	\item{xPx}{Horizonal pixel index of the centre of the measurement box}
	\item{yPx}{Vertical pixel index of the centre of the measurement box}
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
	\code{\link{dispBox}},
	\code{\link{pxIntegrate}}.
}
\examples{
	## Not run.
	## WARNING: This may take a long time to compute, and requires the lists called detector and processing to be set up.
	\dontrun{
		data(detector)
		data(processing)
		rawIm <- read(expose(spot()))
		corrIm <- correct(rawIm)
		findSpot(corrIm, 500, 500)
	}	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{dplot}
\keyword{utilities}
