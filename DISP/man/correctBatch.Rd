\name{correctBatch}
\alias{correctBatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Applies corrections to a batch of images 
}
\description{
	Convenience function to apply corrections to multiple raw images.
}
\usage{
	correctBatch(stringIn, patternOut, envir = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stringIn}{
	A string pattern that matches the names of multiple raw images in the search path.
}
  \item{patternOut}{
	A string pattern giving the common prefix of names for corrected output images.
}
  \item{envir}{
	The environment in which to assign the output images.
}
}
\details{
	The simple pattern format used by \code{stringIn} is described in the help for \code{getFilePattern}.
	This function is not especially versatile as it only operates on, and writes to, objects in the R environment
	rather than files on disk. For generation and correction of large numbers of images see \code{batchGenerate}.
}
\value{
	The return value is NULL. This function is used for its side affect, of assigning objects in the R environment.
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

\section{Warning}{
	This function may be used to pollute the R environment with many objects. It is better to use a function
	such as \code{batchGenerate} to produce and correct multiple simulated images.
}

\seealso{
	\code{\link{correct}},
	\code{\link{getFilePattern}},
	\code{\link{batchGenerate}}.
}
\examples{
## Not run.
## WARNING: This may take a long time to compute, and requires the lists called detector and processing to be set up.
\dontrun{
data(detector)
data(processing)
rawIm01 <- read(expose(spot()))
rawIm02 <- read(expose(spot()))
rawIm03 <- read(expose(spot()))
correctBatch("rawIm##", "corrIm")
}

}

\keyword{datagen}
