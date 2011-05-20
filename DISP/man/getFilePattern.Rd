\name{getFilePattern}
\alias{getFilePattern}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple filename patterns
}
\description{
Converts a simple pattern to the directory and regular expression required for matching the files.
}
\usage{
getFilePattern(string)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{
	Filename pattern. See details.
}
}
\details{
	MX datasets typically consist of sequentially numbered images, with the image number forming part of the filename
	as a zero-padded fixed-width integer. This function simplifies the task of regular expression matching for
	such file name patterns by providing a wildcard character \sQuote{#} to match a single digit. The supplied pattern
	may contain one unbroken block of \sQuote{#} characters, which then specifies the position and width of the image
	numbers within the filenames.
	
	For example, the pattern \code{"./path/to/imageFile_1_###.img"} specifies matches to files such as \file{./path/to/imageFile_1_001.img}
	and \file{./path/to/imageFile_1_999.img} but not \file{./path/to/imageFile_1_1.img}.
	
	\code{getFilePattern} can be used along with \code{ls} to match objects in the search path rather than files
	on the filesystem. Some functions in \pkg{DISP} make use of this facility. In that case, the returned directory
	is arbitrary and is simply ignored.
}
\value{
	String vector of two elements, the first giving the directory path to the image files, the second giving
	a regular expression for matching the image filenames in the format supported by e.g. \code{dir}.
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
	\code{\link{ls}},
	\code{\link{dir}},
	\code{\link{correctBatch}},
	\code{\link{makeImageSet}},
	\code{\link{readImageBatch}},
	\code{\link{rmImageBatch}}.
}
\examples{
	getFilePattern("./path/to/imageFile_1_###.img")
}

\keyword{file}
\keyword{utilities}
