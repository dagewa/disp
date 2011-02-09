\name{taper}
\alias{taper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
taper(input, taperBleed = detector$taperBleed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{taperBleed}{
%%     ~~Describe \code{taperBleed} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (input, taperBleed = detector$taperBleed) 
{
    if (is.null(input)) 
        return(NULL)
    cols <- ncol(input)
    if (is.null(cols) || cols != 2) 
        stop("taper: the supplied input does not have 2 columns (for x and y Xph coordinates)")
    input <- subset(input, input[, 1] > detector$xDetRange[1] & 
        input[, 1] < detector$xDetRange[2] & input[, 2] > detector$yDetRange[1] & 
        input[, 2] < detector$yDetRange[2])
    rows <- nrow(input)
    dr <- rnorm(n = rows, mean = 0, sd = taperBleed)
    theta <- runif(n = rows, min = 0, max = 2 * pi)
    dx <- dr * cos(theta)
    dy <- dr * sin(theta)
    input[, 1] <- input[, 1] + dx
    input[, 2] <- input[, 2] + dy
    output <- .distortPoints(input)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
