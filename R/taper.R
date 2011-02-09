taper <-
function(input, taperBleed = detector$taperBleed){

  ## Simulates distortion and demagnification introduced by a fiber optic taper.
  ## The taper is considered perfect, i.e. the distortion is completely deterministic
  ## and there is no transmission loss.
  ## This version introduces shifts in the photon positions to simulate increase in PSF due to FOT
  ## this is controlled by a gaussian dist, with sd parameter detector$taperbleed


  # check the input is kosher
  if (is.null(input)) return(NULL)
  cols <- ncol(input)
  if (is.null(cols) || cols != 2) stop("taper: the supplied input does not have 2 columns (for x and y Xph coordinates)")  
  
  # anything outside the taper large face bounds is lost
  input <- subset(input, input[,1] > detector$xDetRange[1] & input[,1] < detector$xDetRange[2]
                       & input[,2] > detector$yDetRange[1] & input[,2] < detector$yDetRange[2])
  # make list of shifts due to taper 'bleed' (contributing to PSF, modelled by Gaussian)
  rows <- nrow(input)
  dr <- rnorm(n = rows, mean = 0, sd = taperBleed)
  theta <- runif(n = rows, min = 0, max = 2 * pi)
  dx <- dr * cos(theta)
  dy <- dr * sin(theta)
  input[,1] <- input[,1] + dx
  input[,2] <- input[,2] + dy
  
  output <- .distortPoints(input)
  return(output)
}

