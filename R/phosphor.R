phosphor <-
function(input, PSF = detector$PSF, PHS = detector$PHS){

  ## Simulates light generation from a phosphor.
  ##
  ## To reduce calculation load only those photons that will be transmitted through the
  ## taper and finally recorded by the CCD are generated. The number of photons transmitted
  ## is given by a binomial distribution with the total transmission probability
  ## To stop photons being located at discrete radii, this function adds additional random perturbations
  ## to the generated positions.

  # check input is kosher
  if (is.null(input)) return(NULL)
  cols <- ncol(input)
  if (is.null(cols) || cols != 2) stop("phosphor: the supplied input does not have 2 columns (for x and y Xph coordinates)")  
  
  # Quantum Detection Efficiency of screen randomly samples input
  index <-  runif(nrow(input)) <= detector$QDE
  input <- subset(input, index)
  
  # how many output photons?
  len <- nrow(input) 
  gain <- rbinom(n = len, size = sample(PHS[,1],size=len,replace=TRUE,prob=PHS[,2]),
                 prob = detector$transmission)
  size <- sum(gain)

  #copy all the x and y coordinates
  x <- rep.int(input[,1], gain)
  y <- rep.int(input[,2], gain)
  
  # find the PSF shifts
  r <- sample(PSF[,1], size=size, replace=TRUE, prob=PSF[,2])
  stepSize <- PSF[2,1] - PSF[1,1] #this assumes equal bin width in PSF, so that all steps are this size
  dr <- runif(n=size, min=0, max=stepSize) #to make the distribution continous in r, rather than the discrete output of sample()
  r <- r + dr
  names(r) <- NULL #the names attribute gives confusing output when r is printed. I just want the values
  theta <- runif(n = size, min = 0, max = 2 * pi)
  dx <- r * cos(theta)
  dy <- r * sin(theta)
  x <- x + dx
  y <- y + dy
  
  # make the output
  output <- cbind(x,y)
  
  return(output)
}

