expose <-
function(Xph, block = 200000, ...){

  ## given an input data frame of photon positions, produce a matrix consisting of 
  ## electron counts in CCD wells. Needs to be read() to produce a raw image in ADUs.
  ## To deal with long lists of Xph, the exposure is now done in blocks
  
  ##CHANGE EXPOSE() TO WORK THROUGH FILES OF PHOTON POSITIONS, 'block' LINES AT A TIME (readlines). 
  ##THEN IT CAN BE USED GENERALLY AND SPOT GENERATION FUNCTIONS LIKE flood() CAN BE CHANGED TO WRITE TO DISK
  
  ## test shows that 1000 photons takes up 33,843 bytes as text file (writeImage)
  ## or 18,240 bytes as binary (write). A flood with 10'000 photons per pixel for a 1024*1024 pixel object
  ## will be 191 GB as binary!
  
  ## so will still need a special case for floods
  ## perhaps some kind of X-ray source function that wraps spot, flood, etc and exposes 100MB of data at
  ## a time, using a temporary file

  time1 <- Sys.time()    
  # check input is kosher
  if(is.null(Xph)) return(accumulate())
  cols <- ncol(Xph)
  if(is.null(cols) || cols != 2) stop("expose: the supplied input does not have 2 columns (for x and y Xph coordinates)")
    
  len <- nrow(Xph)
  if (len == 1) return(accumulate(taper(phosphor(Xph)))) #because subsetting used below doesn't work for len=1
  cycles <- (len - 1)%/%block #(len-1) ensures the remainder is not zero
  remainder <- len - (cycles * block)
  temp <- phosphor(Xph[1:remainder,], ...)
  temp <- taper(temp)
  temp <- accumulate(temp)
  
  # now do all cycles
  if (cycles > 0) for(i in 1:cycles){
    time2 <- Sys.time()
    print.noquote(paste(format(difftime(time2,time1)),"expose: cycle number",i,"of",cycles))
    if (.Platform$OS.type == "windows") flush.console()
    start <- remainder + 1 + (i - 1) * block
    end <- start + block - 1
    thisCycleTemp <- phosphor(Xph[start:end,], ...)
    thisCycleTemp <- taper(thisCycleTemp)
    thisCycleTemp <- accumulate(thisCycleTemp)
    temp <- temp + thisCycleTemp
  }
  return(temp)
}

