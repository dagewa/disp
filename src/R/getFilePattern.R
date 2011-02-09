getFilePattern <-
function(string){

  ## reads in a pattern consisting of a path concatenated with a generic filename of the form
  ## "name###" and returns the directory and regular expression required for matching the files
  
  # check input is a single string
  if(typeof(string) != "character" || length(string) != 1){
    stop("getFilePattern: input is not a single character string")
  }
  
  # find position in the character string of the '/' immediately preceeding the filename
  pos <- regexpr("/[[:alnum:]_\\.]+#+[[:alnum:]_\\.]*$", string)
  
  if(pos == -1){
    # if no preceeding path try the current directory
    string <- paste("./", string, sep="")
    pos <- regexpr("/[[:alnum:]_\\.]+#+[[:alnum:]_\\.]*$", string)
    if(pos == -1) stop("supplied string was not of the form path/filename###") # stop if still doesn't match
  }
  
  filenameHash <- substring(string, pos + 1)
  directory <- substring(string, 1, pos)
  prefixPos <- regexpr("#", filenameHash) - 1
  prefix <- substring(filenameHash, 1, prefixPos)
  digits <- attr(regexpr("#+",filenameHash),"match.length")
  suffixPos <- prefixPos + 1 + digits
  suffix <- substring(filenameHash, suffixPos)
  
  pattern <- paste("^", prefix, "[[:digit:]]{", digits, "}", suffix, "$", sep="")
  
  return(c(directory, pattern))
}

