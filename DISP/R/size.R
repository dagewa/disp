size <-
function() {
  a <- sapply(ls(envir=.GlobalEnv), function(x) object.size(get(x)))
  if(length(a) < 1) stop("size: no objects found in the Global Environment")
  a <-sort(a, decreasing = TRUE)
  cat("sizes of objects in .GlobalEnv in Megabytes\n")
  b<-round(a/1000000,digits=3)
  print(b)
  return(b)
}

