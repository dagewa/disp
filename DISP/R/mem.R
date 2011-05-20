mem <-
function() {
  a <- sapply(ls(envir=.GlobalEnv), function(x) object.size(get(x)))
  a <- a/1000000 #convert to MB
  b <- subset(a, a > 1)
  if(length(b) < 1){
	cat("mem: no objects larger than 1MB in .GlobalEnv\n")
  } else dotchart(b,main="objects larger than 1MB in .GlobalEnv", xlab="memory usage (MB)")
}

