readMTZ <-
function(filename, messages = TRUE){
  
  ## a function for reading an MTZ file
  ## N.B. assumes 4 bytes per data item, not sure if this is always true
  
  f <- file(filename, open="rb")
  
  mtzStamp <- readChar(f, 4)
  if(mtzStamp != "MTZ " || length(mtzStamp) == 0){
    close(f)
    stop("readMTZ: file not recognised")
  }
  headerLoc <- readBin(f, "integer", n=1, size=4, signed=FALSE) #header location in number of 'data items' (4 bytes)
  machineStamp <- readChar(f,4) #number formats of the architecture f was written on

  # go to start of reflection data (MTZ documentation says it is at byte 21, actually it seems to be
  # data item 21, i.e. the 4 bytes from byte 80 to 84)
  seek(f, 80)
  numDataItems <- headerLoc - 20 - 1  
  reflnData <- readBin(f, "numeric", n=numDataItems, size=4) #get reflection data 
  
  #read headers
  tagRead <- function(numChars) strsplit(readChar(f, numChars), "[[:blank:]]+")[[1]]
  mtzHeader <- list()
  mtzHeader$VERS <- tagRead(80)
  mtzHeader$TITLE <-  tagRead(80)
  mtzHeader$NCOL <- tagRead(80)
  mtzHeader$CELL <- tagRead(80)
  mtzHeader$SORT <- tagRead(80)
  mtzHeader$SYMINF <- tagRead(80)
  mtzHeader$SYMM <- NULL
  i <- 1
  repeat{
    temp <- tagRead(80)
    if(temp[1] == "SYMM"){
      mtzHeader$SYMM[[i]] <- temp
      i <- i + 1
    }else{
      (mtzHeader$RESO <- temp)
      break()
    }
  }
  mtzHeader$VALM <- tagRead(80)
  mtzHeader$COL <- NULL
  colNames <- NULL
  i <- 1
  repeat{
    temp <- tagRead(80)
    if(temp[1] == "COLUMN"){
      mtzHeader$COL[[i]] <- temp
      colNames[[i]] <- temp[2]
      i <- i + 1
    }else{
      (mtzHeader$NDIF <- temp)
      break()
    }
  }
  mtzHeader$PROJECT <- tagRead(80)
  mtzHeader$CRYSTAL <- tagRead(80)
  mtzHeader$DATASET <- tagRead(80)
  mtzHeader$DCELL <- tagRead(80)
  mtzHeader$DWAVEL <- tagRead(80)
  mtzHeader$BATCH <- NULL
  i <- 1
  repeat{
    temp <- tagRead(80)
    if(temp[1] == "BATCH"){
      mtzHeader$BATCH[[i]] <- temp
      i <- i + 1
    }else{
      (mtzHeader$END <- temp)
      break()
    }
  }
  
  #read batch headers here
  #####
  close(f)
  
  #shape reflection data
  numCols <- length(mtzHeader$COL)
  numRecords <- numDataItems / numCols
  dim(reflnData) <- c(numCols, numRecords)
  reflnData <- t(reflnData)
  colnames(reflnData) <- colNames
  
  # change to data.frame (for easy indexing) and change some storage types. Any of these tags that don't exist
  # will return NULL
  reflnData <- as.data.frame(reflnData)
  reflnData$H <- as.integer(reflnData$H)
  reflnData$K <- as.integer(reflnData$K)
  reflnData$L <- as.integer(reflnData$L)
  reflnData$BATCH <- as.integer(reflnData$BATCH)
    
  #output some stats if messages are wanted  
  if(messages){
    # print header stuff
    cat("HEADER INFORMATION\n")
    cat(mtzHeader$VERS,"\n")
    cat(mtzHeader$TITLE,"\n")
    cat(mtzHeader$NCOL,"\n")
    cat(mtzHeader$CELL,"\n")
    cat(mtzHeader$SORT,"\n")
    cat(mtzHeader$SYMINF,"\n")
    for (line in mtzHeader$SYMM) cat(line,"\n")
    cat(mtzHeader$RESO,"\n")
    cat(mtzHeader$VALM,"\n")
    #don't bother with COL, summary(reflnData) used below gives useful info
    cat(mtzHeader$NDIF,"\n")
    cat(mtzHeader$PROJECT,"\n")
    cat(mtzHeader$CRYSTAL,"\n")
    cat(mtzHeader$DATASET,"\n")
    cat(mtzHeader$DCELL,"\n")
    cat(mtzHeader$DWAVEL,"\n")
    #print column info
    cat("COLUMN SUMMARY\n")
    print(summary(reflnData))
  }
  
  #attach header as attribute
  attr(reflnData, "mtzHeader") <- mtzHeader
  return(reflnData)
}

