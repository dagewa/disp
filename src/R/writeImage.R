writeImage <-
function(imageMatrix, filename){

  ## writes a matrix image as an ascii file, stripping any column and row names
  ## the tab separator is useful for import into imageJ
  
  # reorganise so that data will be written in the same orientation as disp() shows it
  # that is, with the origin at the bottom left, x increasing along columns and y up rows
  yLength <- ncol(imageMatrix)
  imageMatrix <- imageMatrix[,yLength:1]
  imageMatrix <- t(imageMatrix)
  
  write.table(imageMatrix, file=filename, row.names = FALSE, col.names = FALSE, sep="\t")
}

