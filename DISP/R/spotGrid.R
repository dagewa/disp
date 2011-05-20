spotGrid <-
function(lambda = 1000, xSize = detector$xDetRange, ySize = detector$yDetRange,
                      x_spacing = 2, y_spacing = 2, sdx = 0.2, sdy = 0.15, rho = 0.3){
                     
  ## creates a matrix of Xph coordinates consisting of a spot at each at each position on a regular
  ## grid with a default pitch of 2 mm by 2 mm
  positions <- as.matrix(expand.grid(x = seq(from = xSize[1], to = xSize[2], by = x_spacing),
                         y = seq(from = ySize[1], to = ySize[2], by = y_spacing)))
  
  temp <- spot(lambda, positions[1,1], positions[1,2], sdx, sdy, rho)
  for (i in 2:dim(positions)[1]){
    temp <- rbind(temp, spot(lambda, positions[i,1], positions[i,2], sdx, sdy, rho))
  }
  return(temp)
}

