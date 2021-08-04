
nodoscargas2D <- function(nodos, cargas, r, truss){

  numcargTotal <- apply(cargas[-1] != 0, 1, sum)

  # Nodos con cargas (inicio, cabeza de flecha)
  tablanodosj <- list()

  for (i in 1:length(cargas[,1])){
    tablanodosj[[i]] <- matrix(rep(nodos[cargas[i,1],-1], numcargTotal[i]), numcargTotal[i], truss, byrow = TRUE)
  }

  ncargasj <- do.call(rbind, tablanodosj)

  # Nodos con cargas (Final, cola de flecha)
  diagonales <- list()

  for (i in 1:length(cargas[,1])){
    diagonales[[i]] <- diag(cargas[i, -1])
  }

  diagonalesj <- do.call(rbind, diagonales)
  diagonalesj <- diagonalesj[apply(diagonalesj, 1, function(x) !all(x==0)),]

  if(length(numcargTotal) == 1) {
    diagonalesj <- t(as.matrix(diagonalesj))
  } else {
    NULL
  }

  ncargasi <- matrix(0, length(diagonalesj[,1]), truss)

  for(i in 1:length(diagonalesj[,1])){
    for(j in 1:truss){
      if(diagonalesj[i,j] != 0){
        if(diagonalesj[i,j] > 0){
          ncargasi[i,j] <- as.numeric(ncargasj[i,j]) - r
        } else {
          ncargasi[i,j] <- as.numeric(ncargasj[i,j]) + r
        }
      } else {
        ncargasi[i,j] <- as.numeric(ncargasj[i,j])
      }
    }
  }

  nombrecargas <- rowSums(diagonalesj)

  if(length(numcargTotal) == 1) {
    nombrecargas <- t(as.matrix(nombrecargas))
  } else {
    NULL
  }


  ncargasii <- matrix(c(ncargasi[,1], rep(0, length(ncargasi[,1])), ncargasi[,2]), length(diagonalesj[,1]), (truss+1))

  ncargasjj <- matrix(c(ncargasj[,1], rep(0, length(ncargasj[,1])), ncargasj[,2]), length(diagonalesj[,1]), (truss+1))

  return(list(ncargasi=ncargasii, ncargasj=ncargasjj, nombrecargas=abs(nombrecargas), numcargTotal=sum(numcargTotal)))

}
