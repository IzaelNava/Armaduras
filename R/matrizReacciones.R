
matrizReacciones <- function(Reacciones, restricciones, EtiquetasGl, truss) {

  filas <- dim(restricciones[,-1])[1]
  columnas <- dim(restricciones[,-1])[2]

  mat1 <- matrix(0, length(Reacciones[,1]), 2)

  for (i in 1:length(Reacciones[,1])){
    mat1[i,] <- which(EtiquetasGl[restricciones[,1],-1]==Reacciones[i,1], arr.ind=TRUE)
  }

  matReacc <- matrix(0, filas, columnas)

  for ( i in 1:length(Reacciones[,1])){
    matReacc[mat1[i,1], mat1[i,2]] <- Reacciones[i,2]
  }

  matReacc <- cbind(EtiquetasGl[restricciones[,1],1], matReacc)


  if(truss == 2){
    colnames(matReacc) <- c("nodo", "x", "y")
  } else {
    colnames(matReacc) <- c("nodo", "x", "y", "z")
  }

  matReacc <- data.frame(matReacc)

 return(matReacc)
}
