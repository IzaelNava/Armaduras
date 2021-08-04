#' @keywords function
#' @name etiquetasGradosLibertad
#'
#' @title Etiquetas de grados de libertad en cada nodo.
#'
#'
#' @details
#' Se obtienen las etiquetas de los grados de libertad que corresponden a cada nodo.
#' si un nodo está restringido, es cosiderado en la etiqueta y se coloca en su lugar correspondiente
#' con el fin de tener una matriz global ordenada.
#' 
#' @param numNodos Escalar. Número de nodos en el modelo.
#' @param nodos Data.frame. Coordenadas de nodos del modelo.
#' @param numRestr Escalar. Número de nodos que tienen restricciones.
#' @param restricciones Data.frame. Nodos con restricciones. Si la direeción tiene
#' 1, está restringido en dicha dirección. Si es 0, está libre.
#' @param truss Si el modelo está en 2D <- 2, o 3D <- 3.
#' @param dof Número total de grados de libertad.
#'
#'
#' @return
#' Devuelve en forma de data.frame, las etiquetas en cada nodo del modelo ( en sus respectivos ejes).
#'
#'
#' @example 
#' 
#' truss <- 3
#' numNodos <- 2
#' nodos <- data.frame(nodos=c(1,2), x=c(0,6), y=c(0,0), z=c(0,0))
#' numRestr <- 2
#' restricciones <- data.frame(Nodo=c(1,1), x=c(1,0), y=c(0,1), z=c(1,1))
#' dof <- truss * numNodos
#'
#' etGL <- etiquetasGradosLibertad(numNodos, nodos, numRestr, restricciones, truss, dof)
#' 
#' @export
#'
#'
#'
etiquetasGradosLibertad <- function(numNodos, nodos, numRestr, restricciones, truss, dof){
  if(truss != 2 & sum(restricciones[,-1]) == dof){
    n <- data.frame(matrix(1:dof, numNodos, truss, byrow = TRUE))
    n <- cbind(nodos[,1], n)
    
  } else {
    
    if (truss == 2 & sum(restricciones[,c(-1, -4)]) == dof){
      n <- data.frame(matrix(1:dof, numNodos, truss, byrow = TRUE))
      n <- cbind(nodos[,1], n)
    } else {
      
      
      # Inicia si la condición anterior no se cumple
      m <- matrix(0, numNodos, truss+1)
      if(truss == 2){
        colnames(m) <- c("Nodos", "x", "y")
      } else {
        colnames(m) <- c("Nodos", "x", "y", "z")
      }
      
      # Pasa nodos a nueva matriz
      m[,1] <- nodos[,1] 
      
       # Pasa los valores restringidos a la matriz general para obtener etiquetas de los grados de libertad
       for ( i in 1:numRestr){
         for(j in 1:(truss+1)){
           m[restricciones[i,1], j] <- restricciones[i,j]
         }
       }
      
      
      # Truco para nombrar etiquetas
      # Se suma las restricciones y se colocan en una nueva columna
      sumaRestr <- rowSums(m[,-1])
      m <- cbind(m, sumaRestr)
      
      # Ordenar nodos de acuerdo a las restricciones
      m <- m[order(m[,(truss+2)]),]
      
      # Cuando el valores sea 0, agregar una secuencia de 1 al valor correspondiente,
      # de tal forma que así enumere los nodos que no están restringidos por apoyos.
      
      n <- m[, c(-1, -(truss+2))]
      
      if(truss == 2){
        seqgll <- 1:(dof - sum(restricciones[,c(-1, -4)]))
      } else {
        seqgll <- 1:(dof - sum(restricciones[,-1]))
      }
      
      seqglr <- (length(seqgll)+1):dof
      
      # coordenadas de nodos sin/con restricciones
      coor.GDLL <- which(n  == 0, arr.ind = T)
      coor.GDLR <- which(n  == 1, arr.ind = T)
      
      # se ordenan por filas para que no altere el orden de la numeraciÃ³n
      coor.GDLL <- coor.GDLL[order(coor.GDLL[,1]),]
      coor.GDLR <- coor.GDLR[order(coor.GDLR[,1]),]
      
      for(i in 1:length(seqgll)){
        if (length(seqgll) == 1){
          n[coor.GDLL[1], coor.GDLL[2]] <- seqgll[1]
        } else {
          n[coor.GDLL[i,1], coor.GDLL[i,2]] <- seqgll[i]
        }
      }
      
      
      for(i in 1:length(seqglr)){
        n[coor.GDLR[i,1], coor.GDLR[i,2]] <- seqglr[i]
      }
      
      n <- cbind(m[,1], n)
      n <- n[order(n[,1]),]
      
    }
  }
  
  
  if(truss == 2){
    colnames(n) <- c("Nodos", "x", "y")
  } else {
    colnames(n) <- c("Nodos", "x", "y", "z")
  }
  
  return(n)
}
  
  



