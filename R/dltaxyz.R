#' @keywords function
#' @name dltaxyz
#'
#' @title Obtención de deltas (diferencias de las coordenadas xN y xF, yN y yF y zN y zF).
#'
#'
#' @details
#' Los deltas de cada elemento se obtienen como la diferencias de las coordenas N 
#' (considerado como el inicio del elemento) y F (final del elemento).
#'
#' @param nodos Nodos del modelo como data.frame
#' @param elementos Elementos del modelos como data.frame
#' @param truss Armadura en 2D o 3D. Si truss == 2, se cosidera 2D, de otra forma es 3D.
#'
#'
#' @return
#' Devuelve los deltas de las corrdenadas x, y y/o z.
#' Por ejemplo: deltax = xF - xN ... para cada elemento.
#'
#' @example 
#' nodos <- data.frame(nodos=1:2, x=c(0, 6), y=c(0, 0), z=c(0, 0))
#' elementos <- data.frame(Elemento=1, nN=1, nF=2)
#' truss <- 3
#' deltas <- dltaxyz(nodos, elementos, truss)
#' 
#' @export
#'

dltaxyz <- function(nodos, elementos, truss){
  A  <- data.frame(matrix(0, length(elementos[,1]), truss))
  B  <- data.frame(matrix(0, length(elementos[,1]), truss))
  
  for(i in 1:length(elementos[,1])){
    A[i,]  <- nodos[elementos[i,2],2:(truss+1)]
    B[i,]  <- nodos[elementos[i,3],2:(truss+1)]
  }
  
  BA <- B - A
  BA <- cbind(elementos[,1], BA)
  if (truss == 2){
    colnames(BA) <- c("Elemn", "x", "y")
  } else  {
    colnames(BA) <- c("Elemn", "x", "y", "z")
  }
  return(BA)
}




