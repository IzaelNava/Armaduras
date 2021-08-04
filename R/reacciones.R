#' @keywords function
#' @name reacciones
#'
#' @title Reacciones del sistema.
#'
#'
#' @details
#' Se obtienen las reacciones en el sistema
#'
#' @param matGlobal Matriz global de rigidez del sistema estructural.
#' @param Desplazamientos Desplazamientos obtenidos.
#' @param dof NÃºmero total de grados de libertad en el sistema
#'
#'
#' @return
#' Devuelve las reacciones del sistema.
#' 
#' @example 
#' 
#' @export
#'
#'



reacciones <- function(matGlobal, Desplazamientos, dof, truss, restricciones, EtiquetasGl) {

  inferior <- sum(restricciones[,-1])
  superior <- dof - inferior
  
  
  reaccions <- matGlobal[(superior+1):dof, 1:superior] %*% Desplazamientos[1:superior, 2]
  reaccions
  
  reacc <- data.frame(gradoslibertad=(superior+1):dof, reacciones=reaccions)
  
  # tabla con reacciones asociadas a los nodos y eje de acci�n
  coor <- matrix(0, length(reacc[,1]), 2)
  tablaReacc <- restricciones[,-1]
  
  
  for (i in 1:length(reacc[,1])){
    coor[i,] <- which(reacc[i,1] == EtiquetasGl[restricciones[,1], 2:(truss+1)], arr.ind=TRUE)
    tablaReacc[coor[i,1], coor[i,2]] <- reacc[i,2]
  }
  
  tablaReacc <- data.frame(Nodos=restricciones[,1], tablaReacc)
  
  
  return(reacc)

}
