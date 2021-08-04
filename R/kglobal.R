#' @keywords function
#' @name kglobal
#'
#' @title Matriz de rigidez global de la estructura.
#'
#'
#' @details
#' Se obtiene la matriz global de la estructura..
#'
#' @param K Matriz de ceros. El número de filas y columnas está dado por el número de nodos * 2 ( si es 3D) o 3 (si es 3D)
#' @param EtiquetasNF Como data.frame, contiene las etiquetas de grados de libertad del nodo inicial (N) y final (F)
#' @param truss Escalar. Dimensiones 2D o 3D. si es 2D; truss <- 2. De lo contrario, truss <- 3
#'
#'
#' @return
#' Devuelve la matriz de rigidez global del sistema estructural
#' 
#' @example 
#' 3D y numero de nodos=3
#' dof <- 3 * 3
#' K <- matrix(0, dof, dof)
#' EtiquetasNF <- data.frame(elem=1:3, Nx=c(1, 4, 7), Ny=c(2, 5, 8), Nz=c(3, 6, 9), 
#'                           Fx=c(4, 7, 1), Fy=c(5, 8, 2), Fz=c(6, 9, 3))
#' truss <- 3
#' matKglobal <- kglobal(K, EtiquetasNF, truss)
#' matKglobal
#' 
#' @export
#'
#'


kglobal <- function(K, EtiquetasNF, matRigidecesElem, truss){

  coorRow <- list()
  coorCol <- list()
  
  for(i in 1:length(EtiquetasNF[,1])){
    
    coorRow[[i]] <- matrix(rep(EtiquetasNF[i,-1], (truss*2)), (truss*2), (truss*2))
    coorCol[[i]] <- matrix(rep(EtiquetasNF[i,-1], (truss*2)), (truss*2), (truss*2), byrow = TRUE)
    
  }
  
  
  # K global
  K
  
  
  for (e in 1:length(EtiquetasNF[,1])){
    for (i in 1:(truss*2)){
      for (j in 1:(truss*2)){
        if(K[as.numeric(coorRow[[e]][i,j]), as.numeric(coorCol[[e]][i,j])] == 0){
          K[as.numeric(coorRow[[e]][i,j]), as.numeric(coorCol[[e]][i,j])] <- matRigidecesElem[[e]][i, j]
        } else {
          K[as.numeric(coorRow[[e]][i,j]), as.numeric(coorCol[[e]][i,j])] <- matRigidecesElem[[e]][i, j] +
            K[as.numeric(coorRow[[e]][i,j]), as.numeric(coorCol[[e]][i,j])]
        }
        
      }
    }
  }
  
  return(K)
  
}


