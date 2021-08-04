#' @keywords function
#' @name ki
#'
#' @title Matriz de rigidez de cada elemento.
#'
#'
#' @details
#' Se obtienen las matrices de rigideces de cada elemento.
#'
#' @param propiedades Como data.frame, contiene las propiedades de mÃ³dulo de elasticidad (E) y 
#' el Ã¡rea de la secciÃ³n (A) de cada elemento.
#' @param long Como data.frame, contiene las longitudes de cada elemento.
#' @param cosDirec Como data.frame, contiene los cosenos directores de los elementos por ejes.
#' @param EtiquetasNF Como data.frame, contiene las etiquetas de grados de libertad del nodo inicial (N) y final (F)
#' @param truss Escalar. Dimensiones 2D o 3D. si es 2D; truss <- 2. De lo contrario, truss <- 3
#'
#'
#' @return
#' Devuelve la en forma de lista, las matrices de rigideces de cada elemento con sus correspondientes etiquetas 
#' de grados de libertad correspondientes
#' 
#' @example 
#' propiedades <- data.frame(elemento=c(1, 2, 3), E=c(2E10, 2E10, 2E10), y=c(0.01, 0.01, 0.01))
#' long <- data.frame(elemento=c(1, 2, 3), long=c(6, 6, 6))
#' cosDirec <- data.frame(elem=1:3, x=c(1, 1, 1), y=c(0, 0, 0), z=c(0, 0, 0))
#' EtiquetasNF <- data.frame(elem=1:3, Nx=c(31, 1, 4), Ny=c(32, 2, 5), Nz=c(33, 3, 6), 
#'                           Fx=c(1, 4, 7), Fy=c(2, 5, 8), Fz=c(3, 6, 9))
#' truss <- 3
#' matRigidecesElem <- ki(propiedades, long, cosDirec, EtiquetasNF, truss)
#' matRigidecesElem
#' 
#' @export
#'
#'

ki <- function(propiedades, long, cosDirec, EtiquetasNF, truss){
  
  numElem <- length(propiedades[,1])
  kii <- list()
  
  if(truss == 3){

  for (i in 1:numElem){
  
    x <- cosDirec[i,2]
    y <- cosDirec[i,3]
    z <- cosDirec[i,4]
    
    f1 <- cosDirec[i,-1] * x 
    f2 <- cosDirec[i,-1] * y 
    f3 <- cosDirec[i,-1] * z 
    
    m1 <- rbind(f1, f2, f3)
    mn1 <- m1 * -1
    
    kii[[i]] <- cbind(rbind(m1, mn1), rbind(mn1, m1)) * (propiedades[i, 2]*propiedades[i, 3]/long[i,2])
    colnames(kii[[i]]) <- EtiquetasNF[i,-1]
    rownames(kii[[i]]) <- EtiquetasNF[i,-1]
  }
    # c�digo para 2D
    } else {
      for (i in 1:numElem){
      
      x <- cosDirec[i,2]
      y <- cosDirec[i,3]
      
      f1 <- cosDirec[i,-1] * x 
      f2 <- cosDirec[i,-1] * y 
      
      m1 <- rbind(f1, f2)
      mn1 <- m1 * -1
      
      kii[[i]] <- cbind(rbind(m1, mn1), rbind(mn1, m1)) * (propiedades[i, 2]*propiedades[i, 3]/long[i,2])
      colnames(kii[[i]]) <- EtiquetasNF[i,-1]
      rownames(kii[[i]]) <- EtiquetasNF[i,-1]
      }
      }

  return(kii)

}
  
