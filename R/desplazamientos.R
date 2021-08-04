#' @keywords function
#' @name desplazamientos
#'
#' @title desplazamientos del sistema.
#'
#'
#' @details
#' Se obtienen las matrices de rigideces de cada elemento.
#'
#' @param matGlobal Matriz global de rigidez del sistema estructural.
#' @param fzasExt Fuerzas externas asociadas a los grados de libertad del sistema.
#' @param restricciones Restricciones del sistema. Grados de libertad que tienen restricción (1)
#' @param dof Número total de grados de libertad en el sistema
#'
#'
#' @return
#' Devuelve los desplazamientos en los grados de libertad que no están restringidos.
#' 
#' @example 
#' 
#' @export
#'
#'




desplazamientos <- function(matGlobal, fzasExt, restricciones, dof){
  
  
  inferior <- sum(restricciones[,-1])
  superior <- dof - inferior
  
  k11 <- matGlobal[1:superior, 1:superior]
  
  f11 <- fzasExt[1:superior, 2]
  f22 <- rep(0, inferior)
  
  a <- try(solve(k11), silent = TRUE)
  b <- class(a)
  
  if(b[1] == "matrix"){
    des <- solve(k11) %*% f11
  } else {
    des <- matlib::Ginv(k11) %*% f11
    print("Se ha usado Ginv")
  }
    despl <- c(as.numeric(des), f22)
  
  desplazamiento <- data.frame(gradoslibertad=c(1:dof), despl=despl)
  
  return(desplazamiento)
  
}

