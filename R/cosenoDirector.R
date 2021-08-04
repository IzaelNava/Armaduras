#' @keywords function
#' @name cosenoDirector
#'
#' @title Cosenos directores de los elementos por eje de análisis.
#'
#'
#' @details
#' Se obtienen los cosenos directores de los elementos paca cada eje de análisis.
#'
#' @param dlta deltas de los elementos por eje. Está dado como un data.frame. Resultado de
#' aplicar la función deltaxyz(). Para mayor referencia ver dicha función.
#' @param long Longitudes de los elementos. Están dados como data.frame. Resultado de 
#' aplicar la función longitud().  Para mayor referencia ver dicha función.
#' @param truss Armadura en 2D o 3D. Si truss == 2, se cosidera 2D, de otra forma es 3D.
#'
#' @return
#' Devuelve en forma de data.frame, los cosenos directores de cada elemento.
#'
#' @example 
#' 
#' dlta <- data.frame(Elemn=c(1,2), x=c(6,6), y=c(0,0), z=c(0,0))
#' long <- data.frame(Elemn=c(1,2), long=c(6,6))
#' truss <- 3
#' 
#' cosDir <- cosenoDirector(dlta, long, truss)
#' 
#' @export
#'
#'
cosenoDirector <- function(dlta, long, truss){
  
  cosdi <- dlta[,-1]/long[,2]
  cosdi <- cbind(dlta[,1], cosdi)
  
  if (truss == 2){
    colnames(cosdi) <- c("Elemn", "x", "y")
  } else  {
    colnames(cosdi) <- c("Elemn", "x", "y", "z")
  }
  return(cosdi)
}
