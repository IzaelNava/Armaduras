#' @keywords function
#' @name longitud
#'
#' @title Obtenci�n de la longitud del elemento.
#'
#'
#' @details
#' Se obtienen las longitudes de los elementos a partir de los deltas (ver funci�n deltaxyz de esta
#' paquter�a).
#'
#' @param dlta Como data.frame, contiene los elementos y deltas para cada eje de an�lisis.
#'
#'
#' @return
#' Devuelve la longitud de los elementos del modelo.
#' por ejemplo (siendo i un elemento cualquiera): 
#' longitud.i = sqrt(deltax.i^2 + deltay.i^2 + deltaz.i^2) 
#'
#' @example 
#' dlta <- data.frame(Elemn=c(1,2), x=c(6,6), y=c(0,0), z=c(00))
#' 
#' long <- longitud(dlta)
#'
#'
#' @export
#'
#'

longitud <- function(dlta){
  long <- sqrt(rowSums((dlta[,-1])^2))
  long <- cbind(dlta[,1], long)
  
  colnames(long) <- c("Elemn", "long.")
  as.data.frame(long)

  return(long)
}
