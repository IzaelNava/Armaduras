#' @keywords function
#' @name nodosIntermedios
#'
#' @title Nodos intermedios para colocaci�n de etiquetas en el esquema del modelo.
#'
#'
#' @details
#' Nodos en donde se colocar�n etiquetas de n�mero de elemento.
#'
#' @param elementos Data frame de los elementos
#' @param nodos Data frame de los nodos
#'
#' @return
#' regresa vector con coordenadas para colocar el n�mero de elemento en la grafica del modelo.
#'
#' @export
#'
#'
#'

nodosIntermedios <- function(elementos, nodos){
  
  nodosi <- nodos[elementos[,2],-1]
  nodosj <- nodos[elementos[,3],-1]
  
  nodointer <- (nodosi + nodosj)/2

  return(nodointer)
  
}