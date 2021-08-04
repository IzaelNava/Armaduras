#' @keywords function
#' @name fuerzasInternas
#'
#' @title Obtenci?n de fuerzas internas en los elementos.
#'
#'
#' @details
#' Se obtienen las fuerzas internas en los elementos del sistema.
#'
#' @param propiedades Como data.frame, contiene las propiedades de m?dulo de elasticidad (E) y
#' el ?rea de la secci?n (A) de cada elemento.
#' @param long Como data.frame, contiene las longitudes de cada elemento.
#' @param cosDirec Como data.frame, contiene los cosenos directores de los elementos por ejes.
#' @param EtiquetasNF Como data.frame, contiene las etiquetas de grados de libertad del nodo inicial (N) y final (F)
#' @param Desplazamientos Desplazamientos obtenidos.
#'
#' @return
#' Devuelve las fuerzas internas en los elementos. Adem?s, lsi est? trabajando a tensi?n o compresi?n
#' @example
#'
#' @export
#'
#'

fuerzasInternas <- function(propiedades, long, cosDirec, EtiquetasNF, Desplazamientos, truss) {

  EtiquetasNF <- EtiquetasNF[,-1]

  rigidez <- (propiedades[,2]*propiedades[,3])/long[,2]


  ti <- matrix(0, length(propiedades[,1]), truss*2)

  for (i in 1:length(propiedades[,1])){
    for (j in 1:(truss*2)){
      ti[i,j] <- Desplazamientos[EtiquetasNF[i,j], 2]
    }
  }


  fzasInternas <- matrix(0, length(propiedades[,1]), 1)

  for (i in 1:length(propiedades[,1])){
  #       i                         i                   i                                      i
    fzasInternas[i,1] <- (rigidez[i] * matrix(c(as.numeric(cosDirec[i,-1]*(-1)), as.numeric(cosDirec[i,-1])), 1, (truss*2)) ) %*% matrix(ti[i,], (truss*2), 1)

  }

  vecCompTens <- character()
  # compresión o tensión
  for (i in 1:length(propiedades[,1])){
   if (fzasInternas[i, 1] == 0){
     vecCompTens[i] <- "zero"
   } else {
     if (fzasInternas[i, 1] > 0){
       vecCompTens[i] <- "Tensión"
     } else {
       vecCompTens[i] <- "Compresión"
     }
   }
  }

  tabla <- data.frame(elemnto=propiedades[,1], fuerzasinternas=fzasInternas, edo=vecCompTens)

  return(tabla)

}



