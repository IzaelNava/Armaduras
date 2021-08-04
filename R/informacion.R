#' @keywords function
#' @name informacion
#'
#' @title Información del modelo (coordenadas, elementos, propiedades, fuerzas y restricciones).
#'
#'
#' @details
#' Se extraen de un archivo .xlsx los datos del modelo de armadura a analizar.
#' El archivo .xlsx contiene 5 pestañas (coordenadas, elementos, propiedades, fuerzas y restricciones)
#' Para mayor información de cómo introducir los datos, ver la documentación.
#'
#' @param archivo Archivo .xlsx con datos del modelo.
#'
#'
#' @return
#' Devuelve en forma de data.frame, la información de: coordenadas, elementos, 
#' propiedades, fuerzas y restricciones del modelo a analizar.
#'
#' @export
#'
#'
#'



informacion <- function(archivo){
  
  # InformaciÃ³n de la armadura
  nodos         <-  data.frame(readxl::read_xlsx(paste0(archivo, ".xlsx"), 1))
  elementos     <-  data.frame(readxl::read_xlsx(paste0(archivo, ".xlsx"), 2))
  propiedades   <-  data.frame(readxl::read_xlsx(paste0(archivo, ".xlsx"), 3))
  restricciones <-  data.frame(readxl::read_xlsx(paste0(archivo, ".xlsx"), 4))
  cargas        <-  data.frame(readxl::read_xlsx(paste0(archivo, ".xlsx"), 5))
  
  # nÃºmero de datos
  
  numNodos <- dim(nodos)[1]
  numElemn <- dim(elementos)[1]
  numRestr <- dim(restricciones)[1]
  numCargs <- dim(cargas)[1]
  
  return(list(nodos=nodos, elementos=elementos, propiedades=propiedades, restricciones=restricciones, 
              cargas=cargas, numNodos=numNodos, numElemn=numElemn, numRestr=numRestr, numCargs=numCargs))
  
}
  
