#' @keywords function
#' @name fuerzas
#'
#' @title Obtención de fuerzas externas en el modelo, colocadas en su correspondiente grado de libertad.
#'
#'
#' @details
#' Se obtienen las longitudes de los elementos a partir de los deltas (ver función deltaxyz de esta
#' paqutería).
#'
#' @param dlta Como data.frame, contiene los elementos y deltas para cada eje de análisis.
#'
#'
#' @return
#' Devuelve la longitud de los elementos del modelo.
#' por ejemplo (siendo i un elemento cualquiera): 
#' longitud.i = sqrt(deltax.i^2 + deltay.i^2 + deltaz.i^2) 
#' 
#' @example 
#' cargas <- data.frame(Nodo=c(1, 2, 3), x=c(-1e6, 0, 0), y=c(0, 0, 0), z=c(0, -80000, -1200000))
#' numNodos <- 5
#' EtiquetasGl <- data.frame(Nodos=1:5, x=c(1, 4, 7, 10, 13), y=c(2, 5, 8, 11, 14), z=c(3, 6, 9, 12, 15))
#' numNodosCargas <- 3
#' truss <- 3
#' dof <- truss * numNodos
#' fzas <- fuerzas(cargas, numNodos, EtiquetasGl, numNodosCargas, truss, dof)
#' 
#' @export
#'
#'
fuerzas <- function(cargas, numNodos, EtiquetasGl, numNodosCargas, truss, dof){

  # creaciÃ³n de matriz de mismas dimensiones que la cantidad de nodos, con elementos ceros
  m <- as.data.frame(matrix(0, numNodos, truss))
  
  
  if(truss == 2){
    colnames(m) <- c("x", "y")
  } else {
    colnames(m) <- c("x", "y", "z")
  }
  
  # Se introdicen todas las cargas en las direcciones dependiendo el num del nodo
  for ( i in 1:numNodosCargas){
    m[cargas[i,1], ] <- cargas[i,2:(truss+1)]
  }
  
  # Se identifica la coordenada en donde hay cargas
  coorCargas <- which(m  != 0, arr.ind = T)
  
  # Se extrae la etiqueta del grado de libertad correpondiente a la carga asignada
  fzas <- numeric()
  
  for(i in 1:length(coorCargas[,1])){
   fzas[i] <-  EtiquetasGl[coorCargas[i,1], (coorCargas[i,2]+1)]
  }
  
  #Vector de fuerzas
  Vfzas <- rep(0, dof)
  
  for(i in 1:length(coorCargas[,1])){
    Vfzas[fzas[i]] <- m[coorCargas[i,1], coorCargas[i,2]]
  }
  
  Vfzas <- as.data.frame(cbind(1:dof, Vfzas))
  colnames(Vfzas) <- c("GradoLibertad", "Carga")
  
  return(Vfzas)
}
    
    
    
  
  
  