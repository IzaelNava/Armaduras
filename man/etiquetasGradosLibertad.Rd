\name{etiquetasGradosLibertad}
\alias{etiquetasGradosLibertad}
\title{
Etiquetas de grados de libertad en cada nodo.
}
\description{
Se obtienen las etiquetas de los grados de libertad que corresponden a cada nodo.
si un nodo está restringido, es cosiderado en la etiqueta y se coloca en su lugar correspondiente
con el fin de tener una matriz global ordenada.
}
\usage{
etiquetasGradosLibertad(numNodos, nodos, numRestr, restricciones, truss, dof)
}
\arguments{
  \item{numNodos}{Escalar. Número de nodos en el modelo.}
  \item{nodos}{Data.frame. Coordenadas de nodos del modelo.}
  \item{numRestr}{Escalar. Número de nodos que tienen restricciones.}
  \item{restricciones}{Data.frame. Nodos con restricciones. Si la direeción tiene 1,
  está restringido en dicha dirección. Si es 0, está libre.}
  \item{truss}{Si el modelo está en 2D <- 2, o 3D <- 3.}
  \item{dof}{Número total de grados de libertad.}
}
\value{
Devuelve en forma de data.frame, las etiquetas en cada nodo del modelo ( en sus respectivos ejes).
}
\examples{
truss <- 3
numNodos <- 2
nodos <- data.frame(nodos=c(1,2), x=c(0,6), y=c(0,0), z=c(0,0))
numRestr <- 2
restricciones <- data.frame(Nodo=c(1,1), x=c(1,0), y=c(0,1), z=c(1,1))
dof <- truss * numNodos

etGL <- etiquetasGradosLibertad(numNodos, nodos, numRestr, restricciones, truss, dof)
}

