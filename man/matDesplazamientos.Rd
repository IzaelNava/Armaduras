\name{matDesplazamientos}
\alias{matDesplazamientos}
\title{
matriz de desplazamientos.
}
\description{
Se produce una tabla con los nodos desplazados según su dirección.
}
\usage{
matDesplazamientos(numgl, despl, truss)
}
\arguments{
  \item{numgl}{Etiquetas de grados de libertad por nodo.}
  \item{despl}{Desplazamientos de los nodos.}
  \item{truss}{Dimensiones de la estructura. 2D o 3D.}
  }
\value{
Refresa como un data.frame, los desplazamientos de los nodos por dirección.
}
