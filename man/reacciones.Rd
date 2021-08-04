\name{reacciones}
\alias{reacciones}

\title{
Reacciones del sistema.
}
\description{
Se calculan las reacciones de la armadura.
}
\usage{
reacciones(matGlobal, Desplazamientos, dof, truss, restricciones, EtiquetasGl)
}

\arguments{
  \item{matGlobal}{Matriz global de rigidez del sistema estructural.}
  \item{Desplazamientos}{Desplazamientos obtenidos.}
  \item{dof}{Número total de grados de libertad en el sistema}
  \item{truss}{Dimensiones de la armadura. 2D o 3D.}
  \item{restricciones}{Nodos restringidos.}
  \item{EtiquetasGl}{Etiquetas de grados de libertad por nodo.}
}

\value{
Regresa las reacciones del sistema.
}
