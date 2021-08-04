\name{kglobal}
\alias{kglobal}
\title{
Matriz de rigidez global de la estructura.
}
\description{
Se obtiene la matriz global de la estructura..
}
\usage{
kglobal(K, EtiquetasNF, matRigidecesElem, truss)
}

\arguments{
  \item{K}{Matriz de ceros. El número de filas y columnas est? dado por el número de nodos * 2 ( si es 3D) o 3 (si es 3D)}
  \item{EtiquetasNF}{Como data.frame, contiene las etiquetas de grados de libertad del nodo inicial (N) y final (F)}
  \item{matRigidecesElem}{matrices de rigidez de cada elemento}
  \item{truss}{Escalar. Dimensiones 2D o 3D. si es 2D; truss <- 2. De lo contrario, truss <- 3}
}

\value{
Devuelve la matriz de rigidez global del sistema estructural
}

\examples{
# 3D y numero de nodos=3
dof <- 3 * 3
K <- matrix(0, dof, dof)
EtiquetasNF <- data.frame(elem=1:3, Nx=c(1, 4, 7), Ny=c(2, 5, 8), Nz=c(3, 6, 9),
                          Fx=c(4, 7, 1), Fy=c(5, 8, 2), Fz=c(6, 9, 3))
truss <- 3
matKglobal <- kglobal(K, EtiquetasNF, truss)
matKglobal
}

