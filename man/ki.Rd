\name{ki}
\alias{ki}
\title{
Matriz de rigidez de cada elemento.
}
\description{
Se obtienen las matrices de rigideces de cada elemento.
}
\usage{
ki(propiedades, long, cosDirec, EtiquetasNF, truss)
}
\arguments{
  \item{propiedades}{Como data.frame, contiene las propiedades de módulo de elasticidad (E) y
                     el área de la sección (A) de cada elemento.}
  \item{long}{Como data.frame, contiene las longitudes de cada elemento.}
  \item{cosDirec}{Como data.frame, contiene los cosenos directores de los elementos por ejes.}
  \item{EtiquetasNF}{Como data.frame, contiene las etiquetas de grados de libertad del nodo inicial (N) y final (F).}
  \item{truss}{Escalar. Dimensiones 2D o 3D. si es 2D; truss <- 2. De lo contrario, truss <- 3.}
}
\value{
Devuelve la en forma de lista, las matrices de rigideces de cada elemento con sus correspondientes etiquetas
de grados de libertad correspondientes
}
\examples{
propiedades <- data.frame(elemento=c(1, 2, 3), E=c(2E10, 2E10, 2E10), y=c(0.01, 0.01, 0.01))
long <- data.frame(elemento=c(1, 2, 3), long=c(6, 6, 6))
cosDirec <- data.frame(elem=1:3, x=c(1, 1, 1), y=c(0, 0, 0), z=c(0, 0, 0))
EtiquetasNF <- data.frame(elem=1:3, Nx=c(31, 1, 4), Ny=c(32, 2, 5), Nz=c(33, 3, 6),
                          Fx=c(1, 4, 7), Fy=c(2, 5, 8), Fz=c(3, 6, 9))
truss <- 3
matRigidecesElem <- ki(propiedades, long, cosDirec, EtiquetasNF, truss)
matRigidecesElem
}
