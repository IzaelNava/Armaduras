\name{fuerzas}
\alias{fuerzas}
\title{
Fuerzas externas.
}
\description{
Fuerzas externas en los nodos de la armadura.
}
\usage{
fuerzas(cargas, numNodos, EtiquetasGl, numNodosCargas, truss, dof)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cargas}{Nodos con sus respectivas cargas.}
  \item{numNodos}{Número total de nodos.}
  \item{EtiquetasGl}{Etiquetas de los grados de libertad por nodo.}
  \item{numNodosCargas}{Número de nodos con cargas.}
  \item{truss}{Dimensiones de la armadura, 2D o 3D.}
  \item{dof}{Número total de grados de libertad.}
}
\value{
Regresa data.frame con los grados de libertad y las cargas asociados a ellos.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Dr. Izael Nava Viveros
}

\examples{
cargas <- data.frame(Nodo=c(1, 2, 3), x=c(-1e6, 0, 0), y=c(0, 0, 0), z=c(0, -80000, -1200000))
numNodos <- 5
EtiquetasGl <- data.frame(Nodos=1:5, x=c(1, 4, 7, 10, 13), y=c(2, 5, 8, 11, 14), z=c(3, 6, 9, 12, 15))
numNodosCargas <- 3
truss <- 3
dof <- truss * numNodos
fzas <- fuerzas(cargas, numNodos, EtiquetasGl, numNodosCargas, truss, dof)
}
