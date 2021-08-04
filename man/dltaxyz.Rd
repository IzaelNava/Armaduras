\name{dltaxyz}
\alias{dltaxyz}
\title{
Deltas (diferencia de coordenadas).
}
\description{
Los deltas de cada elemento se obtienen como la diferencias de las coordenas N
(considerado como el inicio del elemento) y F (final del elemento).
}
\usage{
dltaxyz(nodos, elementos, truss)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nodos}{Nodos del modelo como data.frame}
\item{Elementos}{Elementos del modelos como data.frame.}
\item{truss}{Armadura en 2D o 3D. Si truss == 2, se cosidera 2D, de otra forma es 3D.}
}

\value{
Devuelve los deltas de las corrdenadas x, y y/o z.
Por ejemplo: deltax = xF - xN ... para cada elemento.
}
\author{
Dr. Izael Nava Viveros
}
\examples{
nodos <- data.frame(nodos=1:2, x=c(0, 6), y=c(0, 0), z=c(0, 0))
elementos <- data.frame(Elemento=1, nN=1, nF=2)
truss <- 3
deltas <- dltaxyz(nodos, elementos, truss)
}
