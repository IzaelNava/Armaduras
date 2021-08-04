\name{cosenoDirector}
\alias{cosenoDirector}
\title{
Cosenos directores.
}
\description{
Se obtienen los cosenos directores de los elementos para cada eje de analisis.
}
\usage{
cosenoDirector(dlta, long, truss)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dlta}{deltas de los elementos por eje. Está dado como un data.frame. Resultado de aplicar la funcion deltaxyz().
  Para mayor referencia ver dicha funcion.
}
\item{long}{Longitudes de los elementos. Están dados como data.frame. Resultado de aplicar la funcion longitud().
  Para mayor referencia ver dicha funcion.
}
\item{truss}{Armadura en 2D o 3D. Si truss == 2, se cosidera 2D, de otra forma es 3D.
}
}

\author{
Dr. Izael Nava
}

\examples{
 dlta <- data.frame(Elemn=c(1,2), x=c(6,6), y=c(0,0), z=c(0,0))
 long <- data.frame(Elemn=c(1,2), long=c(6,6))
 truss <- 3

 cosDir <- cosenoDirector(dlta, long, truss)
 }
