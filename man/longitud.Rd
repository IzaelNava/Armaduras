\name{longitud}
\alias{longitud}
\title{
Longitud del elemento.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
longitud(dlta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dlta}{Como data.frame, contiene los elementos y deltas para cada eje de análisis.}
}
\value{
Devuelve la longitud de los elementos del modelo.
por ejemplo (siendo i un elemento cualquiera):
longitud.i = sqrt(deltax.i^2 + deltay.i^2 + deltaz.i^2)
}
\examples{
dlta <- data.frame(Elemn=c(1,2), x=c(6,6), y=c(0,0), z=c(00))
long <- longitud(dlta)
}
