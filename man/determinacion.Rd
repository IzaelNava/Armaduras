\name{determinacion}
\alias{determinacion}
\title{
Determina la estabilidad de la armadura.
}
\description{
Determina si la armadura es estáticamente determinada, indeterminada o inestable.
}
\usage{
determinacion(b, r, j)
}
\arguments{
  \item{b}{Número de barras.}
  \item{r}{Número de restricciones.}
  \item{j}{Número de nodos.}
}
\details{
Si, b+r == 2*j, la estructura es estáticamente determinada. Por otro lado, si b+r > 2*j,
la armadura es estáticamente indeterminada. Finalmente, si b+r < 2*j, el sistema es inestable.
}
\value{
Devuelve la leyenda; determinada, indeterminada o inestable. Además, los valores de b, r, j, b+j y 2*j.
}
\author{
Dr. Izael Nava Viveros
}
\examples{
b <- 3
r <- 3
j <- 3

determ <- determinacion(b, r, j)
}
