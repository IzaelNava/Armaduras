\name{fuerzasInternas}
\alias{fuerzasInternas}
\title{
Fuerzas internas en los elementos.
}
\description{
Se obtienen las fuerzas internas en los elementos del sistema.
}
\usage{
fuerzasInternas(propiedades, long, cosDirec, EtiquetasNF, Desplazamientos, truss)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{propiedades}{data.frame, contiene las propiedades de m?dulo de elasticidad (E) y
el área de la sección (A) de cada elemento.}
\item{long}{ Como data.frame, contiene las longitudes de cada elemento.}
\item{cosDirec}{Como data.frame, contiene los cosenos directores de los elementos por ejes.}
\item{EtiquetasNF}{Como data.frame, contiene las etiquetas de grados de libertad del nodo inicial (N) y final (F)}
\item{Desplazamientos}{Desplazamientos obtenidos.}
\item{truss}{Espacio 2 o 3D.}
}
\value{
Devuelve las fuerzas internas en los elementos. Además, si está trabajando a tensión o compresión
}
