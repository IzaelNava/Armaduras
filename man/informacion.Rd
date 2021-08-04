\name{informacion}
\alias{informacion}
\title{
Informacion del modelo.
}
\description{
Se extraen de un archivo .xlsx los datos del modelo de armadura a analizar.
El archivo .xlsx contiene 5 pestañas (coordenadas, elementos, propiedades, fuerzas y restricciones)
}
\usage{
informacion(archivo)
}
\arguments{
  \item{archivo}{ archivo .xlsx con datos de coordenadas, elementos, cargas, restricciones y propiedades.
}
}
\details{
Devuelve en forma de data.frame, la información de: coordenadas, elementos,
propiedades, fuerzas y restricciones del modelo a analizar.
}
