\name{desplazamientos}
\alias{desplazamientos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Desplazamientos del sistema.
}
\description{
Se obtienen los desplazamientos en los nodos.
}
\usage{
desplazamientos(matGlobal, fzasExt, restricciones, dof)
}
\arguments{
  \item{matGlobal}{Matriz global de rigidez del sistema estructural.}
\item{fzasExt}{Fuerzas externas asociadas a los grados de libertad del sistema.}
\item{restricciones}{Restricciones del sistema. Grados de libertad que tienen restricción (1).}
\item{dof}{Número total de grados de libertad en el sistema.}
}
\value{
Devuelve los desplazamientos en los grados de libertad que no están restringidos...
}
\author{
Dr. Izael Nava Viveros
}
