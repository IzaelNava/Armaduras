\name{graficaDesplazamientos}
\alias{graficaDesplazamientos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modelo de desplazamientos.
}
\description{
Se grafican los desplazamientos junto al modelo de la estructura.
}
\usage{
graficaDesplazamientos(elementos, nodos, numgl, despl, truss, incremento = 2, lwd.lines= 2, labelNodos = FALSE, labelElem = FALSE)
}
\arguments{
  \item{elementos}{Elementos de la armadura. Nodo inicial y nodo final.}
  \item{nodos}{Coordenadas de los nodos.}
  \item{numgl}{Número total de grados de libertad.}
  \item{despl}{Desplazamientos en los nodos que han sido calculados.}
  \item{truss}{Dimension de la armadura. 2 o 3 dimensiones.}
  \item{incremento}{Factor que incrementa los desplazamientos, a fin de verlos mejor en la gráfica.}
  \item{lwd.line}{Ancho de las líneas de las armaduras}
  \item{labelNodos}{Si es TRUE, muestra la etiquetas de los nodos.}
  \item{labelElem}{Si es TRUE, Muestra la etiqueta de los elementos.}
}
