\name{graficaFuerzas}
\alias{graficaFuerzas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modelo de fuerzas internas en la armadura.
}
\description{
Se grafican las fuerzas internas junto al modelo de la estructura.
}
\usage{
graficaFuerzas(elementos, nodos, FuerzasInter, lwd.lines= 2, labelNodos = FALSE, labelElem = FALSE)
}
\arguments{
  \item{elementos}{Elementos de la armadura. Nodo inicial y nodo final.}
  \item{nodos}{Coordenadas de los nodos.}
  \item{FuerzasInter}{Fuerzas internas en los elementos que han sido estimadas.}
  \item{lwd.line}{Ancho de las líneas de las armaduras}
  \item{labelNodos}{Si es TRUE, muestra la etiquetas de los nodos.}
  \item{labelElem}{Si es TRUE, Muestra la etiqueta de los elementos.}
}
