\name{analisis}
\alias{analisis}

\title{
Analisis matricial en armaduras
}
\description{
Se realiza en análisis de armaduras en 2 o 3D utilizando el método matricial.
}
\usage{
analisis(archivo, truss, lwd.lines= 2, diametro = 0, lados =  20, incremento = 1,
                     labelNodos = FALSE, labelElem = FALSE, labelrestricc = FALSE, labelSeccion = FALSE,
                     modlo = FALSE, desplaza = FALSE, fInternas = FALSE, labelFza = TRUE, imprimir = TRUE)
}

\arguments{
  \item{archivo}{Archivo .xlsx con la información de nodos, elementos, propiedades, restricciones y cargas.}
  \item{truss}{Espacio 2D o 3D.}
  \item{lwd.lines}{Ancho de las línes de gráficos.}
  \item{diametro}{Diámetro del elemento,( si es tubular)}
  \item{lados}{Número de lados con los que se dibujará el cilindro. Se recomienda dejar en 20.}
  \item{incremento}{Incremento para observar los desplazamientos en caso de ser muy pequeños.}
  \item{labelNodos}{Etiquetas de los nodos en el gráfico del modelo. Si es TRUE se mostrarán en el modelo.}
  \item{labelElem}{Etiquetas de los elementos en el gráfico del modelo. Si es TRUE se mostrarán en el modelo.}
  \item{labelrestricc}{Etiquetas de las restricciones en el gráfico del modelo. Si es TRUE se mostrarán en el modelo.}
  \item{labelSeccion}{Sección en 3D del gráfico del modelo. Si es TRUE se mostrarán en el modelo.}
  \item{modlo}{Si es TRUE, se mostrará el gráfico del modelo.}
  \item{desplaza}{Si es TRUE, se mostrará el gráfico de los desplazamientos.}
  \item{fInternas}{Si es TRUE, se mostrará el gráfico de las fuerzas internas.}
  \item{labelFza}{Si es TRUE, se mostrará el gráfico de las fuerzas esternas.}
  \item{imprimir}{Si es TRUE, se imprimen los resultados en un .txt.}

}
\details{
Se hace uso de las funciones dentro de la paquetería para realizar el análisis estructural de la armadura
}
