
analisis <- function(archivo, truss, lwd.lines= 2, diametro = 0, lados =  20, incremento = 1,
                     labelNodos = FALSE, labelElem = FALSE, labelrestricc = FALSE, labelSeccion = FALSE,
                     modlo = FALSE, desplaza = FALSE, fInternas = FALSE, labelFza = FALSE, imprimir = FALSE) {

  tablas <- informacion(archivo)

  # ---------------------------------------------------------------------------------------------#
  # datos de entrada
  #truss <- 3

  nodos <- tablas$nodos
  cargas <- tablas$cargas
  elementos <- tablas$elementos
  propiedades <- tablas$propiedades
  restricciones <- tablas$restricciones

  numNodos  <- tablas$numNodos
  numElem   <- tablas$numElemn
  numRestr  <- tablas$numRestr
  numNodosCargas <- tablas$numCargs

  b <- numElem
  r <- sum(tablas$restricciones[,-1])
  j <- numNodos
  # --------------------------------------------------------------------------------------------- #
  # Determinación
  dter <- determinacion(b, r, j, truss)


  #Num de Grados de libertad

  dof <- truss * numNodos



  # K global ( matriz de ceros de dimensiones dof x dof)
  K <- matrix(0, dof, dof)

  # Obtencion de los deltas
  dlta <- dltaxyz(nodos, elementos, truss)

  # Longitud de los elementos
  long <- longitud(dlta)

  # Cosenos directores
  cosDirec <- cosenoDirector(dlta, long, truss)

  # Etiquetas de grados de libertad por cada nodo
  EtiquetasGl <- etiquetasGradosLibertad(numNodos, nodos, numRestr, restricciones,  truss, dof)

  # Fuerzas externas
  fzasExt <- fuerzas(cargas, numNodos, EtiquetasGl, numNodosCargas, truss, dof)

  # Etiquetas de grado libertad asociados a los nodos N y F.
  EtiquetasNF <- EtiquetasGLNF(EtiquetasGl , elementos, numElem, truss)

  # Matriz de rigidez por elemento
  matRigidecesElem <- ki(propiedades, long, cosDirec, EtiquetasNF, truss)

  # Matriz de rigidez global
  matGlobal <- kglobal(K, EtiquetasNF, matRigidecesElem, truss)

  # CÃ¡lculo de desplazamientos
  Desplazamientos <- desplazamientos(matGlobal, fzasExt, restricciones, dof)

  # CÃ¡lculo de reacciones
  Reacciones <- reacciones(matGlobal, Desplazamientos, dof, truss, restricciones, EtiquetasGl)

  # CÃ¡lculo de fuerzas internas
  FuerzasInter <- fuerzasInternas(propiedades, long, cosDirec, EtiquetasNF, Desplazamientos, truss)

  # Desplazamientos por nodos
  desplNodos <- matDesplazamientos(EtiquetasGl, Desplazamientos, truss)

  # Matriz de reacciones
  mtReacc <- matrizReacciones(Reacciones, restricciones, EtiquetasGl, truss)


  # GrÃ¡ficas del Modelo
  if(modlo == TRUE){
    if (truss == 2){
      graficaModelo2D(elementos, nodos, long, cargas, restricciones, truss, lwd.lines, diametro, lados,
                      labelNodos, labelElem, labelrestricc, labelSeccion)
    } else{
      graficaModelo(elementos, nodos, long, cargas, restricciones, truss, lwd.lines, diametro, lados,
                    labelNodos, labelElem, labelrestricc, labelSeccion)
    }
  } else {
    NULL
  }



  # GrÃ¡fica de desplazamientos
  if(desplaza == TRUE){
    if (truss == 2){
      graficaDesplazamientos2D(elementos, nodos, EtiquetasGl, Desplazamientos, truss, long,
                               incremento, lwd.lines, labelNodos, labelElem )

    } else{
      graficaDesplazamientos(elementos, nodos, EtiquetasGl, Desplazamientos, truss, long,
                             incremento, lwd.lines, labelNodos, labelElem)
    }
  } else {
    NULL
  }



  # Gráfica de fuerzas internas
  if(fInternas == TRUE){
    if (truss == 2){
      graficaFuerzas2D(elementos, nodos, FuerzasInter, long, lwd.lines, labelNodos, labelFza)
    } else{
      graficaFuerzas(elementos, nodos, FuerzasInter, long, lwd.lines, labelNodos, labelFza)
    }
  } else {
    NULL
  }
  # Resultados

  Resultados <- list(nodos=nodos, cargas=cargas, elementos=elementos,
                     propiedades=propiedades, restricciones=restricciones,
                     numNodos=numNodos, numElem=numElem, numRestr=numRestr,
                     numNodosCargas=numNodosCargas, dter=dter, dof=dof,
                     K=K, dlta=dlta, long=long, cosDirec=cosDirec, fzasExt=fzasExt,
                     EtiquetasGl=EtiquetasGl, EtiquetasNF=EtiquetasNF,
                     matRigidecesElem=matRigidecesElem, matGlobal=matGlobal,
                     Desplazamientos=Desplazamientos, Reacciones=Reacciones,
                     FuerzasInter=FuerzasInter, desplNodos=desplNodos, mtReacc=mtReacc
                     )

  if(imprimir == TRUE){
    reporte(Resultados, archivo, truss, imprimir)
  } else {
    NULL
  }


  return(Resultados)

}







