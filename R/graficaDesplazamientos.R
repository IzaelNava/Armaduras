
graficaDesplazamientos <- function(elementos, nodos, numgl, despl, truss, long, incremento = 1, lwd.lines= 2, labelNodos = FALSE, labelElem = FALSE){

  # Matriz de nodos
  matrizNodos <- data.frame(nodos)
  #Matriz de nodos con desplazamiento
  matrizDesplazados <- matDesplazamientos(numgl, despl, truss)

  matrizNodosDesplazados <- matrizNodos + (matrizDesplazados * incremento)


  ni <- elementos[,2]
  nj <- elementos[,3]

  coorX <- list()
  coorY <- list()
  coorZ <- list()

  coorXdes <- list()
  coorYdes <- list()
  coorZdes <- list()

  for (i in 1:length(ni)){
    coorX[[i]]    <- matrizNodos[ni[i],2]
    coorX[[i]][2] <- matrizNodos[nj[i],2]

    coorY[[i]]    <- matrizNodos[ni[i],3]
    coorY[[i]][2] <- matrizNodos[nj[i],3]

    coorZ[[i]]    <- matrizNodos[ni[i],4]
    coorZ[[i]][2] <- matrizNodos[nj[i],4]

    #coordenadas elementos desplazados
    coorXdes[[i]]    <- matrizNodosDesplazados[ni[i],2]
    coorXdes[[i]][2] <- matrizNodosDesplazados[nj[i],2]

    coorYdes[[i]]    <- matrizNodosDesplazados[ni[i],3]
    coorYdes[[i]][2] <- matrizNodosDesplazados[nj[i],3]

    coorZdes[[i]]    <- matrizNodosDesplazados[ni[i],4]
    coorZdes[[i]][2] <- matrizNodosDesplazados[nj[i],4]
  }

  nodosinter <- nodosIntermedios(elementos, nodos)

  nmax <- abs(max(nodos[,2:4])); nmin <- abs(min(nodos[,2:4]))
  radio <- 0.10 * max(nmax, nmin)

  rgl::open3d()
  #rgl::spheres3d(coor, radius = radio, col = "cyan")
  p0 <- c(0, 0, 0)
  r <- radio
  radio.esf <- max(abs(matrizNodos[,-1]))/120
  rgl::arrow3d(p0, c(r, 0, 0), thickness= radio * 2, type = "rotation", col = "blue")
  rgl::arrow3d(p0, c(0, r, 0), thickness= radio * 2, type = "rotation", col = "green")
  rgl::arrow3d(p0, c(0, 0, r), thickness= radio * 2, type = "rotation", col = "red")

  rgl::decorate3d(xlab = "x", ylab = "y", zlab = "z",
             box = FALSE, axes = FALSE, main = NULL, sub = NULL,
             top = FALSE, aspect = FALSE, expand = 1.2)

  rgl::rgl.bg(color = "white") # Setup the background color
  rgl::spheres3d(matrizNodos[,2:4], radius = radio.esf, col = "cyan") # Scatter plot
  rgl::spheres3d(matrizNodosDesplazados[,2:4], radius = radio.esf, col = "red") # Nodos desplazados

  for(i in 1:length(ni)){
    rgl::lines3d(coorX[[i]], coorY[[i]], coorZ[[i]], col="orange", lwd=lwd.lines)
  }

  # Elementos con desplazamiento
  for(i in 1:length(ni)){
    rgl::lines3d(coorXdes[[i]], coorYdes[[i]], coorZdes[[i]], col="green", lwd=lwd.lines/2)
  }

  if (labelNodos == TRUE){
    rgl::text3d(matrizNodos[,2:4], texts=matrizNodos[,1], col = 4, adj = c(1.5, 1.5))
    rgl::text3d(matrizNodosDesplazados[,2:4], texts=matrizNodos[,1], col = "red", adj = c(1.5, 1.5))
  } else {
    NULL
  }
  if (labelElem == TRUE){
    rgl::text3d(nodosinter, texts=elementos[,1], col = "black", adj = c(1.5, 1.5))
  } else {
    NULL
  }




}

