
graficaModelo <- function(elementos, nodos, long, cargas, restricciones, truss, lwd.lines= 2, diametro = 0, lados = 20,
                          labelNodos = FALSE, labelElem = FALSE, labelrestricc = FALSE, labelSeccion = FALSE){


  # Matriz de nodos
  matrizNodos <- nodos

  ni <- elementos[,2]
  nj <- elementos[,3]

  coorX <- list()
  coorY <- list()
  coorZ <- list()

  for (i in 1:length(ni)){
    coorX[[i]]    <- matrizNodos[ni[i],2]
    coorX[[i]][2] <- matrizNodos[nj[i],2]

    coorY[[i]]    <- matrizNodos[ni[i],3]
    coorY[[i]][2] <- matrizNodos[nj[i],3]

    coorZ[[i]]    <- matrizNodos[ni[i],4]
    coorZ[[i]][2] <- matrizNodos[nj[i],4]
  }

  #nodos intermedios
  nodosinter <- nodosIntermedios(elementos, nodos)

  #largo de las flechas
  nmax <- abs(max(nodos[,2:4])); nmin <- abs(min(nodos[,2:4]))
  radio <- 0.10 * max(nmax, nmin)

  #texto de restricciones
  textoRestr <- apply(restricciones[,2:4], 1, paste, collapse="")

  #plot
  rgl::open3d()
  p0 <- c(0, 0, 0)
  r <- radio
  radio.esf <- max(abs(matrizNodos[,-1]))/150
  rgl::arrow3d(p0, c(r, 0, 0), thickness= radio * 2, type = "rotation", col = "blue")
  rgl::arrow3d(p0, c(0, r, 0), thickness= radio * 2, type = "rotation", col = "green")
  rgl::arrow3d(p0, c(0, 0, r), thickness= radio * 2, type = "rotation", col = "red")

  rgl::decorate3d(xlab = "x", ylab = "y", zlab = "z",
             box = FALSE, axes = FALSE, main = NULL, sub = NULL,
             top = FALSE, aspect = FALSE, expand = 1.2)

  rgl::rgl.bg(color = "white") # Setup the background color
  rgl::spheres3d(matrizNodos[,2:4], radius = radio.esf, col = "cyan") # Scatter plot

  #lineas
  for(i in 1:length(ni)){
    rgl::lines3d(coorX[[i]], coorY[[i]], coorZ[[i]], col="orange", lwd=lwd.lines)
  }

  #Tubos
  if(labelSeccion == TRUE){
    for(i in 1:length(ni)){
      m <- matrix(c(coorX[[i]][1], coorY[[i]][1], coorZ[[i]][1],
                    coorX[[i]][2], coorY[[i]][2], coorZ[[i]][2]), 2, 3, byrow = TRUE)

      rgl::shade3d(rgl::addNormals(
        rgl::cylinder3d(m, radius = ifelse(diametro == 0, radio.esf, diametro/2), sides = lados)),
        col = "yellow")
    }
  } else {
    NULL
  }

  #Flechas
  nodsFlechas <- nodoscargas(nodos, cargas, r, truss)

  for (i in 1:nodsFlechas$numcargTotal){
    rgl::arrow3d(nodsFlechas$ncargasi[i,], nodsFlechas$ncargasj[i,], type = "extrusion", col = "purple")
    rgl::text3d(nodsFlechas$ncargasi[i,], texts=paste(nodsFlechas$nombrecargas[i], "kg."), col = "purple", adj = c(1.0, 1.0))
  }

  #Etiquetas de nodos
  if (labelNodos == TRUE){
    rgl::text3d(matrizNodos[,2:4], texts=matrizNodos[,1], col = 4, adj = c(1.5, 1.5))
  } else {
    NULL
  }

  #Etiquetas de elementos
  if (labelElem == TRUE){
    rgl::text3d(nodosinter, texts=elementos[,1], col = "black", adj = c(1.5, 1.5))
  } else {
    NULL
  }

  #Etiquetas de restricciones
  if (labelrestricc == TRUE){
    rgl::text3d(matrizNodos[restricciones[,1],2:4], texts=textoRestr, col = "green", adj = c(1.0, 1.0))
  } else {
    NULL
  }





}

