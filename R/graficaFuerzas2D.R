
graficaFuerzas2D <- function(elementos, nodos, FuerzasInter, long, lwd.lines= 2, labelNodos = FALSE, labelFza = FALSE){

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

    coorY[[i]]    <- 0
    coorY[[i]][2] <- 0

    coorZ[[i]]    <- matrizNodos[ni[i],3]
    coorZ[[i]][2] <- matrizNodos[nj[i],3]
  }

  nodosinter <- nodosIntermedios(elementos, nodos)
  nodosinter2D <- data.frame(x=nodosinter[,1], y=rep(0, length(nodosinter[,2])), z=nodosinter[,2])
  esferas    <- data.frame(x=matrizNodos[,2], y=rep(0, length(matrizNodos[,2])), z=matrizNodos[,3])


  # Colores en barras -----------------------------#
  # minimo <-  min(FuerzasInter[,2])
  # maximo <-  max(FuerzasInter[,2])
  #
  # distancia <- abs(maximo - minimo)
  # nn <- 100
  # paso <- distancia/nn
  #
  # secuencia <- seq(minimo, maximo, paso)
  #
  # fun_color_range <- colorRampPalette(c("red", "blue"))
  # my_colors <- fun_color_range(nn+1)
  #
  # intervalos <- findInterval(FuerzasInter[,2], secuencia)

  colores <- character()
  for (i in 1:length(FuerzasInter[,3])){
    if(FuerzasInter[i,3] == "Compresión"){
      colores[i] <- "red"
    } else {
      if(FuerzasInter[i,3] == "Tensión"){
        colores[i] <- "blue"
      } else {
        colores[i] <- "black"
      }
    }
  }
  #------------------------------------------------#

  l <- max(long[,2])
  radio <- 0.10 * l

  rgl::open3d()
  p0 <- c(0, 0, 0)
  r <- radio
  radio.esf <- max(abs(matrizNodos[,-1]))/120
  rgl::arrow3d(p0, c(r, 0, 0), thickness= radio * 2, type = "rotation", col = "blue")
  #rgl::arrow3d(p0, c(0, r, 0), thickness= radio * 2, type = "rotation", col = "green")
  rgl::arrow3d(p0, c(0, 0, r), thickness= radio * 2, type = "rotation", col = "red")

  rgl::decorate3d(xlab = "x", ylab = "", zlab = "y",
             box = FALSE, axes = FALSE, main = NULL, sub = NULL,
             top = FALSE, aspect = FALSE, expand = 1.2)

  rgl::rgl.bg(color = "white") # Setup the background color
  rgl::spheres3d(esferas, radius = radio.esf, col = "cyan") # Scatter plot

  for(i in 1:length(ni)){
    #lines3d(coorX[[i]], coorY[[i]], coorZ[[i]], col=my_colors[intervalos[i]], lwd=lwd.lines)
    rgl::lines3d(coorX[[i]], coorY[[i]], coorZ[[i]], col=colores[i], lwd=lwd.lines)
  }

  if (labelNodos == TRUE){
    rgl::text3d(esferas, texts=matrizNodos[,1], col = 4, adj = c(1.5, 1.5))
  } else {
    NULL
  }
  if (labelFza == TRUE){
    rgl::text3d(nodosinter2D, texts=round(FuerzasInter[,2], 2), col = "black", adj = c(0.5, 0.5))
  } else {
    NULL
  }



}

