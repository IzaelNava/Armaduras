
reporte <- function(Resultados, archivo, truss, imprimir, uniLong = "metros", uniCarga = "Toneladas"){

  if(imprimir == TRUE){

    t.ini <- proc.time()
    salida  <- paste0("Reporte_", archivo, ".txt")


    # readline(prompt = "Nombre del archivo de resultados:\n")
    salida  <- as.character(salida)
    if(file.exists(salida)) file.remove(salida)

      cat('\nANALISIS DE ARMADURAS EN 2D Y 3D', file = salida, append = TRUE)
      #cat('\nELABORADO POR: DR. IZAEL NAVA VIVEROS', file = salida, append = TRUE)
      cat('\nMODELO:', archivo,'\n', file = salida, append = TRUE)
      cat('\n\n', file = salida, append = TRUE)
      #cat('\n\n', file = salida, append = TRUE)
      cat('\n#RESUMEN----------------------------------------------------------------#\n', file = salida, append = TRUE)
      #cat('\nNúmero de elementos    = ', numElem, '\n', file = salida, append = T)
      cat('\nNumero de nodos              = ', Resultados$numNodos, file = salida, append = T)
      cat('\nNumero de elementos          = ', Resultados$numElem, file = salida, append = T)
      cat('\nNumero de nodos restringidos = ', Resultados$numRestr, file = salida, append = T)
      cat('\nNumero de nodos con cargas   = ', Resultados$numNodosCargas, file = salida, append = T)
      cat('\nNumero de grados de libertad = ', Resultados$dof, file = salida, append = T)
      cat('\nNumero de dimensiones        = ', Resultados$truss, '\n', file = salida, append = T)

      cat('\n\n', file = salida, append = TRUE)
      cat('\n#Longitud de elementos--------------------------------------------------#\n', file = salida, append = TRUE)
      cat("Elemento  Longitud", "(" , uniLong, ")","\n", file = salida, append = T)
      write.table(round(Resultados$long, 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE, row.names = FALSE)
    TRUE
      cat('\n\n', file = salida, append = TRUE)
      cat('\n#Cargas en los nodos----------------------------------------------------#\n', file = salida, append = TRUE)
      if(truss == 2){
        cat("Elemento\tx\ty", "(" , uniCarga, ")","\n", file = salida, append = T)
        write.table(round(Resultados$cargas, 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
      } else{
        cat("Elemento\tx\ty\tz", "(" , uniCarga, ")","\n", file = salida, append = T)
        write.table(round(Resultados$cargas, 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
      }

      cat('\n\n', file = salida, append = TRUE)
      cat('\n#Etiquetas de grados de libertad----------------------------------------#\n', file = salida, append = TRUE)
      if(truss == 2){
        cat("Elemento\tx\ty", "\n", file = salida, append = T)
        write.table(round(Resultados$EtiquetasGl, 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
      } else{
        cat("Elemento\tx\ty\tz", "\n", file = salida, append = T)
        write.table(round(Resultados$EtiquetasGl, 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
      }

      cat('\n\n', file = salida, append = TRUE)
      cat('\n#Cosenos directores-----------------------------------------------------#\n', file = salida, append = TRUE)
      if(truss == 2){
        cat("Elemento\tx\ty", "\n", file = salida, append = T)
        write.table(round(Resultados$cosDirec, 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
      } else{
        cat("Elemento\tx\ty\tz", "\n", file = salida, append = T)
        write.table(round(Resultados$cosDirec, 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
      }

      cat('\n\n', file = salida, append = TRUE)
      cat('\n#matriz de rigidez por elemento------------------------------------------#\n', file = salida, append = TRUE)
      #cat("Elemento\tLongitud", "(" , uniLong, ")","\n", file = salida, append = T)
      for(i in 1:length(Resultados$matRigidecesElem)){
        cat("Elemento", Resultados$long[i,1], "\n", file = salida, append = TRUE)
        print(paste("Elemento", Resultados$long[i,1]))
        write.table(round(Resultados$matRigidecesElem[[i]][], 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
        cat('\n\n', file = salida, append = TRUE)
      }

      cat('\n\n', file = salida, append = TRUE)
      cat('\n#matriz Global del sistema-----------------------------------------------#\n', file = salida, append = TRUE)
      #cat("Elemento\tLongitud", "(" , uniLong, ")","\n", file = salida, append = T)
      write.table(round(Resultados$matGlobal, 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)


      cat('\n\n', file = salida, append = TRUE)
      cat('\n#Desplazamientos en los Nodos--------------------------------------------#\n', file = salida, append = TRUE)

      coor <- matrix(0, length(Resultados$EtiquetasGl[,1]), 2)
      tabladespl <- matrix(0, length(Resultados$EtiquetasGl[,1]), truss)

      for (i in 1:length(Resultados$EtiquetasGl[,1])){
        coor[i,] <- which(Resultados$Desplazamientos[i,1] == Resultados$EtiquetasGl[,-1], arr.ind=TRUE)
        tabladespl[coor[i,1], coor[i,2]] <- Resultados$Desplazamientos[i,2]
      }

      tabladespl <- data.frame(Nodo=Resultados$EtiquetasGl[,1], tabladespl)
      if(truss == 2){
        cat("\tNodo\tx\ty",  "(" , uniLong, ")","\n", file = salida, append = T)
        write.table(round(tabladespl, 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
      } else{
        cat("\tNodo\tx\ty\tz",  "(" , uniLong, ")","\n", file = salida, append = T)
        write.table(round(tabladespl, 4), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
      }

      cat('\n\n', file = salida, append = TRUE)
      cat('\n#Reacciones--------------------------------------------------------------#\n', file = salida, append = TRUE)
      #cat("Elemento\tLongitud", "(" , uniLong, ")","\n", file = salida, append = T)
      if(truss == 2){
        cat("\tNodo\tx\ty", "\n", file = salida, append = T)
        write.table(format(Resultados$mtReacc, 6), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
      } else{
        cat("\tNodo\tx\ty\tz", "\n", file = salida, append = T)
        write.table(format(Resultados$mtReacc, 6), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)
      }

      cat('\n\n', file = salida, append = TRUE)
      cat('\n#Fuerzas internas--------------------------------------------------------#\n', file = salida, append = TRUE)
      cat("\tElemento\tFzas Internas\tEstado",  "(" , uniCarga, ")","\n", file = salida, append = T)
      tablaFzas <- data.frame(elemento=Resultados$FuerzasInter[,1], Fuerza=round(Resultados$FuerzasInter[,2],4),
                              Estado=Resultados$FuerzasInter[,3])
      write.table(format(tablaFzas, 6), file = salida, append = TRUE, quote = FALSE, sep = "\t", col.names = FALSE)

      tiempo <- proc.time() - t.ini
      cat("\nTiempo de ejecución (s)  = ", tiempo[3], "\n")

  } else {
    NULL
  }


}
