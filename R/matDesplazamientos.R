

matDesplazamientos <- function(numgl, despl, truss){
  
  mat1 <- matrix(0, max(numgl[,-1]), 2)
  
  for (i in 1:max(numgl[,-1])){
    mat1[i,] <- which(numgl[,-1]==despl[i,1], arr.ind=TRUE)
  }
  
  matDespl <- numgl[,-1]
  
  for ( i in 1:max(numgl[,-1])){
    matDespl[mat1[i,1], mat1[i,2]] <- despl[i,2]
  }
  
  matDespl <- cbind(numgl[,1], matDespl)
  
  matDespl <- matDespl[order(matDespl[,1]),]
  
  if(truss == 2){
    colnames(matDespl) <- c("nodo", "x", "y")
  } else {
    colnames(matDespl) <- c("nodo", "x", "y", "z")
  }
  
  matDespl <- data.frame(matDespl)
  
 return(matDespl) 
}
