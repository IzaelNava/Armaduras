
determinacion <- function(b, r, j, truss){

  if (b + r == truss*j){
    leyenda <- "Estáticamente determinada"
  } else {
    if (b + r > truss*j){
      leyenda <- "Estáticamente indeterminada"
    } else {
      leyenda <- "Inestable"
    }
  }

  return(list(leyenda=leyenda, b=b, r=r, j=j, br=b+r, j2=2*j))
}
