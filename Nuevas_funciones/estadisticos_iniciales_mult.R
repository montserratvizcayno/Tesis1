estadisticos_iniciales_mult <- function(datos){
  
  ## esta función regresa una base con la media y varianza para cada
  ## variable de la base de datos (se toman solo las variables numéricas)
  
  nums <- sapply(datos, is.numeric)
  
  datos[ , nums]
  
  res <- datos[,nums]
  media<-colMeans(res)

  resultado<-data.frame(media,cov(datos[ ,nums]))

  
  return(resultado)
  
}
