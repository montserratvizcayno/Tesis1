estadisticos_iniciales <- function(datos){
  
  ## esta función regresa una base con la media y varianza para cada
  ## variable de la base de datos (se toman solo las variables numéricas)
  
  nums <- sapply(datos, is.numeric)
  
  datos[ , nums]
  
  res <- datos[,nums] 
  media<-colMeans(res)
  varianza<-diag(var(res))
  res<-data.frame(media,varianza)
 
  return(res)
  
}