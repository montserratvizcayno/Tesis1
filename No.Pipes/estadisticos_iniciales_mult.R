estadisticos_iniciales_mult <- function(datos){
  
  ## esta función regresa una base con la media y varianza para cada
  ## variable de la base de datos (se toman solo las variables numéricas)
  
  nums <- sapply(datos, is.numeric)
  
  datos[ , nums]
  
  covar<-melt(cov(datos[ ,nums]))
  covar <- rename(covar,c(X1="variable",X2="O"))
  covar <- dcast(covar,variable~O)
  
  res <- datos[,nums]
  res <- gather(res,variable, value)
  res <- group_by(res,variable) 
  res <- summarise(res,media = mean(value)) 
  
  
  resultado<-merge(res,covar)
  
  return(resultado)
  
}
