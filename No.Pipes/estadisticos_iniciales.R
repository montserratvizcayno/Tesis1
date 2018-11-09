estadisticos_iniciales <- function(datos){
  
  ## esta función regresa una base con la media y varianza para cada
  ## variable de la base de datos (se toman solo las variables numéricas)
  
  nums <- sapply(datos, is.numeric)
  
  datos[ , nums]
  
  res <- datos[,nums] 
  res <- gather(res,variable, value) 
  res <- group_by(res,variable)
  res <- summarise(res,media = mean(value),
                   varianza = var(value)) 
 
  return(res)
  
}