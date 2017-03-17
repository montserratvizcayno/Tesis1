estadisticos_iniciales_mult <- function(datos){
  
  ## esta función regresa una base con la media y varianza para cada
  ## variable de la base de datos (se toman solo las variables numéricas)
  
  nums <- sapply(datos, is.numeric)
  
  datos[ , nums]
  
  covar<-cov(datos[ ,nums])
  
  res <- datos[,nums] %>%
    gather(variable, value) %>%
    group_by(variable) %>% 
    summarise(media = mean(value),
              varianza = var(value)) %>%
    ungroup()
  resulta<-list(res,covar)
  return(resulta)
  
}
