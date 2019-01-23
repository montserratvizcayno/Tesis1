estadisticos_iniciales <- function(datos){
  
  ## esta función regresa una base con la media y varianza para cada
  ## variable de la base de datos (se toman solo las variables numéricas)
  
  nums <- sapply(datos, is.numeric)
  
  datos[ , nums]
  
  res <- datos[,nums] %>%
    gather(variable, value) %>%
    group_by(variable) %>% 
    summarise(media = mean(value),
              varianza = var(value)) %>%
    ungroup()
  return(res)
  
}
