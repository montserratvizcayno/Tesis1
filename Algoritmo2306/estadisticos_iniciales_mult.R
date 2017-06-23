estadisticos_iniciales_mult <- function(datos){
  
  ## esta función regresa una base con la media y varianza para cada
  ## variable de la base de datos (se toman solo las variables numéricas)
  
  nums <- sapply(datos, is.numeric)
  
  datos[ , nums]
  
  covar<-cov(datos[ ,nums])%>%
    melt()%>%
    rename(c(X1="variable",X2="O"))%>%
    dcast(variable~O)
  
  res <- datos[,nums] %>%
    gather(variable, value) %>%
    group_by(variable) %>% 
    summarise(media = mean(value)) %>%
    ungroup%>%
    data.frame()
  
  resultado<-merge(res,covar)
  
  return(resultado)
  
}


