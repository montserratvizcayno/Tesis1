dist_init_discreta <- function(datos, nom.vard = names(datos), 
                               componente = 2, a=1,t=1/10){
  
  ##a es el par�metro de forma y se le da un valor de 1 por default
  
  #selecciona las variables discretas
  d <- datos[ nom.vard]
  p <- dim(d)[2] #n�mero de variables discretas
  
  #calcula los estadisticos iniciales (media y var) de las variables discretas
  est.init <- estadisticos_iniciales(d)
  
  k <- componente
  
  
  ## hiperparámetros iniciales 
  
  #vector con los p hiperpar�metros de forma 
  aj <- rep(a, times=k, each=p)
  
  #vector con el n�mero de componente
  v.k<-rep(1:k,each=p)
  
  
  ## calcular la distribuci�n previa  Gamma(aj,varianza) del par�metro lambda 
  ## (cuando las variables discretas son iid )
  aux.lambda0 <- data.frame(v.k,variable=row.names(est.init),
                            media=est.init$media,varianza=est.init$varianza,
                            a = aj, lambda=NA)
  v <- k*p
  
  for(i in 1:v){
    aux.lambda0$lambda[i] <- rgamma(1,shape=aux.lambda0$a[i],
                                    scale=aux.lambda0$media[i])
  }
  
  
  lambda0 <- aux.lambda0
  
  return(lambda0)
}

