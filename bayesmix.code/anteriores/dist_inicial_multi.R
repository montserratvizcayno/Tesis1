dist_inicial_multi <- function(datos, nom.varc = names(datos), k ){
  #se generan los parámetros de las distribuciones de parámetros continuos
  # variables continuas
  d<-datos[ ,nom.varc]
  
  est.init <- estadisticos_iniciales_mult(d)
  
  ##p es el número de variables continuas
  p<-dim(d)[2]
  ##numero de observaciones
  n<-dim(d)[1]
  param.init<-NULL
  
  
  ## hiperparÃ¡metros iniciales 
  nj <- ((32547/k)-1)
  vj <- 2*p
  #matriz de covarianza para los datos
  S<-cov(d)
  #vector de medias de los datos
  ms<-sapply(d,mean)
  
  #calcula las distribuciones previas de sigma y mu por componente
  for(l in 1:k){
    ## calcular la dist previa del parámetro Sigma
    ##  con una distribución wishart inversa (vj=p+1,S=matriz(covar))
    aux.sigmas <- riwish(vj,S) 

    ## calcular la dist previa del parámetro Mu 
    ## con una distribución normal multivariada(ms=Medias observadas,aux.sigmas=Sigmas)
    aux.mu0 <- as.vector(rmvnorm(1,ms,(aux.sigmas)/nj))
  
    ##Merge(aux.sigma0,aux.mu0,est.init,id=~variable)
    
    ## variables auxiliares par almacenar los parámetros e hiperparámetros
    aux<-data.frame(variable=row.names(aux.sigmas),v.k=rep(l,times=p),aux.sigmas,mu0=aux.mu0,
                    v=rep(vj,times=p),njs=rep(nj,times=p))
    
    param.init<-rbind(aux,param.init)
    
  }
  
  return(param.init)
  
}
