dist_inicial_multi <- function(datos, nom.varc = names(datos), k ){
  #se generan los par�metros de las distribuciones de par�metros continuos
  # variables continuas
  d<-datos[ ,nom.varc]
  
  est.init <- estadisticos_iniciales_mult(d)
  
  ##p es el n�mero de variables continuas
  p<-dim(d)[2]
  ##numero de observaciones
  n<-dim(d)[1]
  param.init<-NULL
  
  
  ## hiperparámetros iniciales 
  nj <- ((32547/k)-1)
  vj <- 2*p
  #matriz de covarianza para los datos
  S<-cov(d)
  #vector de medias de los datos
  ms<-sapply(d,mean)
  
  #calcula las distribuciones previas de sigma y mu por componente
  for(l in 1:k){
    ## calcular la dist previa del par�metro Sigma
    ##  con una distribuci�n wishart inversa (vj=p+1,S=matriz(covar))
    aux.sigmas <- riwish(vj,S) 

    ## calcular la dist previa del par�metro Mu 
    ## con una distribuci�n normal multivariada(ms=Medias observadas,aux.sigmas=Sigmas)
    aux.mu0 <- as.vector(rmvnorm(1,ms,(aux.sigmas)/nj))
  
    ##Merge(aux.sigma0,aux.mu0,est.init,id=~variable)
    
    ## variables auxiliares par almacenar los par�metros e hiperpar�metros
    aux<-data.frame(variable=row.names(aux.sigmas),v.k=rep(l,times=p),aux.sigmas,mu0=aux.mu0,
                    v=rep(vj,times=p),njs=rep(nj,times=p))
    
    param.init<-rbind(aux,param.init)
    
  }
  
  return(param.init)
  
}
