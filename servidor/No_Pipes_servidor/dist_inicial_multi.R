dist_inicial_multi <- function(datos, nom.var = names(datos), k ){
  
  # variables continuas
  d<-datos[ ,nom.var]
  
  est.init <- estadisticos_iniciales_mult(d)
  
  ##p es el n?mero de variables continuas
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
    ## calcular la dist previa del par?metro Sigma
    ##  con una distribuci?n wishart inversa (vj=p+1,S=matriz(covar))
    aux.sigmas <- riwish(vj,S) 
    
    
    aux.sigma0 <- melt(aux.sigmas,measure.vars=c('Ing_tot','Monto_prom','Saldo'))
    
    aux.sigma0 <- rename(aux.sigma0,c(Var1="variable"))
    
    aux.sigma0 <- dcast(aux.sigma0,variable~Var2)
    
    
    ## calcular la dist previa del par?metro Mu 
    ## con una distribuci?n normal multivariada(ms=Medias observadas,aux.sigmas=Sigmas)
    aux.mu0 <- data.frame(rmvnorm(1,ms,(aux.sigmas)/nj))
    aux.mu0 <- melt(aux.mu0,measure.vars=c('Ing_tot','Monto_prom','Saldo'))
    aux.mu0 <- rename(aux.mu0,c(value="mu0"))  
    
    aux.merge<-merge(aux.sigma0,aux.mu0,by='variable')
    aux.merge<-merge(aux.merge,est.init,by='variable')
    #Merge(aux.sigma0,aux.mu0,est.init,id=~variable)
    
    ## variables auxiliares par almacenar los par?metros e hiperpar?metros
    aux<-data.frame(v.k=rep(l,times=p),aux.merge,
                    v=rep(vj,times=p),njs=rep(nj,times=p))
    
    param.init<-rbind(aux,param.init)
    
  }
  
  return(param.init)
  
}

