dist_inicial_multi <- function(datos, nom.var = names(datos), k ){
  
  # variables continuas
  d<-datos[ nom.var]
  
  est.init <- estadisticos_iniciales_mult(d)
  
  ##p es el número de variables continuas
  p<-dim(d)[2]
  ##numero de observaciones
  n<-dim(d)[1]
  param.init<-NULL
  
 
  ## hiperparÃ¡metros iniciales 
  nj <- ((n/k)-1)
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
  
  aux.sigma0<-aux.sigmas%>%
    melt()%>%
    rename(c(X1="variable"))%>%
    dcast(variable~X2)
 
  
  ## calcular la dist previa del parámetro Mu 
  ## con una distribución normal multivariada(ms=Medias observadas,aux.sigmas=Sigmas)
  aux.mu0 <-rmvnorm(1,ms,(aux.sigmas)/nj)%>%
    data.frame()%>%
    melt()%>%
    rename(c(value="mu0"))

  ## variables auxiliares par almacenar los parámetros e hiperparámetros
  aux<-data.frame(v.k=rep(l,times=p),Merge(aux.sigma0,aux.mu0,est.init,id=~variable),
                  v=rep(vj,times=p),njs=rep(nj,times=p))
  
  param.init<-rbind(aux,param.init)
  
  }
  
  return(param.init)
  
}




