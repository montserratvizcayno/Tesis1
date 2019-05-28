vcontinua<-function(datos,param,l,i,nom.varc){
  
  ## Variables continuas
  cs<-datos[nom.varc]
  ##numero de variables continuas
  h<-dim(cs)[2]
  
  #valores de las variables continuas de la observación i
  xcs<-as.numeric(datos[i, nom.varc])
  
  ##se filtra por componente l la base param que contiene los parámetros iniciales
  param.init <- filter(param,v.k==l) 
  param.init <- data.frame(param.init,nor=NA)

  ##la matriz de covarianzas
  vmatrix<-as.matrix(param.init[,3:(h+2)])
  
  #Se obtienen los valores de la  normal multivariada 
  #con los parámetros calcualdos con las distribuciones previas
  param.init$nor <- dmvnorm(xcs,as.matrix(param.init$mu0),vmatrix)
  

  
  return(param.init)
  
}




