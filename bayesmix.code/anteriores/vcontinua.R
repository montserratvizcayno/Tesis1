vcontinua<-function(datos,param,l,i,nom.varc){
  
  ## Variables continuas
  cs<-datos[nom.varc]
  ##numero de variables continuas
  h<-dim(cs)[2]
  
  #valores de las variables continuas de la observaci?n i
  xcs<-as.numeric(datos[i, nom.varc])
  
  ### ERROR 999 - Ref `sim_mult.R` ll.79
  ##se filtra por componente l la base param que contiene los par?metros iniciales
  param.init <- filter(param,v.k==l) 
  param.init <- data.frame(param.init,nor=NA)

  ##la matriz de covarianzas
  vmatrix<-as.matrix(param.init[,3:(h+2)])
  
  #Se obtienen los valores de la  normal multivariada 
  #con los par?metros calcualdos con las distribuciones previas
  param.init$nor <- dmvnorm(xcs,as.matrix(param.init$mu0),vmatrix)
  

  
  return(param.init)
  
}




