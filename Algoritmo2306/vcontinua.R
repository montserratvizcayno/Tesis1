vcontinua<-function(datos,param,l,i,nom.varc){
  
  ## Variables continuas
  cs<-datos[nom.varc]
  ##numero de variables continuas
  h<-dim(cs)[2]
  
  #valores de las variables continuas de la observación i
  xcs<-as.numeric(datos[i, nom.varc])
  
 ##se filtra por componente l la base param que contiene los parámetros iniciales
   param.init<-param%>%
    dplyr::filter(v.k==l)%>%
    data.frame()
    
  ##la matriz de covarianzas
  vmatrix<-as.matrix(param.init[,3:(h+2)])
   
  #Se obtienen los valores de la  normal multivariada 
  #con los parámetros calcualdos con las distribuciones previas
  aux.nor<-param.init%>%
    mutate(nor=dmvnorm(xcs,as.matrix(mu0),vmatrix))%>%
    data.frame()
  
  return(aux.nor)
  
}




