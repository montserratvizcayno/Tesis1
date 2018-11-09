
post_lambda<-function(xjs_barra,lambda0, nom.vard,t=1/10){
  
  ##calcula los parámetros para la distribución posterior de las variables discretas
  
  anom.vard<-paste0("a.",nom.vard)
  
  aux.xjs<-xjs_barra[c("v.k","zj",nom.vard)]
  aux.melt <- melt(aux.xjs,measure.vars=c(nom.vard),value.name="xj")
  aux.xjs <- data.frame(aux.melt,zxl=aux.melt$zj*aux.melt$xj)
  aux.xjs <- setNames(aux.xjs,c("v.k","zj","variable","xj","zxl"))
  
  
  aux.lambda<-lambda0[c("v.k","variable","media","varianza","a")]
  aux.lambda <- merge(aux.lambda,aux.xjs,id=c("variable","v.k"))
  
  new.lambda<-data.frame(aux.lambda[1:2],media=aux.lambda$media+aux.lambda$zj,
                         varianza=aux.lambda$varianza+aux.lambda$zj,
                         a=aux.lambda$a+aux.lambda$zxl,aux.lambda[6:8])
  
  
  return(new.lambda) 
  
}