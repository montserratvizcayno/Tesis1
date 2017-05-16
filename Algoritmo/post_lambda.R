
post_lambda<-function(xjs_barra,lambda, nom.vard){
  
  ##calcula los parámetros para la distribución posterior de las variables discretas
  
  anom.vard<-paste0("a.",nom.vard)
  
  aux.xjs<-xjs_barra%>%
    dplyr::select(one_of("v.k","zj",nom.vard))%>%
    melt(measure.vars=c(nom.vard),value.name="Xj")%>%
    setNames(c("v.k","zj","variable","xj"))%>%
    mutate(zxl=zj*xj)

  aux.lambda<-lambda%>%
    dplyr::select(one_of(c("v.k","variable","media","varianza","a")))%>%
    merge(aux.xjs,id=c("variable","v.k"))
  
  new.lambda<-aux.lambda%>%
    mutate(a=a+zxl,varianza=varianza+zj)
   
  return(new.lambda) 
  
}
