
post_lambda<-function(xjs_barra,lambda0, nom.vard,t=1/10){
  
  ##calcula los parámetros para la distribución posterior de las variables discretas
  
  anom.vard<-paste0("a.",nom.vard)
  
  aux.xjs<-xjs_barra%>%
    dplyr::select(one_of("v.k","zj",nom.vard))%>%
    melt(measure.vars=c(nom.vard),value.name="xj")%>%
    setNames(c("v.k","zj","variable","xj"))%>%
    mutate(zxl=zj*xj)

  aux.lambda<-lambda0%>%
    dplyr::select(one_of(c("v.k","variable","media","varianza","a")))%>%
    merge(aux.xjs,id=c("variable","v.k"))
  
  new.lambda<-aux.lambda%>%
    mutate(a=ifelse(variable=="Electronico",(a+zxl)/10,a+zxl),
           varianza=ifelse(variable=="Electronico",(varianza*t)+zj,varianza+zj),
           media=ifelse(variable=="Electronico",(media*t)+zj,media+zj))
  
  return(new.lambda) 
  
}
