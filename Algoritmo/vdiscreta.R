vdiscreta<-function(datos,lambda0,l,i,nom.vard){
 
   #valores de las variables discretas de la observación i
  xds<-as.numeric(datos[i, nom.vard])
 
  #t es el parametro de escala que afecta a la variable electrónico

  
  #Se obtienen los valores de las p densidades poisson (suponiendo iid) 
  ## utlizando las lambdas calculadas con las distribuciones previas . 
  aux.po<-lambda0%>%
    dplyr::filter(v.k==l)%>% ## se filta por componente
    mutate(po=dpois(xds,lambda))%>%
    data.frame()
  
  return(aux.po)
}


