##se calculan las nuevas medias por cada componente 
##ie la summa de las xis dentro de cada componenete entre el numero de observaciones del componente
por_componente_multi<-function(datos,zjs_barra,sims_vlat,asigna,nom.var,folio=NULL,k){
  
  asigna <- setNames(asigna,c(folio, "v.k"))
 
  aux.media <- left_join(datos,asigna, by = folio)
    
  aux <- filter(aux.media,!is.na(v.k))
  c <- ncol(aux)
  
  aux.ms <- as.data.frame(matrix(rep(NA,k*(c-1)),byrow=TRUE,nrow=k))
  aux.ms <- setNames(aux.ms,c("v.k",nom.var))
  
  for(l in 1:k){
    aux.m <- filter(aux,v.k==l)
    aux.ms[l,1] <- l
    aux.ms[l,2:(c-1)]<-apply(aux.m[,2:(c-1)],2,mean)
    
  }
  
  
  
  xjs_barra<-merge(zjs_barra,aux.ms,id=v.k)
  
  return(xjs_barra)
  
}

