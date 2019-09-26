vdiscreta<-function(datos,lambda0,l,i,nom.vard,j, t){
  
  d <- datos[ nom.vard]
  p <- dim(d)[2] 
  #valores de las variables discretas de la observación i
  xds<-as.numeric(datos[i, nom.vard])
  

  
  lambda0 <- data.frame(lambda0,po=NA)
  
  #Se obtienen los valores de las p densidades poisson (suponiendo iid) 
  ## utlizando las lambdas calculadas con las distribuciones previas . 
  
  aux.po <- lambda0[which(lambda0$v.k==l),]## se filta por componente
  
  for(r in 1:p){
  aux.po$po[r] <- ifelse(j==1,dpois(xds[r],aux.po$lambda[r]),dpois(xds[r],aux.po$lambda[r]/t))
  }
  
  return(aux.po)
}
