##esta función obtiene el número de observaciones i que pertencen a cada componente
##ie la suma de las zij 
en_componente_multi<-function(sims,k){
  
  en_componente <- data.frame(v.k=c(1:k),zj=NA)
  
  for(l in 1:k){
  sims_componente <- filter(sims,v.k==l)
  en_componente$zj[l] <- sum(sims_componente$zij)
  }

  return(en_componente)
  
}

