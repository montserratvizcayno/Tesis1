## esta funci?n calcula la variable latente para cada una de las Xi's y la asigna a un componente
var_latente_multi<-function(datos,nom.varc = names(datos),nom.vard=NULL,phis,
                            folio = NULL,j,i,k=2){
  e <- sample(k:100,k)
  epsilon<-e/sum(e)
  
  sum.phi <- sum(phis$phi)
  deltas <- data.frame(phis,delta=NA,zij=NA, id = as.character(datos[i, folio]),sim_interna = i,
                       sim = j)
  for(l in 1:k){
    deltas$delta[l]<- ifelse(sum.phi!=0, phis$phi[l]/sum.phi, epsilon[l])
    
  }
  deltas$zij <- as.numeric(rmultinom(1,1,deltas$delta))
  
  return<-(deltas)
  
}
