## esta funci?n calcula la variable latente para cada una de las Xi's y la asigna a un componente
var_latente_multi<-function(datos,nom.varc = names(datos),nom.vard=NULL,phis,
                            folio = NULL,j,i,k=2){
  
  epsilon<-data.frame(e=sample(k:100,k))%>%
    transmute(epsilon=e/sum(e))
  
  deltas<-phis%>%
    mutate(sum.phi=sum(phi),delta=(ifelse(sum.phi!=0, phi/sum.phi, epsilon[v.k,])),
    #mutate(sum.phi=sum(phi),delta=phi/sum.phi)%>%
    #rowwise()%>%
   zij=as.numeric(rmultinom(1,1,delta)),
           id = as.character(datos[i, folio]),
           sim_interna = i,
           sim = j)%>%
    dplyr::select(-sum.phi)%>%
    data.frame()
  
  
  aux<-deltas%>%
    filter(zij == 1)%>%
    data.frame()
  
  return<-(aux)
  
  
}
