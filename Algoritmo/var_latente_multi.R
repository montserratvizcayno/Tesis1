## esta funci?n calcula la variable latente para cada una de las Xi's y la asigna a un componente
var_latente_multi<-function(datos,nom.varc = names(datos),nom.vard=NULL,phis,
                           folio = NULL,j,i,k=2){
 epsilon<-sample(k:100,k)
  
  deltas<-phis%>%
    mutate(sum.phi=sum(phi),delta=(ifelse(sum.phi!=0, phi/sum.phi, 1/epsilon[v.k])))%>%
    #mutate(sum.phi=sum(phi),delta=phi/sum.phi)%>%
    rowwise()%>%
    mutate(zij=rbernoulli(1,delta)*1,
           id = datos[i, folio],
           sim_interna = i,
           sim = j)%>%
    data.frame()
  
  aux<-deltas%>%
    #filter(zij == 1)%>%
    data.frame()
  
  return<-(aux)
  
 
  }
    
