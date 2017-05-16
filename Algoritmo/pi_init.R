
pi_init<-function(k){
  
  pi <- rdirichlet(1, rep(1/k,k))
  
  colnames(pi)<-c(1:k)
  rownames(pi)<-"alfa"
  
  pis<-melt(pi,"alfa")%>%
    setNames(c("hparam","v.k","pi"))%>%
    dplyr::select(v.k,pi)
  
  return(pis)
  
}
