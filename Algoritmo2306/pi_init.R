
pi_init<-function(k){
  
  probs<-rep(k,k)
  
  pi <- rdirichlet(1, probs)
  
  colnames(pi)<-c(1:k)
  rownames(pi)<-"alfa"
  
  pis<-melt(pi,"alfa")%>%
    setNames(c("hparam","v.k","pi"))%>%
    dplyr::select(v.k,pi)%>%
    mutate(p.alfa=probs)
  
  return(pis)
  
}
