
pi_init<-function(k){
  
  
  probs<-rep(k,k)
  
  pi <- rdirichlet(1, probs)
  
  colnames(pi)<-c(1:k)
  rownames(pi)<-"alfa"
  
  pis<-melt(pi,"alfa")
    
  pis <- setNames(pis,c("hparam","v.k","pi"))
  
  pis1 <- data.frame(v.k=pis$v.k,pi=pis$pi,p.alfa=probs)

  
  return(pis1)
  
}
