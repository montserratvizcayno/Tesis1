

##Se calcula la previa de la variable latente mediante una Dirichlet

pi_init<-function(k){
  
  
  probs<-rep(1/k,k)
  
  pi <- rdirichlet(1, probs)
  
  colnames(pi)<-c(1:k)
  rownames(pi)<-"alfa"
  

  pis <- data.frame(v.k=colnames(pi),pi=as.vector(pi[1:k]),p.alfa=probs)

  
  return(pis)
  
}
