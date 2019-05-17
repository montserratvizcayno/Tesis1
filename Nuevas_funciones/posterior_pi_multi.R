
posterior_pi_multi<-function(xjs_barra,pis,k){
  
  aux.alfa<-data.frame(xjs_barra[1],alfa=pis$p.alfa+xjs_barra$zj)
 
  post.pi<-rdirichlet(1,aux.alfa$alfa)
  
  colnames(post.pi)<-c(1:k)
  rownames(post.pi)<-"alfa"
  

  post.pis <- data.frame(v.k=colnames(post.pi),pi=as.vector(post.pi[1:k]),p.alfa=aux.alfa$alfa)

  
  return(post.pis)
  
}
