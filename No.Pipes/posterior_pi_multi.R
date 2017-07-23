
posterior_pi_multi<-function(xjs_barra,pis,k){
  
  aux.alfa<-data.frame(xjs_barra[1],alfa=pis$p.alfa+xjs_barra$zj)
 
  post.pi<-rdirichlet(1,aux.alfa$alfa)
  
  colnames(post.pi)<-c(1:k)
  rownames(post.pi)<-"alfa"
  
  post.pis<-melt(post.pi,"alfa")
  post.pis <- setNames(post.pis[2:3],c("v.k","pi"))
  post.pis <- data.frame(post.pis,p.alfa=aux.alfa$alfa) 

  
  return(post.pis)
  
}
