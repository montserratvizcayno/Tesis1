
posterior_pi_multi<-function(xjs_barra,pis,k){
  
  aux.alfa<-data.frame(xjs_barra[1:2],pis$pi)%>%
    mutate(alfa=pis.pi+zj)%>%
    dplyr::select(v.k,alfa)
    
  post.pi<-rdirichlet(1,aux.alfa$alfa)
  
  colnames(post.pi)<-c(1:k)
  rownames(post.pi)<-"alfa"
  
  post.pis<-melt(post.pi,"alfa")%>%
    setNames(c("hparam","v.k","pi"))%>%
    dplyr::select(v.k,pi)
  
  return(post.pis)
   
}
