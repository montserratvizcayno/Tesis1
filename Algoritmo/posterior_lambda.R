##se obtienen los parámetros para las distribuciones posteriores discretas.
posterior_lambda<-function(hlambda.post,nom){
  
  aux.postlambda <- hlambda.post %>%
    rowwise() %>% 
    mutate(lambda= rgamma(1, shape=(a/2), scale=(sqrt(varianza)/2))) %>%
    data.frame()%>%
    dplyr::select(one_of("v.k","variable","media","varianza","a","lambda"))
  
  return(aux.postlambda)

}
