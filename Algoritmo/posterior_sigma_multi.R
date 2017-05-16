posterior_sigma_multi<-function(hparam.post,nom.varc,k){

  ynom.varc<-paste0(nom.varc,".y")
  xnom.varc<-paste0(nom.varc,".x")
  param.post<-NULL


for(l in 1:k){
  
  aux.pparam<-hparam.post%>%
    dplyr::filter(v.k==l)
  
  aux.s<-aux.pparam%>%
    dplyr::select(one_of(ynom.varc))%>%
    setNames(c(nom.varc))%>%
    as.matrix()
  
  rownames(aux.s)<-nom.varc
  
  aux.postsigma<-riwish(aux.pparam$v[1],aux.s)
    
   
  aux.posts0<-aux.postsigma%>%
    melt()%>%
    rename(c(X1="variable"))%>%
    dcast(variable~X2)
 
  
  ## calcular la dist previa del parámetro Mu con una distribución normal multivariada(Medias observadas,Sigmas)
  
  aux.postmed<-aux.pparam%>%
    dplyr::select(variable,media)%>%
    rename(c(variable="var"))%>%
    melt(id.vars="var")%>%
    dcast(variable~var)%>%
    dplyr::select(one_of(c(nom.varc)))%>%
    as.matrix()
  
  
  
  aux.postmu0 <-rmvnorm(1,aux.postmed,aux.postsigma/aux.pparam$njs[1])
  colnames(aux.postmu0)<-nom.varc
 
    aux.postmu0<-data.frame(aux.postmu0)%>%
      melt()%>%
      rename(c(value="mu0"))
  

  aux.post<-data.frame(v.k=l,Merge(aux.posts0,aux.postmu0,id=~variable),
                  aux.pparam[c("media",ynom.varc,"v","njs")])%>%
    setNames(c("v.k","variable",xnom.varc,"mu0","media",ynom.varc,"v","njs"))
  
  param.post<-rbind(aux.post,param.post)
  
  }
  
  return(param.post)
  
}
  

  
  
  