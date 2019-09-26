posterior_sigma_multi<-function(hparam.post,nom.varc,xjs_barra,k){
  
  ynom.varc<-paste0(nom.varc,".y")
  xnom.varc<-paste0(nom.varc,".x")
  param.post<-NULL
  
  
  for(l in 1:k){
    
    indicadora<-filter(xjs_barra,v.k==l)
    
    indice<- nrow(indicadora)
    
    
    
    if(indice!=0){
      
      aux.pparam<-filter(hparam.post,v.k==l)
      
      aux.s<-aux.pparam[c(ynom.varc)]
      aux.s <- as.matrix(setNames(aux.s,c(nom.varc)))
      aux.s<-set_rownames(aux.s, c(nom.varc))
      
      indice.s<-nrow(aux.s)
      
      if(indice.s!=0){
        rownames(aux.s)<-nom.varc
      }
      
      aux.postsigma<-riwish(as.numeric(aux.pparam$v[1]),aux.s)

      
  
      ##aux.posts0 <- rename(aux.posts0,c(Var1="variable"))
      ##aux.posts0 <- dcast(aux.posts0,variable~Var2)
      
      
      ## calcular la dist previa del parámetro Mu con una distribución normal multivariada(Medias observadas,Sigmas)
      
      aux.postmed<-aux.pparam[c("variable","media")]

      
      aux.postmu0 <-rmvnorm(1,aux.postmed$media,aux.postsigma/aux.pparam$njs[1])
      colnames(aux.postmu0)<-nom.varc

     aux.post<- data.frame(v.k=l,variable=colnames(aux.postsigma),aux.postsigma,
                           mu0=as.vector(aux.postmu0),aux.pparam[c("media",ynom.varc,"v","njs")])
      ##aux.post <- setNames(aux.post,c("v.k","variable",
                              ##  xnom.varc,"mu0","media",ynom.varc,"v","njs"))
      
      param.post<-rbind(aux.post,param.post)
      
    }
    
  }
  
  return(param.post)
  
}

