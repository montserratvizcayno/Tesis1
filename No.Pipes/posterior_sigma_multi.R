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
      
      indice.s<-nrow(aux.s)
      
      if(indice.s!=0){
        rownames(aux.s)<-nom.varc
      }
      
      aux.postsigma<-riwish(aux.pparam$v[1],aux.s)
      
      
      aux.posts0<-melt(aux.postsigma)
      aux.posts0 <- rename(aux.posts0,c(Var1="variable"))
      aux.posts0 <- dcast(aux.posts0,variable~Var2)
      
      
      ## calcular la dist previa del par?metro Mu con una distribuci?n normal multivariada(Medias observadas,Sigmas)
      
      aux.postmed<-aux.pparam[c("variable","media")]
      aux.postmed <- rename(aux.postmed,c(variable="var"))
      aux.postmed <- melt(aux.postmed,id.vars="var")
      aux.postmed <- dcast(aux.postmed,variable~var)
      aux.postmed <- as.matrix(aux.postmed[c(nom.varc)] ) 
      
      
      aux.postmu0 <-rmvnorm(1,aux.postmed,aux.postsigma/aux.pparam$njs[1])
      colnames(aux.postmu0)<-nom.varc
      
      aux.postmu0<-melt(data.frame(aux.postmu0))
      aux.postmu0 <- rename(aux.postmu0,c(value="mu0"))
      

      
      aux.post<-data.frame(v.k=l,merge(aux.posts0,aux.postmu0,by='variable'),
                           aux.pparam[c("media",ynom.varc,"v","njs")])
      aux.post <- setNames(aux.post,c("v.k","variable",
                                      xnom.varc,"mu0","media",ynom.varc,"v","njs"))
      
      param.post<-rbind(aux.post,param.post)
      
    }
    
  }
  
  return(param.post)
  
}

