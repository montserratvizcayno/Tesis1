
##se obtienen los nuevos hiperparámetros para la parte continua

post_param<-function(datos,param0,asigna,nom.varc,xjs_barra,k,d,folio=NULL){
  
  diffs<-NULL
  
  aux.xjs<-xjs_barra[c("v.k","zj",nom.varc)]
  
  xjnom.varc<-paste0("xj_",nom.varc)
  difnom.varc<-paste0("dif_",nom.varc)
  mdifnom.varc<-paste0("difm_",nom.varc)
  ynom.varc<-paste0(nom.varc,".y")
  
  aux.media<-param0[c("v.k","variable","media")]
  
  aux.param<-distinct(param0[c("v.k","v","njs")])
  aux.param <- merge(aux.param,aux.xjs,id=v.k) 
  aux.vars <- melt(aux.param[5:(4+d)])
  aux.vars <- setNames(aux.vars,c("variable","xj"))
 
  aux.param <- data.frame(aux.param[1:4],aux.vars)
  aux.param <- merge(aux.param,aux.media,id=c("v.k","variable"))
  aux.param <- data.frame(aux.param,nzj=(aux.param$njs*aux.param$zj)/aux.param$njs
                          +aux.param$zj,dif_m=aux.param$xj-aux.param$media)
 
  
  xjs <- setNames(aux.xjs,c("v.k","zj",xjnom.varc))
  
  asigna <- setNames(asigna,c(folio, "v.k"))
  
  aux.a <- left_join(datos,asigna, by = folio)
  
  aux.asigna <- filter(aux.a,!is.na(v.k))
  aux.asigna <- merge(aux.asigna[c("v.k",nom.varc)],xjs,id=v.k)
  
  
  aux.diff<-aux.asigna[nom.varc]-aux.asigna[xjnom.varc]
  
  for(l in 1:k){
    
    indicadora<-filter(xjs_barra,v.k==l)
    
    indice<- nrow(indicadora)
    
    
    
    if(indice!=0){
      
      ##calcula el cuadrado de xi-xj_tilde  
      diffc <- data.frame(aux.asigna,aux.diff)
      diffc <- setNames(diffc,c(names(aux.asigna),difnom.varc))
      diffc <- filter(diffc,v.k==l)
      
      diff1<-data.frame(crossprod(as.matrix(diffc[difnom.varc])),vks=l)
      
      
      ##calcula el cuadrado de la diferencia de xj_tilde y la media 
      diffm<-filter(aux.param,v.k==l)
      diffm <- diffm[c("variable","dif_m")]
      diffm <- rename(diffm,c(variable="var"))
      diffm <- melt(diffm,id.vars="var")
      diffm <- dcast(diffm,variable~var)
      diffm <- setNames(diffm,c("var",mdifnom.varc))
      diffm <- diffm[c(mdifnom.varc)]  
      
      
      
      diff2<-data.frame(crossprod(as.matrix(diffm)),vk=l)
       
      
      ##Obtener las varianzas observadas para cada v.k
      aux.vars<-filter(param0,v.k==l)
      
      vars.y<-as.matrix(aux.vars[ynom.varc])
      
      
      aux.diffs<-cbind(diff1,diff2,vars.y)
      
      diffs<-rbind(diffs,aux.diffs)
      
    }
  }   
  

  diffs_tot<-bind_cols(diffs,aux.param)
  diffs_tot <- diffs_tot[c("v.k",names(aux.param),difnom.varc,mdifnom.varc,ynom.varc)]
  
  ##se obtiene la nueva varianza para la distribución posterior de sigma
  new.var<-diffs_tot[ynom.varc]+diffs_tot[difnom.varc]+
    (diffs_tot$nzj*diffs_tot[ynom.varc])
  
  new.param<-data.frame(aux.param[1:2],
                        v=aux.param$v+aux.param$zj,
                        njs=aux.param$njs+aux.param$zj,aux.param[5:6],
                        media=(aux.param$media*aux.param$njs
                               +aux.param$zj*aux.param$xj)/(aux.param$njs+aux.param$zj),
                        aux.param[ncol(aux.param)-1],new.var)

  return(new.param)
  
}



