
post_param<-function(datos,param,asigna,nom.varc,xjs_barra,k,d,folio=NULL){
  
  diffs<-NULL
  
  aux.xjs<-xjs_barra%>%
    dplyr::select(one_of("v.k","zj",nom.varc))
    
  xjnom.varc<-paste0("xj_",nom.varc)
  difnom.varc<-paste0("dif_",nom.varc)
  mdifnom.varc<-paste0("difm_",nom.varc)
  ynom.varc<-paste0(nom.varc,".y")
  
  aux.media<-param%>%
    dplyr::select(v.k,variable,media)
 
   aux.param<-param%>%
    dplyr::select(one_of(c("v.k","v","njs")))%>%
    distinct()%>%
    merge(aux.xjs,id=v.k)%>%
    gather(key=variable, value=xj, one_of(nom.varc))%>%
    merge(aux.media,id=c(v.k,variable))%>%
    mutate(nzj=(njs*zj)/njs+zj,dif_m=xj-media) 
   

   xjs<-aux.xjs%>%
    setNames(c("v.k","zj",xjnom.varc))
   
   aux.asigna <- datos %>%
     left_join(asigna %>%
                 setNames(c(folio, "v.k")), by = folio) %>%
     filter(!is.na(v.k))%>%
     dplyr::select(v.k,one_of(nom.varc))%>%
     merge(xjs,id=v.k)%>%
     data.frame()
   
   aux.diff<-aux.asigna[nom.varc]-aux.asigna[xjnom.varc]
   
   for(l in 1:k){
     
     indicadora<-param%>%
       group_by(v.k)%>%
       summarise(ks=sum(v.k))%>%
       dplyr::filter(v.k==l)
     
  if(indicadora$ks!=0){
     
  ##calcula el cuadrado de xi-xj_tilde  
   diffc<-data.frame(aux.asigna,aux.diff)%>%
     setNames(c(names(aux.asigna),difnom.varc))%>%
     dplyr::filter(v.k==l)
   
   diff1<-crossprod(as.matrix(diffc[difnom.varc]))%>%
     data.frame()%>%
     mutate(vks=l)
   
   
   
   ##calcula el cuadrado de la diferencia de xj_tilde y la media 
   diffm<-aux.param%>%
     dplyr::filter(v.k==l)%>%
     dplyr::select(dif_m)%>%
     as.matrix()%>%
     t()
   
     colnames(diffm)<-mdifnom.varc
  
   
   diff2<-crossprod(diffm)%>%
     data.frame()%>%
     mutate(vk=l)
   
   ##Obtener las varianzas observadas para cada v.k
   aux.vars<-param%>%
     dplyr::filter(v.k==l)
   
   vars.y<-as.matrix(aux.vars[ynom.varc])
   
   
   aux.diffs<-cbind(diff1,diff2,vars.y)
   
   diffs<-rbind(diffs,aux.diffs)
   
     }
   }   
   
 diffs_tot<-diffs%>%
   bind_cols(aux.param)%>%
   dplyr::select(one_of("v.k",names(aux.param),difnom.varc,mdifnom.varc,ynom.varc))
   
 ##se obtiene la nueva varianza para la distribución posterior de sigma
 new.var<-diffs_tot[ynom.varc]+diffs_tot[difnom.varc]+(diffs_tot$nzj*diffs_tot[ynom.varc])
 
 new.param<-aux.param%>%
   mutate(media=(media*njs+zj*xj)/(njs+zj), v=v+zj,njs=njs+zj)%>%
   bind_cols(new.var)%>%
   dplyr::select(-dif_m)
 
 return(new.param)

}


