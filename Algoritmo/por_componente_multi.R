
por_componente_multi<-function(datos,zjs_barra,sims_vlat,asigna,nom.var,folio=NULL){

aux.media <- datos %>%
  left_join(asigna %>%
              setNames(c(folio, "v.k")), by = folio) %>%
  filter(!is.na(v.k))%>%
  dplyr::select(v.k,one_of(nom.var)) %>% 
  group_by(v.k) %>%
  summarise_all(mean)

xjs_barra<-merge(zjs_barra,aux.media,id=v.k)

return(xjs_barra)

}
  
