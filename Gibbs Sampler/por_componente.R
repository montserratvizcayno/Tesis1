
##esta funcion calcula:
##la nueva media de las xi's de cada compentene en la iteración j (media)
##la diferencia de cuadrados (nom.var-media)^2

por_componente<-function(datos,sims,auxj,id_bern,nom.var,nom.media,folio=NULL){
  
  aux.media <- datos %>%
    left_join(id_bern %>%
                setNames(c(folio, "componente")), by = folio) %>%
    filter(!is.na(componente)) %>% 
    dplyr::select(componente,contains(nom.var)) %>% 
    group_by(componente) %>%
    mutate_(media = paste( "mean(" ,  as.name(nom.var)  ,")"  ),
            suma = paste( "sum(" ,  as.name(nom.var)  ,")"  ),
            diff = paste("(", as.name(nom.var)  ,"-",  as.name(nom.media), ")^2"  )) %>%
    summarise(med_tot = mean(media),
              suma_tot = mean(suma),
              diff_cuad = sum(diff)^2) 
}

##------------------------prueba-----------------------------------------------------
id_berns <- trials[which(trials$sim_bern == 1), c("id", "componente")]
est_componente<-por_componente(datos=data.clientes,sims=trials,auxj=intento2,id_bern=id_berns, 
                               nom.var='Creditos',nom.media='media',folio='Cliente')
trials

por_componente(datos=data.clientes,sims=trials,id_bern=id_berns, 
               nom.var='Creditos',nom.media='media',folio='Cliente')