
## esta función calcula número de total de xi's que hay en cada componente en la iteración j (n_tot)


en_componente<-function(sims,id_bern){

auxj <-sims  %>%
  ##filter(sim_bern == 1) %>%  
  mutate(n_tot = n()) %>% 
  group_by(componente) %>%
  summarise(prop = n()) %>%
  ungroup()
return(auxj)
}


##---------prueba--------

id_berns <- trials[ c("id", "componente")]
intento2<-en_componente(trials,id_berns)
intento2



