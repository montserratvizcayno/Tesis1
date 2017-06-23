
##esta función obtiene el número de observaciones i que pertencen a cada componente
##ie la suma de las zij 
en_componente_multi<-function(sims){
sims_componente<-sims%>%
  mutate(ntot=n())%>%
  group_by(v.k)%>%
  summarise(zj = n()) %>%
  ungroup()
return(sims_componente)

}
