
en_componente_multi<-function(sims_vlat){
sims<-sims_vlat%>%
  mutate(ntot=n())%>%
  group_by(v.k)%>%
  summarise(zj = n()) %>%
  ungroup()
return(sims)

}
