
##calcula la posterior de sigma
posterior_sigma<-function(auxj,aux.media,param,mu0){
sigma.sim <- auxj %>% ##se calcula la posterior de sigma
  left_join(aux.media, by = "componente") %>%
  left_join(param, by = "componente") %>%
  rowwise() %>%
  mutate(sigma_sim = rinvgamma(1, (vj + prop)/2,
                              0.5*( sj^2 + diff_cuad + (nj*prop*(med_tot - mu0)^2)/(nj + prop)))) %>%
  data.frame()
 return(sigma.sim)
}

##----------prueba----------------------------

post_sig<-posterior_sigma(auxj=intento2,aux.media=est_componente, param=param.iniciales,mu0=2.887114)
