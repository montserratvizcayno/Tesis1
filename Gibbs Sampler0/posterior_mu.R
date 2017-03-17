
##calcula la posterior del par?metro mu
posterior_mu<-function(sigma.sim,mu0){
mu.sim <- sigma.sim %>%
  rowwise() %>% 
  mutate(mu_sim = rnorm(1, (nj*mu0 + prop*med_tot)/(nj + prop),
                        sigma_sim/(nj + prop))) %>%
  data.frame()

return(mu.sim)
}


###-----------------prueba--------------------
#
#post_mu<-posterior_mu(sigma.sim=post_sig,mu0=2.887114)