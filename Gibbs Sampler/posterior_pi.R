
##calcula la posterior de pi
posterior_pi<-function(mu.sim){
pi.sim <- as.numeric(rdirichlet(1, mu.sim$prop/sum(mu.sim$prop)))
}


post_pi<-posterior_pi(post_mu)