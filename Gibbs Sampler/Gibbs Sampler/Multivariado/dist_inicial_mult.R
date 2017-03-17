dist_incial_mult <- function(datos, nom.var = names(datos), componente = 2){
  
  
  est.init <- estadisticos_iniciales(datos) %>%
    data.frame()
  
  nums <- sapply(datos, is.numeric)
  d<-datos[ , nums]
  k <- componente
  l<-dim(d)[2]
  m<-k*l
  mu0<-NULL
  sigmas<-NULL
  ##es la variable guia de los componentes
  #v_k<-rep(1:k,each=l)
  
  ## hiperparÃ¡metros iniciales que en este caso toman valores de 1 y pi= 1 / #de componentes
  nj <- rep(1, times=l)
  vj <- rep(1, times=k, each=l)
  sj <- rep(1, times=k, each=l)
  pi <- rep(1/k, times=k)
  
  ## calcular gamma inversa (cuando se tiene una variable)
  aux.sigma0 <- data.frame(v = vj,
                           s = sj) %>%
    rowwise() %>% 
    mutate(sim = rinvgamma(1, v/2, s/2)) %>%
    data.frame
  
  sigma0 <- diag(aux.sigma0$sim)
  ##sigma0.aux1<-diag(sigma0[1:l])
  ##sigma0.aux2<-diag(sigma0[(l+1):(l*k)])
  ##sigma0<-matrix(c(sigma0.aux1,sigma0.aux2),nrow=(l*k),ncol=l, byrow=TRUE)
  
  ## calcular normal multivariada, porlo que ahora se introducirá un for, (buscare una forma mas efectiba después)
  for(f in 0:(k-1)){
  aux.mu0 <- data.frame(v_k=rep((f+1),l), xbarra=est.init[which(est.init$variable == nom.var),2],
                        sigma = sigma0[((f*l)+1):((f+1)*l),((f*l)+1):((f+1)*l)],
                        n = nj)%>%
    rename(c(media="xbarra"))%>%
    #rowwise()%>%
    mutate(sim = rmnorm(1, xbarra, sigma/n))%>%
    data.frame()
  
  mu0 <- matrix(c(aux.mu0$sim, mu0),nrow=l)
 sigmas<-matrix(c(sigma0[((f*l)+1):((f+1)*l),((f*l)+1):((f+1)*l)],sigmas),ncol=(l), byrow=TRUE)
  }
  
  mus<-melt(mu0)
  ## parÃ¡metros de las distribuciones inciales
  param.init <- data.frame(mu_inicial = mus,
                           sigma_inicial = sigmas,
                           pi = pi,
                           nj = nj,
                           vj = vj,
                           sj = sj)%>%
    rename(c(mu_inicial.X2="componente"))%>%
    rename(c(mu_inicial.X1="variables"))
  
  return(param.init)
  
}
