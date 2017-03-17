
simulaciones <- function(datos, nom.var = names(datos), componente = 2,
                          folio = NULL, iteraciones = 1000){
  ##set.seed(9024)
  
  k <- componente
  param <- dist_inciales(datos, nom.var, k) ##devuelve los par?metros iniciales
  sims <- NULL
  sims_total <- NULL
  col <- as.numeric(which(names(datos) %in% nom.var))
  est <- estadisticos_iniciales(datos)
  mu0 <- as.numeric(est[which(est$variable == nom.var), "media"]) #la media obtenida de los datos
  #var0 <- as.numeric(est[which(est$variable == nom.var), "varianza"]) #la varianza obtenida de los datos
  
  n <- dim(datos)[1]
  for(j in 1:iteraciones){
    
    ## primer for sirve para actualizar parámetros (mu y sigma) que se calculan con las distribuciones posteriores 
    
    
    for( i in 1:n){
      
      ## se calcula la variable latente para cada una de las Xi's
      aux<-var_latente(data.clientes,nom.vars = nom.var,k=k,folio = 'Cliente',
                      param= param,est=est,j,i)
      
      sims <- rbind(aux, sims)
    }
    
    id_bern <- sims[ c("id", "componente")]
    ## se hace un filtrado por componente y se saca el numero de xi´s que tiene cada uno
    ## y la media de cada uno
    auxj<-en_componente(sims=sims,id_bern=id_bern)
    ##auxj <- sims %>%
    ##filter(sim_bern == 1) %>%
    ##mutate(n_tot = n()) %>% 
    ##group_by(componente) %>%
    ##summarise(prop = n()) %>%
    ##ungroup()
    
    nom.media <- "media"
    
    aux.media<-por_componente(data.clientes,sims=sims,auxj=auxj,id_bern=id_bern,nom.var=nom.var,
                              nom.media=nom.media,folio='Cliente')
    ##aux.media <- datos %>%
    ##left_join(id_bern %>%
    ## setNames(c(folio, "componente")), by = folio) %>%
    ##filter(!is.na(componente)) %>% 
    ##dplyr::select(componente,contains(nom.var)) %>% 
    ##group_by(componente) %>%
    ##mutate_(media = paste( "mean(" ,  as.name(nom.var)  ,")"  ),
    ##suma = paste( "sum(" ,  as.name(nom.var)  ,")"  ),
    ##diff = paste("(", as.name(nom.var)  ,"-",  as.name(nom.media), ")^2"  )) %>%
    ##summarise(med_tot = mean(media),
    ##suma_tot = mean(suma),
    ##diff_cuad = sum(diff)^2) ##esta es la suma por componente de (diferencia de xi-media del componente) ^2
    sigma.sim<-posterior_sigma(auxj=auxj,aux.media=aux.media,param=param,mu0=mu0) ##se calcula la posterior de sigma
    ##sigma.sim <- auxj %>% 
    ##left_join(aux.media, by = "componente") %>%
    ##left_join(param, by = "componente") %>%
    ##rowwise() %>%
    ##mutate(sigma_sim = rinvgamma(1, (vj + prop)/2,
    ## 0.5*( sj^2 + diff_cuad + (nj*prop*(med_tot - mu0)^2)/(nj + prop)))) %>%
    ## data.frame
    
    mu.sim<-posterior_mu(sigma.sim=sigma.sim,mu0=mu0)
    ##mu.sim <- sigma.sim %>%##se calcula la posterior de mu
    ##rowwise() %>% 
    ##mutate(mu_sim = rnorm(1, (nj*mu0 + prop*med_tot)/(nj + prop),
    ##sigma_sim/(nj + prop))) %>%
    ##data.frame()
    pi.sim<-posterior_pi(mu.sim=mu.sim)
    #pi.sim <- as.numeric(rdirichlet(1, mu.sim$prop/sum(mu.sim$prop)))##se calcula la nueva pi 
    
    param <- data.frame(componente = 1:k, ##se actualizan los valores de los parametros
                        mu_inicial = mu.sim$mu_sim,
                        sigma_inicial = mu.sim$sigma_sim,
                        pi = pi.sim,
                        nj = rep(1, k),
                        vj = rep(1, k),
                        sj = rep(1, k))
    
    print(j) ## imprime el n?mero de iteraci?n que ha transcurrido
    gc()
  }
  return(sims)
  
}

