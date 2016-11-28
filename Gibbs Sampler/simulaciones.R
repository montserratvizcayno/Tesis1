?wdri <- function(datos, nom.var = names(datos), componente = 2,
                         folio = NULL, iteraciones = 1000){
  
  
  k <- componente
  param <- dist_inciales(datos, nom.var, k) ##devuelve los par?metros iniciales
  sims <- NULL
  sims_total <- NULL
  col <- as.numeric(which(names(datos) %in% nom.var))
  est <- estadisticos_iniciales(datos)
  mu0 <- as.numeric(est[which(est$variable == nom.var), "media"]) #la media obtenida de los datos
  
  n <- dim(datos)[1]
  for(j in 1:iteraciones){
    
    ## primer for sirve para actualizar parámetros (mu y sigma) que se calculan con las distribuciones posteriores 
    print(j) ## imprime el n?mero de iteraci?n que ha transcurrido
    
    for( i in 1:n){
      
      ## se calcula la variable latente para cada una de las Xi's
      aux <- param %>%
        mutate(delta = pi*dnorm(datos[i, nom.var],mu_inicial,sqrt(sigma_inicial)),
               ### en las pruebas por alguna raz?n la delta que se define arriba se vuelve cero, as?
               ###que por el momento en caso de que eso ocurra le asigno .5 al par?metro p de la bernoulli
               p_bern = ifelse(sum(delta) != 0, delta/sum(delta), 0.5)) %>%
        # p_bern = delta/sum(delta)) %>% 
        rowwise() %>%
        mutate(sim_bern = rbernoulli(1, p_bern)*1,
               id = datos[i, folio],
               sim_interna = i,
               sim = j) %>%
        data.frame()
      
      sims <- rbind(aux, sims)
    }
    
    id_bern <- sims[which(sims$sim_bern == 1), c("id", "componente")]
    ## se hace un filtrado por componente y se saca el numero de xi´s que tiene cada uno
    ## y la media de cada uno
    auxj <- sims %>%
      filter(sim_bern == 1) %>%
      mutate(n_tot = n()) %>% 
      group_by(componente) %>%
      summarise(prop = n()) %>%
      ungroup()
    
    nom.media <- "media"
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
                diff_cuad = sum(diff)^2) ##esta es la suma por componente de (diferencia de xi-media del componente) ^2
    
    sigma.sim <- auxj %>% ##se calcula la posterior de sigma
      left_join(aux.media, by = "componente") %>%
      left_join(param, by = "componente") %>%
      rowwise() %>%
      mutate(sigma_sim = rinvgamma(1, (vj + prop)/2,
                                   0.5*( sj^2 + diff_cuad + (nj*prop*(med_tot - mu0)^2)/(nj + prop)))) %>%
      data.frame
    
    mu.sim <- sigma.sim %>%##se calcula la posterior de mu
      rowwise() %>% 
      mutate(mu_sim = rnorm(1, (nj*mu0 + prop*med_tot)/(nj + prop),
                            sigma_sim/(nj + prop))) %>%
      data.frame()
    
    pi.sim <- as.numeric(rdirichlet(1, mu.sim$prop/sum(mu.sim$prop)))##se calcula la nueva pi 
    
    param <- data.frame(componente = 1:k, ##se actualizan los valores de los parametros
                        mu_inicial = mu.sim$mu_sim,
                        sigma_inicial = mu.sim$sigma_sim,
                        pi = pi.sim,
                        nj = rep(1, k),
                        vj = rep(1, k),
                        sj = rep(1, k))
    
  }
  return(sims)
  
}