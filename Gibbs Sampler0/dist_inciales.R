## esta funcion calcula las distribuciones previas, 
## toma como valor por default que sean dos componentes
## en la funcion se pueden tomar 1 o mas variables, 
## en el ejemplo solo ocupo 1
dist_inciales <- function(datos, nom.var = names(datos), componente = 2){
  
  
  est.init <- estadisticos_iniciales(datos) %>%
    data.frame()
  
  k <- componente
  
  ## hiperparámetros iniciales que en este caso toman valores de 1 y pi= 1 / #de componentes
  nj <- rep(1, k)
  vj <- rep(1, k)
  sj <- rep(1, k)
  pi <- rep(1/k, k)
  
  ## calcular gamma inversa (cuando se tiene una variable)
  aux.sigma0 <- data.frame(v = vj,
                           s = sj) %>%
    rowwise() %>% 
    mutate(sim = rinvgamma(1, v/2, s/2)) %>%
    data.frame
  
  sigma0 <- aux.sigma0$sim
  
  ## calcular normal (cuando se tiene una variable)
  aux.mu0 <- data.frame(x_barra = rep(est.init[which(est.init$variable == nom.var),2], k),
                        sigma = sigma0,
                        n = nj) %>%
    rowwise() %>%
    mutate(sim = rnorm(1, x_barra, sigma/n)) %>%
    data.frame()
  
  mu0 <- aux.mu0$sim
  
  ## parámetros de las distribuciones inciales
  param.init <- data.frame(componente = 1:k,
                           mu_inicial = mu0,
                           sigma_inicial = sigma0,
                           pi = pi,
                           nj = nj,
                           vj = vj,
                           sj = sj)
  
  return(param.init)
  
}