## cargar paquetes y funciones
##source("funciones.R") 

rm(list=ls())

require("repmis")
require("tidyverse") 
require("MCMCpack")  
require("LCA") 

source('simulaciones.R')
source('dist_inciales.R')
source('estadisticos_iniciales.R')
source('var_latente.R')
source('en_componente.R')
source('por_componente.R')
source('posterior_sigma.R')
source('posterior_mu.R')
source('posterior_pi.R')

data.clientes <- read.csv('Clientes_17092105.csv',
                  stringsAsFactors = FALSE)
head(data.clientes)
glimpse(data.clientes)


hiper.param <- estadisticos_iniciales(data.clientes)

param.iniciales <- dist_inciales(data.clientes, nom.var = "Creditos", componente = 2)


prueba_univar <- simulaciones(datos = data.clientes, nom.var = 'Creditos', componente = 2,
                    folio = 'Cliente', iteraciones = 2)

prueba_univar %>%
  dplyr::select(sim, componente, pi) %>% 
  ggplot(aes(x = sim, y = pi, group = as.factor(componente), colour = as.factor(componente))) +
    geom_point() +
    geom_line()


prueba_univar4 <- simulaciones(datos = data.clientes, nom.var = 'Creditos', componente = 2,
                              folio = 'Cliente', iteraciones = 2)

filtrados <- prueba_univar[which(prueba_univar$sim_bern == 1), c("id", "componente")]
filtrados


prueba_univar4 %>%
  dplyr::select(sim, componente, pi) %>% 
  ggplot(aes(x = sim, y = pi, group = as.factor(componente), colour = as.factor(componente))) +
  geom_point() +
  geom_line()

prueba_univar4 %>%
  dplyr::select(sim, componente, mu_inicial) %>% 
  ggplot(aes(x = sim, y = mu_inicial, group = as.factor(componente), colour = as.factor(componente))) +
  geom_point() +
  geom_line()

prueba_univar %>%
  dplyr::select(sim, componente, delta) %>% 
  ggplot(aes(x = sim, y = delta, group = as.factor(componente), colour = as.factor(componente))) +
  geom_point() +
  geom_line()

library(dplyr)
delta_cero <- filter(prueba_univar,delta == 0)
delta_cero




