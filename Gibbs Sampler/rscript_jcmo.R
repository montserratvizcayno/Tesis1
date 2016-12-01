
#install.packages("repmis")
#install.packages("tidyverse")
#install.packages("MCMCpack")
#install.packages("LCA")

require("repmis")
require("tidyverse") 
require("MCMCpack")  
require("LCA") 

rm(list=ls())
setwd("C:/Users/jmartineov/Documents/GitHub/Tesis1/Gibbs Sampler/")

# source("https://raw.githubusercontent.com/montserratvizcayno/Gibbs Sampler/funciones.R")
source("C:/Users/jmartineov/Documents/GitHub/Tesis1/Gibbs Sampler/funciones.R")
#source("C:/Users/jmartineov/Documents/GitHub/Tesis1/Gibbs Sampler/simulaciones.R")
#source("C:/Users/jmartineov/Documents/GitHub/Tesis1/Gibbs Sampler/dist_inciales.R")
#source("C:/Users/jmartineov/Documents/GitHub/Tesis1/Gibbs Sampler/estadisticos_iniciales.R")

# Data
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



