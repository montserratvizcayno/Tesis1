#
#   Tesis: Montserrat
#   -- 2017-mar-03 --
#   -- 2016-dic-13 --
#   

rm(list=ls())

#install.packages("repmis")
#install.packages("tidyverse")
#install.packages("MCMCpack")
#install.packages("LCA")
#install.packages("dplyr")
#install.packages("tidyr")

# require("repmis")
require("tidyverse") 
require("MCMCpack")  
require("LCA") 
require("dplyr")
require("tidyr")

#setwd("C:/JCMO.Trabajo/@Estudiantes/Montserrat Vizcayno/Tesis1/Gibbs Sampler/") # Windows
setwd("/run/media/jmartineov/JC.ITAM/JCMO.Trabajo/@Estudiantes/Montserrat Vizcayno/Tesis1/Gibbs Sampler/") # Linux

# Funciones
source("dist_inciales.R")
source("en_componente.R")
source("por_componente.R")
source("posterior_mu.R")
source("posterior_pi.R")
source("posterior_sigma.R")
source("var_latente.R")
source("estadisticos_iniciales.R")
source("simulaciones.R")

# Data
data.clientes <- read.csv('Clientes_17092105.csv', 
                          stringsAsFactors = FALSE)
head(data.clientes)

glimpse(data.clientes)

hiper.param <- estadisticos_iniciales(data.clientes)

param.iniciales <- dist_inciales(data.clientes, nom.var = "Creditos", componente = 2)

# Simulaciones (Gibbs sampler)
datos <- data.clientes[,c("Saldo","Monto_prom", "Certificado")]
head(datos)
summary(datos)
nom.var <-  names(datos)  # nombre de las covariables
componente <- 2  # número de componentes
folio <- NULL
iteraciones <- 10  # Longitud de la cadena de Markov
set.seed(9024)
simulaciones(datos, nom.var, componente, folio, iteraciones)



