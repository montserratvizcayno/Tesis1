## cargar paquetes y funciones
##source("funciones.R") 
setwd("~/GitHub/Tesis1/Gibbs Sampler")

rm(list=ls())

require("repmis")
require("tidyverse") 
require("MCMCpack")  
require("LCA") 
require("readr")
require("dplyr")
require("psych")
require('lubridate')
require('reshape')
require('reshape2')
require('Hmisc')
require('GGally')
require('mnormt')

source('simulaciones.R')
source('dist_inicial_mult.R')
source('estadisticos_iniciales.R')
source('var_latente_mult.R')
source('var_latente.R')
source('en_componente.R')
source('por_componente.R')
source('posterior_sigma.R')
source('posterior_mu.R')
source('posterior_pi.R')

data.clientes <- read.csv('Clientes_v2017.csv',
                  stringsAsFactors = FALSE)

##nueva base de datos
porvenir<-read_csv(file="Clientes_v2017.csv", col_names=TRUE,locale=locale(date_format = "%d%.%m%.%Y"),
                   col_types=cols(Cliente=col_character(), Sexo=col_factor(levels=c("F","M")), Suc=col_factor(levels=c(1,2,3,4,5,6,7,8,9,10)) , Ctedesde=col_date(),
                                  F_nacimiento=col_date(),  CP=col_character()))
str(porvenir)
head(porvenir)
glimpse(porvenir)

fecha_corte<-rep(dmy(01112012), dim(porvenir)[1])

##transformación de datos:
###se crea una nueva variable de antiguedad del cliente

porvenir1<-data.frame(porvenir,fecha_corte)%>%
  mutate(antig=round((fecha_corte-porvenir$Ctedesde)/30))

antig_n<-parse_integer(porvenir1$antig)

###se crea una variable de Sexo binaria (M=0,F=1)

attach(porvenir1)
porvenir1$Sexobin[ Sexo == "M"] <- 0
porvenir1$Sexobin[Sexo == "F"] <- 1
detach(porvenir1)

data.clientes<- data.frame(porvenir,antig_n, Sexobin=porvenir1$Sexobin)


datos<-data.clientes%>%
  dplyr::select(Cliente,Creditos,Ing_tot,Saldo)

nom.var<-c("Creditos", "Ing_tot" , "Saldo")


##Solo la parte numérica de la base para obtenenr correlaciones

numericos<-porvenir%>%
  dplyr::select_if(is.numeric)

head(porvenir)


###correlaciones


ggcorr(numericos, nbreaks=7, label=TRUE, label_alpha = TRUE, label_size=3, layout.exp = 5,
       hjust = 1)


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


