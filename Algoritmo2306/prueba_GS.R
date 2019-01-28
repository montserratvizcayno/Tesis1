
rm(list=ls())


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
require('mvtnorm')
require('stats')
require('magrittr')


source('dist_inicial_multi.R')
source('estadisticos_iniciales.R')
source('dist_init_discreta.R')
source("estadisticos_iniciales_mult.R")
source('var_latente_multi.R')
source('en_componente_multi.R')
source('por_componente_multi.R')
source('sim_mult.R')
source('vcontinua.R')
source('vdiscreta.R')
source('post_lambda.R')
source('post_param.R')
source('posterior_lambda.R')
source('pi_init.R')
source('posterior_pi_multi.R')
source('posterior_sigma_multi.R')



data.clientes <- read_csv('Clientes_v2017.csv',
                          col_names=TRUE, col_types=cols(Cliente=col_character()))

##nueva base de datos
porvenir<-read_csv(file="Clientes_v2017.csv", col_names=TRUE,locale=locale(date_format = "%d%.%m%.%Y"),
                   col_types=cols(Cliente=col_character(), Sexo=col_factor(levels=c("F","M")), Suc=col_factor(levels=c(1,2,3,4,5,6,7,8,9,10)) , Ctedesde=col_date(),
                                  F_nacimiento=col_date(),  CP=col_character()))

##seleccion de variables
datos<-data.clientes%>%
  dplyr::select(Cliente, Cred_perd, Cred_PF, Ing_tot, Monto_prom,Saldo)


nom.var<-c("Cred_perd","Cred_PF","Ing_tot","Monto_prom","Saldo")

nom.varc<-c("Ing_tot","Monto_prom","Saldo")

nom.vard<-c("Cred_perd","Cred_PF")

##pruebas

s<-sim_mult(datos,nom.var=nom.var,nom.vard=nom.vard,nom.varc=nom.varc,
            componente=2,folio='Cliente',n=15, a=1,t=1/1000,iteraciones=10)

##prueba aislada manual

componente<-2
folio<-"Cliente"
n<-40
a<-1
t<-1/100
iteraciones<-3

j<-1
l<-1

##graficas

##graficas

s%>%
  ggplot(aes(x= sim, y = phi)) +
  geom_line()  + facet_grid(~v.k)

s%>%
  ggplot(aes(x= sim, y =delta , colour = as.factor(v.k))) +
  geom_line()


s%>%
  ggplot(aes(x= nor, y =Monto_prom,colour = as.factor(v.k))) +
  geom_line()  + facet_grid(~v.k)


s%>%
  ggplot(aes(nor,colour = as.factor(v.k)))+
  geom_density(kernel="gaussian")+  scale_y_continuous(breaks = NULL) + facet_grid(~v.k)



