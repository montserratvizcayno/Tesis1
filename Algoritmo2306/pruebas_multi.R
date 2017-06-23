
setwd("/home/jmartineov/Downloads/Algoritmo")
#setwd("~/GitHub/Tesis1/Gibbs Sampler")

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

data.clientes1 <- read_csv("Muestra.csv",
                          col_names=TRUE, col_types=cols(Cliente=col_character()))


data.clientes <- read_csv("Clientes_v2017.csv",
                          col_names=TRUE, col_types=cols(Cliente=col_character()))

head(data.clientes[,c("Cred_perd","Electronico","Cred_PF")])

hist(data.clientes[,c("Electronico")],50)
hist(data.clientes[,c("Cred_perd")],50)
hist(data.clientes[,c("Cred_PF")],50)

summary(data.clientes[,c("Electronico")])
summary(data.clientes[,c("Cred_perd")])
summary(data.clientes[,c("Cred_PF")])

quantile(as.matrix(data.clientes[,c("Electronico")]),probs = seq(0.9,1,0.001))
quantile(as.matrix(data.clientes[,c("Cred_perd")]),probs = seq(0.9,1,0.001))
quantile(as.matrix(data.clientes[,c("Cred_PF")]),probs = seq(0.9,1,0.001))

##nueva base de datos
porvenir<-read_csv(file="Clientes_v2017.csv", col_names=TRUE,locale=locale(date_format = "%d%.%m%.%Y"),
                   col_types=cols(Cliente=col_character(), Sexo=col_factor(levels=c("F","M")), Suc=col_factor(levels=c(1,2,3,4,5,6,7,8,9,10)) , Ctedesde=col_date(),
                                  F_nacimiento=col_date(),  CP=col_character()))
str(porvenir)
head(porvenir)
glimpse(porvenir)

fecha_corte<-dmy(01112012)

##transformaci?n de datos:
###se crea una nueva variable de antiguedad del cliente

porvenir1<-porvenir%>%
  mutate(antig=difftime(fecha_corte,Ctedesde, units="auto"))
  

dias<-porvenir1%>%
  count(antig)




antig_n<-parse_integer(porvenir1$antig)

###se crea una variable de Sexo binaria (M=0,F=1)

attach(porvenir1)
porvenir1$Sexobin[ Sexo == "M"] <- 0
porvenir1$Sexobin[Sexo == "F"] <- 1
detach(porvenir1)

data.clientes<- data.frame(porvenir,antig_n, Sexobin=porvenir1$Sexobin)

variable.names(porvenir)

datos<-data.clientes%>%
  dplyr::select(Cliente, Cred_perd, Cred_PF, Electronico, Ing_tot, Monto_prom,Saldo)

nom.var<-c("Cred_perd","Cred_PF","Electronico","Ing_tot","Monto_prom","Saldo")

nom.varc<-c("Ing_tot","Monto_prom","Saldo")

nom.vard<-c("Cred_perd","Cred_PF", "Electronico")

continuas<-porvenir[,c("Cliente","Ing_tot" , "Monto_prom" ,"Saldo")]

discretas<-porvenir[,c("Cliente","Cred_PF","Cred_perd", "Electronico")]

##pruebas

s<-sim_mult(data.clientes1[1:1000,],nom.var=nom.var,nom.vard=nom.vard,nom.varc=nom.varc,
            componente=2,folio='Cliente',n=1000, a=1,t=1/100,iteraciones=5)

##prueba aislada 

#datos <- datos[125:405,c("Cliente","Cred_perd","Cred_PF","Electronico","Ing_tot","Monto_prom","Saldo")]
componente<-2
folio<-"Cliente"

a<-1
t<-1/100
iteraciones<-3
n <- nrow(datos)

j<-1
l<-1

k<-2

graficas

s%>%
  ggplot(aes(x= sim, y = delta)) +
  geom_line()  + facet_grid(~v.k)

s%>%
  ggplot(aes(x= sim, y =delta , colour = as.factor(v.k))) +
  geom_line()

s%>%
  ggplot(aes(x= nor, y =Monto_prom,colour = as.factor(v.k))) +
  geom_line()  + facet_grid(~v.k)


sf%>%
  ggplot(aes(nor,colour = as.factor(v.k)))+
  geom_density(kernel="gaussian")+  scale_y_continuous(breaks = NULL) + facet_grid(~v.k)


write.csv(ceros,file="ceros.csv")
write.csv(dceros,file="dceros.csv")
