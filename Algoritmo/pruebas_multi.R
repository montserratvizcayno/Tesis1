## cargar paquetes y funciones
##source("funciones.R") 
#setwd("~/GitHub/Tesis1/Gibbs Sampler")

rm(list=ls())

install.packages(c("broom", "dplyr", "forcats", "ggplot2", "haven", "hms", "jsonlite", "lubridate", "magrittr", "modelr", 
                   "purrr", "readr", "readxl", "stringr", "tibble", "rvest", "tidyr", "xml2", "knitr", "rmarkdown"))
install.packages("MCMCpack")  
install.packages("LCA") 
install.packages("readr")
install.packages("dplyr")
install.packages("psych")
install.packages('lubridate')
install.packages('reshape')
install.packages('reshape2')
install.packages('Hmisc')
install.packages('GGally')
install.packages('mvtnorm')
install.packages('magrittr')

#require("repmis")
require(c("broom", "dplyr", "forcats", "ggplot2", "haven", "hms", "jsonlite", "lubridate", "magrittr", "modelr", 
          "purrr", "readr", "readxl", "stringr", "tibble", "rvest", "tidyr", "xml2", "knitr", "rmarkdown"))
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
source('var_latente.R')
source('en_componente.R')
source('por_componente.R')
source('posterior_sigma.R')
source('posterior_mu.R')
source('posterior_pi.R')
source('sims_mult.R')
source('vcontinua.R')
source('vdiscreta.R')


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

variable.names(porvenir)

datos<-data.clientes%>%
  dplyr::select(Cliente, Cred_perd, Cred_PF, Electronico, Ing_tot, Monto_prom,Saldo)

nom.var<-c("Cred_perd","Cred_PF","Electronico","Ing_tot","Monto_prom","Saldo")

nom.varc<-c("Ing_tot","Monto_prom","Saldo")

nom.vard<-c("Cred_perd","Cred_PF", "Electronico")

continuas<-porvenir[,c("Cliente","Ing_tot" , "Monto_prom" ,"Saldo")]

discretas<-porvenir[,c("Cliente","Cred_PF","Cred_perd", "Electronico")]

##pruebas

s<-sim_mult(datos,nom.var=nom.var,nom.vard=nom.vard,nom.varc=nom.varc,componente=2,folio='Cliente',n=5, a=6,t=1/100,iteraciones=10)




