
rm(list=ls())

# install.packages("tidyverse") 
# install.packages("MCMCpack")  
# install.packages("LCA") 
# install.packages("readr")
# install.packages("dplyr")
# install.packages("psych")
# install.packages("lubridate")
# install.packages("reshape")
# install.packages("reshape2")
# install.packages("Hmisc")
# install.packages("GGally")
# install.packages("mvtnorm")
# install.packages("stats")
# install.packages("magrittr")
# install.packages("base")
# install.packages("grid")

require("tidyverse") 
require("MCMCpack")  
require("LCA") 
require('readr')
require('dplyr')
require('psych')
require('lubridate')
require('reshape')
require('reshape2')
require('Hmisc')
require('GGally')
require('mvtnorm')
require('stats')
require('magrittr')
require('base')
require('grid')

#setwd("C:/Users/Montse/Downloads/Montse DELL Backup/GitHub1.1/Tesis1/Gibbs Sampler/Sampler/No pipes")


source("./No.Pipes/dist_inicial_multi.R")
source("./No.Pipes/estadisticos_iniciales.R")
source("./No.Pipes/dist_init_discreta.R")
source("./No.Pipes/estadisticos_iniciales_mult.R")
source("./No.Pipes/var_latente_multi.R")
source("./No.Pipes/en_componente_multi.R")
source("./No.Pipes/porcomponente_multi.R")
source("./No.Pipes/sim_mult.R")
source("./No.Pipes/vcontinua.R")
source("./No.Pipes/vdiscreta.R")
source("./No.Pipes/post_lambda.R")
source("./No.Pipes/post_param.R")
source("./No.Pipes/posterior_lambda.R")
source("./No.Pipes/pi_init.R")
source("./No.Pipes/posterior_pi_multi.R")
source("./No.Pipes/posterior_sigma_multi.R")


#data.clientes <- read_csv('./No.Pipes/Clientes_v2017.csv',
#                          col_names=TRUE, col_types=cols(Cliente=col_character()))

##nueva base de datos
porvenir <-read_csv(file="./No.Pipes/Clientes_v2017.csv", col_names=TRUE,locale=locale(date_format = "%d%.%m%.%Y"),
                   col_types=cols(Cliente=col_character(), Sexo=col_factor(levels=c("F","M")), Suc=col_factor(levels=c(1,2,3,4,5,6,7,8,9,10)) , Ctedesde=col_date(),
                                  F_nacimiento=col_date(),  CP=col_character()))

##seleccion de variables
datos <- porvenir[c("Cliente", "Cred_perd", "Cred_PF", "Ing_tot", "Monto_prom","Saldo")]

nom.var<-c("Cred_perd","Cred_PF","Ing_tot","Monto_prom","Saldo")

nom.varc<-c("Ing_tot","Monto_prom","Saldo")

nom.vard<-c("Cred_perd","Cred_PF")

##pruebas

s <- sim_mult(datos,nom.var=nom.var,nom.vard=nom.vard,nom.varc=nom.varc,
            componente=2,folio='Cliente', a=1,iteraciones=1000)

datos_asignados <- evalua_test(data_t=datos[20000:20100,],nmult=s[[1]],
                               poisson=s[[2]],dirichlet=s[[3]],
                               nom.var=nom.var,nom.vard=nom.vard,
                               nom.varc=nom.varc, componente = 2,
                               folio = 'Cliente')

##prueba aislada manual

componente<-2
folio<-"Cliente"
n<-1000
a<-1
t<-1/100
iteraciones<-3

j<-1
l<-1
i<-1
##graficas

##graficas
legend_title<-"componente"


resultado1%>%
  ggplot(aes(x= sim, y = delta, colour = as.factor(v.k))) +
  geom_point()  + facet_grid(~v.k) +   labs(colour=legend_title)

simulacion1%>%
  ggplot(aes(x= sims_vlat.sim, y = sims_vlat.delta, colour = as.factor(sims_vlat.v.k))) +
  geom_point()  + facet_grid(~sims_vlat.v.k)+   labs(colour=legend_title)



s[[4]]%>%
  ggplot(aes(x= sims_vlat.sim, y =sims_vlat.delta , colour = as.factor(sims_vlat.v.k))) +
  geom_point()

resultado1%>%
  ggplot(aes(x= sim, y =delta , colour = as.factor(v.k))) +
  geom_point()

simulacion1%>%
  ggplot(aes(x= sims_vlat.nor, y =Monto_prom,colour = as.factor(v.k))) +
  geom_line()  + facet_grid(~v.k)

legend_title<-"componente"


s%>%
  ggplot(aes(Monto_prom, colour = as.factor(v.k), fill=as.factor(v.k)))+
  geom_density(kernel="gaussian", alpha=.2)+  scale_y_continuous(breaks = NULL) + 
  labs(fill=legend_title) + guides(colour = FALSE)

s%>%
  ggplot(aes(Cred_perd,colour = as.factor(v.k), fill=as.factor(v.k)))+
  geom_histogram(position="stack") + 
  labs(fill=legend_title) + guides(colour = FALSE)##+ facet_grid(~v.k)

s%>%
  ggplot(aes(x=Cred_PF, colour = as.factor(v.k),fill=as.factor(v.k))) + geom_density(alpha=.3)

##resultados


resultado3 <- read_csv('Simulaciones_100_300obs_4comp.csv',
                          col_names=TRUE, col_types=cols(Cliente=col_character()))

resultado2 <- read_csv('Simulaciones_100_900obs_2comp.csv',
                       col_names=TRUE, col_types=cols(Cliente=col_character()))

resultado1 <- read_csv('Simulaciones_30_100obs_2comp.csv',
                       col_names=TRUE)

simulacion1<-read_csv('Simulaciones_200_100obs_3comp.csv',
                      col_names=TRUE)

simulacionNMV<-read_csv('NormalMultivariada_200_100obs_3comp.csv',
                      col_names=TRUE)
simulacionPois<-read_csv('Poisson_200_100obs_3comp.csv',
                        col_names=TRUE)
simulacionD<-read_csv('Dirichlet_200_100obs_3comp.csv',
                        col_names=TRUE)

write.csv(s1[[4]], file = "Simulaciones_200_100obs_3comp.csv")
write.csv(s1[[1]], file = "NormalMultivariada_200_100obs_3comp.csv")
write.csv(s1[[2]], file = "Poisson_200_100obs_3comp.csv")
write.csv(s1[[3]], file = "Dirichlet_200_100obs_3comp.csv")
write.csv(datos_asignados, file = "Asigna_100datos_con_150_2000obs_2comp.csv")