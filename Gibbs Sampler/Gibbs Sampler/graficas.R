rm(list=ls())

require("repmis")
require("tidyverse") 
require("MCMCpack")  
require("LCA") 
require("readr")
require("dplyr")
require("psych")
require('lubridate')
require('reshape2')
require('Hmisc')
require('GGally')

source('simulaciones.R')
source('dist_inciales.R')
source('estadisticos_iniciales.R')
source('var_latente.R')
source('en_componente.R')
source('por_componente.R')
source('posterior_sigma.R')
source('posterior_mu.R')
source('posterior_pi.R')


##gráficass
ggplot(data=porvenir) +
  geom_bar(mapping = aes(x = Creditos))

porvenir%>%
  count(Creditos)

ggplot(data=porvenir) +
  geom_histogram(mapping = aes(x = Cred_trad), binwidth = 2)


ggplot(porvenir, aes(Creditos)) + geom_bar() + facet_grid(~Sexo)+
  scale_x_continuous(limits = c(0, 50))
 

ggplot(data=porvenir) +
  geom_bar(mapping = aes(x = Creditos))

porvenir%>%
  count(Creditos)

ggplot(data=porvenir) +
  geom_histogram(mapping = aes(x = Ing_tot), binwidth = 2)


ggplot(data = porvenir) + 
  geom_jitter(mapping = aes(x = Ing_tot, y = Creditos, shape=Sexo,color=Suc))+
  scale_x_continuous(limits = c(0, 1100000))


ggplot(data = porvenir) + 
  geom_point(mapping = aes(x = Creditos<100, y = Monto_prom<100000, color = Sexo))

ggplot(data = porvenir) + 
  geom_point(mapping = aes(x = Saldo, y = Ing_tot, shape = Sexo, color=Suc), position = "jitter")+
  geom_smooth(mapping = aes(x = Saldo, y = Ing_tot))
#scale_x_continuous(limits = c(0, 250000))+
#scale_y_continuous(limits = c(0, 1000000))


ggplot(data = porvenir) + 
  geom_point(mapping = aes(x = Ctedesde, y = Creditos, color=Suc))

ggplot(data = porvenir) + 
  geom_point(mapping = aes(x = Cred_perd, y = Creditos, shape=Sexo, size=Ing_tot, color=Suc))

ggplot(data = porvenir) + 
  geom_point(mapping = aes(x = Creditos, y = Monto_prom, color = Edad))

ggplot(data = porvenir) + 
  geom_point(mapping = aes(x = Ing_tot, y = Creditos, color=Sexo), position = "jitter")+
  geom_smooth(mapping = aes(x = Ing_tot, y = Creditos))+
  scale_x_continuous(limits = c(0, 100000))+
  scale_y_continuous(limits = c(0, 100))

#facet_wrap(~ Suc, nrow = 2)

ggplot(data = porvenir) + 
  geom_point(mapping = aes(x = Ing_tot, y = Creditos))+
  geom_smooth(mapping = aes(x = Ing_tot, y = Creditos))

ggplot(data = porvenir, mapping = aes(x = Ing_tot, y = Creditos)) + 
  geom_point(mapping = aes(color = Suc)) + 
  geom_smooth()

Ing_sin_atipico<-porvenir%>%
  dplyr::filter(Ing_tot<10000000)


Atip_ing<-porvenir%>%
  dplyr::filter(Ing_tot>1100000)


ggcorr(Ing_sin_atipico, nbreaks=7, label=TRUE, label_alpha = TRUE, label_size=3, layout.exp = 5,
       hjust = 1)



ggplot(data = porvenir1) + 
  stat_summary(
    mapping = aes(x = Suc, y = Edad),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = data.clientes) + 
  geom_boxplot(mapping = aes(x = Suc, y = Edad))



ggplot(data=porvenir)+
  geom_point(mapping = aes(x=Cliente, y = Ing_tot<10000000))


