## cargar paquetes y funciones
source("funciones.R") 

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


prueba_univar4 <- simulaciones2(datos = data.clientes, nom.var = 'Creditos', componente = 2,
                              folio = 'Cliente', iteraciones = 20)

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

prueba_univar4 %>%
  dplyr::select(sim, componente, delta) %>% 
  ggplot(aes(x = sim, y = delta, group = as.factor(componente), colour = as.factor(componente))) +
  geom_point() +
  geom_line()

library(dplyr)
delta_cero <- filter(prueba_univar4,delta == 0)
delta_cero




