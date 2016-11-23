## cargar paquetes y funciones
source("funciones.R") 

data.clientes <- read.csv('Clientes_17092105.csv',
                  stringsAsFactors = FALSE)
head(data.clientes)
glimpse(data.clientes)


hiper.param <- estadisticos_iniciales(data.clientes)

param.iniciales <- dist_inciales(data.clientes, nom.var = "Monto_prom", componente = 2)


prueba_univar <- simulaciones(datos = data.clientes, nom.var = 'Creditos', componente = 2,
                    folio = 'Cliente', iteraciones = 10)

prueba_univar %>%
  dplyr::select(sim, componente, pi) %>% 
  ggplot(aes(x = sim, y = pi, group = as.factor(componente), colour = as.factor(componente))) +
    geom_point() +
    geom_line()
