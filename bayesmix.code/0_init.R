#
# El codigo fue probadcon con MRO 3.5.3
#

data.all <- read.csv("Clientes_v2017.csv", header = TRUE)

dim(data.all)

colnames(data.all)

class(data.all)

head(data.all)

data <- data.all[,c("Cliente", "Cred_perd", "Cred_PF", "Ing_tot", "Monto_prom","Saldo")]

disc.var <- c("Cred_perd","Cred_PF","Ing_tot","Monto_prom","Saldo")

cont.var <- c("Ing_tot","Monto_prom","Saldo")

mod.var <- c(disc.var,cont.var)



