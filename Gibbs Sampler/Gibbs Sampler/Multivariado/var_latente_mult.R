
## esta funci?n calcula la variable latente para cada una de las Xi's y la asigna a un componente
var_latente_mult<-function(datos,nom.var = names(datos), k,
                  folio = NULL,param,est,j,i){

  m<-dim(param)[1]/k
  ##sigmas<-as.matrix(param[4:(3+m)])
  xs<-as.vector(datos[i, nom.var])
  aux0<-NULL

  #f<-0
 
  for(f in 1:k){
   aux <- param %>%
    dplyr::filter(componente==f)%>% 
    mutate(delta = pi[(f*m)]*dmnorm(xs,mu_inicial.value,as.matrix(aux[4:(3+m)])),
           ### en las pruebas por alguna raz?n la delta que se define arriba se vuelve cero, as?
           ###que por el momento en caso de que eso ocurra le asigno .5 al par?metro p de la bernoulli
           p_bern = ifelse(sum(delta) != 0, delta/sum(delta), 0.5)) %>%
          ## p_bern = delta/sum(delta)) %>% 
    rowwise() %>%
    mutate(sim_bern = rbernoulli(1, p_bern)*1,
           id = datos[i, folio],
           sim_interna = i,
           sim = j) %>%
    data.frame()
  
    aux0<-rbind(aux,aux0)
       # filter(sim_bern == 1)%>%
    #data.frame()
  }
    auxi<-aux0%>%
      filter(sim_bern == 1)%>%
      data.frame()
    
    return(auxi)
}


#### ----------------------------pruebas-------------------------------------------------------
#
#trial<-NULL
#trials<-NULL
#for(i in 1:15000){
#trial<-var_latente(datos=data.clientes,nom.vars ='Creditos', k=2,
#               folio = 'Cliente',param=param.iniciales,est=hiper.param,j=1,i=i) 
#
#trials<-rbind(trial,trials)
#print(i)
#}
