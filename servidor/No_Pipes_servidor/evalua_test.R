#función para evaluar la predicción de la clasificación con base en las distribuciones encontradas.

evalua_test<-function(data_t,nmult,poisson,dirichlet, nom.var = names(data_t), nom.vard,nom.varc, componente = 2,
                      folio = NULL, n=dim(data_t)[1]){
  j<-2
  k<-componente
  phis<-NULL
  asignaciones<-NULL
  
  for(i in 1:n){
  phis<-NULL
      for(l in 1:k){
  
    #primero evaluamos las variables continuas de la observación
     aux.nmv<-vcontinua(data_t,nmult,l,i,nom.varc)
     
     #evaluamos las variables discretas de la observación
     aux.po<-vdiscreta(data_t,poisson,l,i,nom.vard,j)
     
     #las probabilidades de la distibución de la variable latente
     aux.phi<-data.frame(v.k=l,phi=dirichlet$pi[l]*aux.nmv$nor[1]*prod(aux.po$po))
  
     phis<-rbind(aux.phi,phis)
    }
  
  #simulacion de la variable latente para cada observacion i en la iteración j
  aux<-var_latente_multi(data_t,nom.varc = nom.varc,nom.vard=nom.vard,phis,folio = 'Cliente',
                           j,i,k)
  
  asignaciones<-rbind(aux,asignaciones)
  gc()
  }
  
  return(asignaciones)
}
