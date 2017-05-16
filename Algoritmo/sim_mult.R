sim_mult <- function(datos, nom.var = names(datos), nom.vard,nom.varc, componente = 2,
                            folio = NULL, n=dim(datos)[1],a=1, t=1,iteraciones = 1000){
  
  # a es el hiperparámetro de forma para la distribución previa de lambda
  # t es el valor para escalar el parámetro lambda de la variable Electronico
  k <- componente #numero de componentes del modelo
  #pi<-1/k
  
  
  pis<-pi_init(k) #Calcula la distribución previa dirichlet(alphaj=1/k) para el parámetro pi

  
  param <- dist_inicial_multi(datos, nom.varc, k) ##devuelve los par?metros iniciales de variables continuas
  lambda<-dist_init_discreta(datos,nom.vard,k,a=a) ##devuelve los par?metros iniciales de variables discretas
  ##variables continuas
  cs<-datos[nom.varc]
  ##numero de variables continuas
  d<-dim(cs)[2]
  ##variables discretas
  ds<-datos[nom.vard]
  ##numero de variables discretas
  p<-dim(ds)[2]
  ##col <- as.numeric(which(names(datos) %in% nom.var))##da las posiciones de las variables en la base de datos )
  est <- estadisticos_iniciales(datos) ##media y varianza de los datos 
  
  po<-NULL
  nor<-NULL
  phis<-NULL
  sims_vlat <- NULL
  sims_total <- NULL
  evaluadas<-NULL
  evs<-NULL
  
  mu0 <- est[which(est$variable == nom.varc), "media"] #la media obtenida de los datos
  #var0 <- data.frame(est[which(est$variable == nom.var), "varianza"]) #la varianza obtenida de los datos
  
  
  for(j in 1:iteraciones){

  ## se calcula la variable latente para cada una de las Xi's
      for( i in 1:n){
      
      po<-NULL
      nor<-NULL
      phis<-NULL
      
      ##por componente
      for(l in 1:k){
        #se obtienen los valores de las fns de densidad poisson  
        #con los parámetros calcualdos con las distribuciones previas 
        #por cada  xi discreta
        aux.po<-vdiscreta(datos,lambda,l,i,nom.vard, t=t)
        po<-rbind(aux.po,po)
        #se obtienen los valores de la fn de densidad normal multivariada  
        #con los parámetros calcualdos con las distribuciones previas 
        #por cada  xi continua
        aux.nor<-vcontinua(datos,param,l,i,nom.varc)
        nor<-rbind(aux.nor,nor)
        
        ##Se incorpora la previa de la variable latente (pis) con las densidades
        ##de la parte ontinua y discreta
        aux.phi<-data.frame(v.k=l,phi=pis$pi[l]*aux.nor$nor[1]*prod(aux.po$po))
        
        phis<-rbind(aux.phi,phis)
        
        evs<-data.frame(nor$nor,po$po) 
        
        evaluadas<-rbind(evs,evaluadas)
       }
      
      #simulacion de la variable latente para cada observacion i en la iteración j
      aux<-var_latente_multi(datos,nom.varc = nom.varc,nom.vard=nom.vard,phis,folio = 'Cliente',
                             j,i,k)
      print(aux)
      sims_vlat <- rbind(aux, sims_vlat)
      print(i)
     
      }
  
  asigna<-sims_vlat%>%
    dplyr::filter(zij==1)%>%
    dplyr::select(id,v.k)
  
  ## Se obtienen los hiperparámetros y parámetros para las distribuciones posteriores
  zjs_barra<-en_componente_multi(sims_vlat)
  
  xjs_barra<-por_componente_multi(datos,zjs_barra,sims_vlat,asigna,nom.var,folio='Cliente')
  
  hparam.post<-post_param(datos,param,asigna,nom.varc,xjs_barra,k,d,folio='Cliente')
  
  hlambda.post<-post_lambda(xjs_barra,lambda, nom.vard)
  
  param.post<-posterior_sigma_multi(hparam.post,nom.varc,k)
  
  lambda.post<-posterior_lambda(hlambda.post)
  
  post.pis<-posterior_pi_multi(xjs_barra,pis,k)
  
  #actualización de los parámetros que pasan a ser las previas en t+1
  param<-param.post
  lambda<-lambda.post
  pis<-post.pis
  
  print(j) ## imprime el n?mero de iteraci?n que ha transcurrido
  gc()
  
  }
 return(sims_vlat)
}    


   
    