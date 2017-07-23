sim_mult <- function(datos, nom.var = names(datos), nom.vard,nom.varc, componente = 2,
                     folio = NULL, n=dim(datos)[1],a=1, t=1,iteraciones = 1000){
  
  # a es el hiperparámetro de forma para la distribución previa de lambda
  # t es el valor para escalar el parámetro lambda de la variable Electronico
  k <- componente #numero de componentes del modelo
  #pi<-1/k
  
  
  pis<-pi_init(k) #Calcula la distribución previa dirichlet(alphaj=1/k) para el parámetro pi
  
  pis0 <- pis
  
  param <- dist_inicial_multi(datos, nom.varc, k) ##devuelve los par?metros iniciales de variables continuas
  param0 <- param
  
  lambda<-dist_init_discreta(datos,nom.vard,k,a=a,t=t) ##devuelve los par?metros iniciales de variables discretas
  lambda0 <- lambda
  ##variables continuas
  cs<-datos[nom.varc]
  ##numero de variables continuas
  d<- ncol(cs)
  ##variables discretas
  ds<-datos[nom.vard]
  ##numero de variables discretas
  p<-ncol(ds)
  ##col <- as.numeric(which(names(datos) %in% nom.var))##da las posiciones de las variables en la base de datos )
  est <- estadisticos_iniciales(datos) ##media y varianza de los datos 
  
  
  mu0s <- matrix(rep(NA,(d+4)*k),ncol=d+4)
  colnames(mu0s)<-c("v.k","sim","sim_interna",nom.varc,"nor")
  l0s <- matrix(rep(NA,(p+4)*k),ncol=p+4)
  colnames(l0s)<-c("v.k","sim","sim_interna",nom.vard,"po")
  
  phis<-NULL
  sims_vlat <- NULL
  sims_total <- NULL
  
  
  ##mu0 <- est[which(est$variable == nom.varc), "media"] #la media obtenida de los datos
  
  
  for(j in 1:iteraciones){
    
    sims_vlat <- NULL
    ## se calcula la variable latente para cada una de las Xi's
    for( i in 1:n){
      
      #po<-NULL
      #nor<-NULL
      phis<-NULL
      
      ##por componente
      for(l in 1:k){
        #se obtienen los valores de las fns de densidad poisson  
        #con los parámetros calcualdos con las distribuciones previas 
        #por cada  xi discreta
        aux.po<-vdiscreta(datos,lambda,l,i,nom.vard,j)
        #print(aux.po)
        aux.l0s<- t(as.matrix(aux.po$lambda))
        l0s[l,] <- c(l,j,i,aux.l0s,prod(aux.po$po))
        #se obtienen los valores de la fn de densidad normal multivariada  
        #con los parámetros calcualdos con las distribuciones previas 
        #por cada  xi continua
        aux.nor<-vcontinua(datos,param,l,i,nom.varc)
        
        aux.mu0s <-t(as.matrix(aux.nor$mu0)) 
        mu0s[l,] <- c(l,j,i,aux.mu0s,aux.nor$nor[1])
        
        ##Se incorpora la previa de la variable latente (pis) con las densidades
        ##de la parte ontinua y discreta
        aux.phi<-data.frame(v.k=l,phi=pis$pi[l]*aux.nor$nor[1]*prod(aux.po$po))
        
        phis<-rbind(aux.phi,phis)
        
      }
      
      #simulacion de la variable latente para cada observacion i en la iteración j
      aux<-var_latente_multi(datos,nom.varc = nom.varc,nom.vard=nom.vard,phis,folio = 'Cliente',
                             j,i,k)
      #print(aux)
      
      p_aux <- merge(merge(aux,mu0s,id=c("sim","sim_interna","v.k")),
                     l0s,id=c("sim","sim_interna","v.k"))
      
      sims_vlat <- rbind(p_aux, sims_vlat)
     
     print("interna")   
     print(i)
    }
    
    sims <- filter(sims_vlat,sim==j)
    sims <- sims[c("v.k","phi","delta","zij","id","sim_interna","sim")]
    
    #asigna <- sims[c("id","v.k")]
    asigna <- data.frame(id=as.character.factor(sims$id),v.k=sims$v.k,stringsAsFactors=FALSE)
    sims_total<-rbind(sims_vlat, sims_total)
    
    ## Se obtienen los hiperparámetros y parámetros para las distribuciones posteriores
    zjs_barra<-en_componente_multi(sims,k) #devuelve el número de observaciones dentro de cada componente
    
    ##Se obtienen las medias dentro de cada componente
    xjs_barra<-por_componente_multi(datos,zjs_barra,sims,asigna,
                                    nom.var,folio='Cliente',k)
    
    ##se obtienen los nuevos hiperparámetros para las dist. posteriores continuas
    hparam.post<-post_param(datos,param0,asigna,nom.varc,xjs_barra,k,d,
                            folio='Cliente')
    
    ##se obitneen los nuevos hiperparámetros para las dist. posteriores discretas.
    hlambda.post<-post_lambda(xjs_barra,lambda0, nom.vard,t=t)
    
    ##se obtienen los parámetros para las dist. posterioes continuas
    param.post<-posterior_sigma_multi(hparam.post,nom.varc,xjs_barra,k)
    
    ##Se obtienen los parámetros para las dist. posterioes discretas
    lambda.post<-posterior_lambda(hlambda.post)
    
    #se obtienen los nuevos parámetros para la dist posterios de pij
    post.pis<-posterior_pi_multi(xjs_barra,pis0,k)
    
    #actualización de los parámetros que pasan a ser las previas en t+1
    param<-param.post
    lambda<-lambda.post
    pis<-post.pis
    
    print("sim")
    print(j)  ## imprime el n?mero de iteraci?n que ha transcurrido
    #gc()
    
  }
  return(sims_total)
}    



