##se obtienen los parámetros para las distribuciones posteriores discretas.
posterior_lambda<-function(hlambda.post){
  
  p <- nrow(hlambda.post)
  
  aux.postlambda <- data.frame(hlambda.post[c("v.k","variable","media","varianza","a")],
                               lambda=NA)

    for(l in 1:p){
    aux.postlambda$lambda[l] <-  rgamma(1, shape=aux.postlambda$a[l],
                                        scale=aux.postlambda$media[l])
    }
  
  return(aux.postlambda)
  
}

