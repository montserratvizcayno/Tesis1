
media<-s1[[1]]
media<-as.vector(media)
media

muu<-media$mu0
muu[1:3]
varianza<-as.matrix(media[1:3,3:5])
mmult1<-rmvnorm(n=100,mean=muu[1:3],sigma=varianza) 
mmult2<-rmvnorm(n=100,mean=muu[4:6],sigma=as.matrix(media[4:6,3:5]) )
mmult3<-rmvnorm(n=100,mean=muu[7:9],sigma=as.matrix(media[7:9,3:5]) )
mmult1<-data.frame(k=1,mmult1)
mmult2<-data.frame(k=2,mmult2)
mmult3<-data.frame(k=3,mmult3)
mmult_tot<-rbind(mmult1,mmult2,mmult3)

media<-data.frame(simulacionNMV)
 media
muu<-media$mu0
 muu[1:3]
varianza<-as.matrix(media[1:3,4:6])
mmult1<-rmvnorm(n=100,mean=muu[1:3],sigma=varianza) 
 mmult2<-rmvnorm(n=100,mean=muu[4:6],sigma=as.matrix(media[4:6,4:6]) )
 mmult3<-rmvnorm(n=100,mean=muu[7:9],sigma=as.matrix(media[7:9,4:6]) )
 mmult1<-data.frame(k=1,mmult1)
mmult2<-data.frame(k=2,mmult2)
 mmult3<-data.frame(k=3,mmult3)
 mmult_tot<-rbind(mmult1,mmult2, mmult3)
 
 legend_title<-"componente"
  mmult_tot%>%
       ggplot(aes(X3, colour = as.factor(k), fill=as.factor(k)))+
       geom_density(kernel="gaussian", alpha=.2)+  scale_y_continuous(breaks = NULL) + 
       labs(fill=legend_title) + guides(colour = FALSE)
  
  require("rgl")


  mmult3.kde <- kde2d(mmult3[,2], mmult3[,4], n = 50)
  image(mmult3.kde)
  
  mmult2.kde <- kde2d(mmult2[,4], mmult2[,3], n = 50)
  image(mmult2.kde)
  
  mmult1.kde <- kde2d(mmult1[,2], mmult1[,4], n = 50)
  image(mmult1.kde)
  
  
  x <- mmult1.kde$x; y <- mmult1.kde$y; z <- mmult1.kde$z
  # Construct x,y,z coordinates
  xx <- rep(x,times=length(y))
  yy <- rep(y,each=length(x))
  zz <- z; dim(zz) <- NULL
  # Set up color range
  ra <- ceiling(16 * zz/max(zz))
  col <- rainbow(16, 2/3)
  # 3D interactive scatter plot
  scatterplot3ds(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")