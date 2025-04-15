######################################
ventana <- function(pp,size,aleatorio=TRUE,x=1,y=1){
  
  if(aleatorio==TRUE){
    punto<-rpoint(1,win=c((pp$window$xrange)-c(-45,45),
                          (pp$window$yrange)-c(-45,45)))
    x<- punto$x
    y<- punto$y
     }
  
  if(aleatorio==FALSE){
    x <- x
    y <- y
  }
  
  s2<-size/2
  return(owin(c(x-s2, x+s2), c(y-s2, y+s2)))
}

sampleCom <- function(com,NM,EM, type,aleatorio=TRUE,x=1,y=1){
  #NM: numero de muestras
  #EM: esfuerzo de muestreo entre 1 y 8
  #type: se refiere al tipo de muestreo existen tres tipos
  #      redes: hace referencia a un muestreo con redes
  #      puntos: hace referencia a puntos de observación
  #      transecto: referencia a recorrido en transectos
  
  
  ##Crear comunidades 
  
  require(spatstat)
  require(raster)
  require(vegan) 
  
  xrange=c(0, 500)
  yrange=c(0, 500)
  window<-owin(xrange, yrange)
  
  # Build maps from random points ----
  elev   <- density(rpoispp(lambda=0.5, win=window)) #
  elev <- elev*2000
  if(aleatorio==TRUE){
    
      k <- NM
      unif.sample<- vector(mode = "list", length= k)
      
      for (i in 1:k){  
    unif.sample[[i]] <- com[ventana(com, EM*10, aleatorio = TRUE)]
      }
      par(mar=c(1,1,1,1))
    plot(elev, col=grey.colors(7))
    mtext(print(com), side=3, cex=1.2, line =0.5, font =2, adj=0)
    for (i in 1:k){
      plot(unif.sample[[i]]$window, add=T, lwd=1)
      }
      }

  
  if(aleatorio==FALSE){
    k <- NM
    unif.sample<- vector(mode = "list", length= k)
    
    for (i in 1:k){
      unif.sample[[i]]<-com[ventana(com, EM*10, aleatorio = FALSE,x,y)]
    } 
    
    plot(elev1)
    abline(h=seq(100,900, by=100), col="grey80")
    abline(v=seq(100,1400, by=100), col="grey80")
    points(x=x,y=y, pch=19, col="black")
    
    }
  
  sCom <- data.frame(Var1=unique(com$marks))
  
  for (i in 1:k) {
    msp <- data.frame(table(unif.sample[[i]]$marks))
    sCom <- merge(sCom, msp, by="Var1", all=T)
  }
  
  sCom <- t(sCom[,-1])
  rownames(sCom) <- 1:k
  colnames(sCom) <- unique(com$marks)
  
  sCom[is.na(sCom)] <- 0
  
  sCom <- sCom[,colSums(sCom)>0]
  
  #modificamos la eficiencia del muestreo cambiando detectabilidad
  #dependiendo del tipo de muestreo
 
  #Redes: menos 40% de riqueza y menos 50% de abundancia
  #P.focal: Puntos focales menos 10% de riqueza y menos 10% de abundancia
  #Trans: transectos datos brutos

  
  if(type=="redes"){
    #cambiamos la riqueza y abundancia
    
    R <- rep(ncol(sCom),nrow(sCom))
    
    for(i in 1:nrow(sCom)){
    #cambiamos riqueza
      riqR <- sample(1:R[i], round(R[i]*0.6, 0))
      sCom[i,-riqR] <- 0
    
    #cambiamos abundancia
    abunR <- c(rep(0, round(length(riqR)*0.3, 0)),
    sample(seq(0.4,0.6, by=0.05),round(length(riqR)*0.7, 0), replace=T))
    sCom[i,riqR] <- sCom[i,riqR]*abunR[1:length(riqR)]
    }
    sCom <- round(sCom,0)
    sCom <- sCom[,colSums(sCom)>0]  
  }
  
  if(type=="puntos"){
    #cambiamos la riqueza y abundancia
    
    R <- rep(ncol(sCom),nrow(sCom))
    
    for(i in 1:nrow(sCom)){
      #cambiamos riqueza
      riqR <- sample(1:R[i], round(R[i]*0.9, 0))
      sCom[i,-riqR] <- 0
      
      #cambiamos abundancia
      abunR <- c(rep(0, round(length(riqR)*0.3, 0)),
                sample(seq(0.90,0.95, by=0.01),round(length(riqR)*0.7, 0), replace=T))
     
      sCom[i,riqR] <- sCom[i,riqR]*abunR[1:length(riqR)]
    }
    sCom <- round(sCom,0)
    sCom <- sCom[,colSums(sCom)>0]  
  }
  if(type=="transecto"){
  sCom <- sCom
    }

  return(sCom)
}

