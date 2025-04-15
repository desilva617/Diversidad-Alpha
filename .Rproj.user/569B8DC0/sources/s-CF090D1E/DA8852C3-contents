##############################################################
#Función para crear comunidades aleatorias que pueden ser    # 
#muestreadas bajo diferentes condiciones                     #
#Esta función permite modificar la riqueza y la estructura   #
#de la comunidad (la dominancia de especies)                                           #
##############################################################
library(readxl)

spp <- as.matrix(read_excel("Aves.xlsx"))
riq <- c(0.70, 0.85, 0.55)
dom <- matrix(c(.05,.3,.65,.05,.41,.54,.05,.26,.69),3,3)
prop <- matrix(c(.40,.35,.25,.24,.41,.35,.30,.56,.14),3,3)
pD <- c(16000, 7000, 10000)




Randcom <- function(pD, spp, riq, dom, prop){
  
  #pD = vector número de individuos a generar por cada comunidad
  #spp = listado de especies a aleatorizar, la cantidad de especies que 
  #      que contendrá la comunidad aleatorizada. Puede suministrarse una una 
  #      lista de especies o dejarla por defecto como sp1 a spx.
  #riq = La proporción de las especies que quiere que contenga
  #      las comunidadedes, tomada del total de especies definidas 
  #      en spp. Debe tener tantos elementos como comunidades a
  #      generar.
  #dom = matriz de la dominancia esperada, marcada por diferencias
  #      en la abundancia relativa (proporción de individuos) de 
  #      las especies raras, comunes y dominantes. Debe contener 
  #      tantas columnas como comunidades creadas.
  #prop = matriz con la proporción de especies que deberían ser 
  #       definidos como raras, comunes y dominantes. Debe contener 
  #       tantas columnas como comunidades creadas.
 

##Crear comunidades 

require(spatstat)
require(raster)
require(vegan) 

xrange=c(0, 500)
yrange=c(0, 500)
window<-owin(xrange, yrange)

# Build maps from random points ----
set.seed(25)
elev   <- density(rpoispp(lambda=0.5, win=window)) #
elev <- elev*2000
set.seed(3)
hum <- density(rpoispp(lambda=0.5, win=window))

##Ensamble of communities----

##Puntos aleatoreos
ppE_d <- list()
ppH_d <- list()

#Especies total por riqueza
sppRiq <- list()

##Especies divididas
sppE <- list()
sppH <- list()

#Estructura de la comunidad (dominancia + proporcion)
estE <- list()
estH <- list()

sppE_d <- list()
sppH_d <- list()
#Comunidades
Rcomu <- list()

for(i in 1:ncol(prop)){
  
  #Elegimos aleatoriamente unos puntos 
  #dentro de un espacio definido en una ventana
  
  ppE_d[[i]] <- rpoint(round(pD[i]*0.7,0), elev) 
  ppH_d[[i]] <- rpoint(round(pD[i]*0.3,0), hum)
  
  #Extraemos las especies aleatoreamente en función de la
  #riqueza esperada para cada comunidad
  
  ifelse(is.vector(spp), spp <- matrix(as.factor(spp),length(spp), 1),
                          spp <- as.matrix(as.factor(spp[,1])))
  
  sppRiq[[i]] <- as.matrix(sample(spp, round(nrow(spp)*riq[i],0)))
  
  sppE[[i]] <- sample(sppRiq[[i]][,1], 
                       round(nrow(sppRiq[[i]])*0.7,0))
  sppH[[i]] <- sample(sppRiq[[i]][,1], 
                       round(nrow(sppRiq[[i]])*0.3,0)) 
  

  #Definimos la estructura para cada comunidad en base de
  #la dominancia y la proporción de individuos
  
  estE[[i]] <- sample(c(rep(dom[1,i], length(sppE[[i]])*prop[1,i]), 
                  rep(dom[2,i], length(sppE[[i]])*prop[2,i]),
                  rep(dom[3,i], length(sppE[[i]])*prop[3,i])), 
                  length(sppE[[i]]),replace=T)
  
  estH[[i]] <- sample(c(rep(dom[1,i], length(sppH[[i]])*prop[1,i]), 
                        rep(dom[2,i], length(sppH[[i]])*prop[2,i]),
                        rep(dom[3,i], length(sppH[[i]])*prop[3,i])), 
                      length(sppH[[i]]),replace=T)

  #Seleccionamos las especies aleatoreamente y en base de la 
  #estructura propuesta.
  
  sppE_d[[i]] <- sample(sppE[[i]], pD[i]*0.7, 
                               replace=T, prob=estE[[i]])

  sppH_d[[i]] <- sample(sppH[[i]], pD[i]*0.3, 
                               replace=T, prob=estH[[i]])
  
  
  ##generamos un ppp con la ubicación aleatorea y las especies
  ##con el primer patron
  
  ComE_d <- ppp(x=ppE_d[[i]]$x,y=ppE_d[[i]]$y, marks= sppE_d[[i]], 
              window=window) 

  ComH_d <- ppp(x=ppH_d[[i]]$x,y=ppH_d[[i]]$y, marks= sppH_d[[i]], 
              window=window) 

Rcomu[[i]]<-superimpose(ComE_d,ComH_d)

}


return(Rcomu)
}

comEJ <- Randcom(pD, spp, riq, dom, prop)

rm(list = setdiff(ls(), "comEJ"))
install.packages("spatstat")


