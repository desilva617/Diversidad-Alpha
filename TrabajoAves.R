#Ejecutamos el codigo para obtener las comunidades
source("Randcom.R")

##Cargamos la funcion muestreo.

source("sampleCom.R")

#La función sampleCom nos permite muestrear las comunidades.
#En el ejemplo el primer argumento de la función sampleCom es "com",
# este argumento representa a la comunidad a muestrearse
#en el ejemplo la comunidad comEJ[[i]], donde i puede ser 1, 2 o 3,
#que representan las tres comunidades. La comunidad 1 es un bosque maduro,
#la comunidad 2 es un bosque secundario, la comunidad 3 es una zona de cultivos mixtos
#El segundo argumento NM representa el número de muestras que vamos tomar, en este caso 10. El
#argumento EM se refiere al esfuerzo de muestreo que deberá ser 8 horas
#de observacion, finalmente, el argumento aleatorio es TRUE para elegir la ubicación
#de los puntos de muestreo aleatoreamente

comBMr <- sampleCom(com =comEJ[[1]],NM = 10, EM=8, type="redes",aleatorio = TRUE)
comBMr <- comBMr[,!is.na(colnames(comBMr))]

install.packages("spatstat")
