###############POSTWORK 4 ################################################################################################

'Ahora investigarás la dependencia o independencia del número de goles anotados por el equipo de casa y el número de 
goles anotados por el equipo visitante mediante un procedimiento denominado bootstrap, revisa bibliografía en 
internet para que tengas nociones de este desarrollo.'

'1. Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles (x=0,1,... ,8), y el 
equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al dividir estas 
probabilidades conjuntas por el producto de las probabilidades marginales correspondientes.'

conjunt[, 5] = rep(0, length(conjunt[, 1]))
for(i in 1:length(conjunt$goles_local )){
  for(j in 1:length(locales$goles)){
    for(k in 1:length(visitantes$goles)){
      if((conjunt[i,1] == locales[j,1]) & (conjunt[i,2] == visitantes[k,1])){
        conjunt[i,5] = locales[j,3] * visitantes[k, 3]
      }
    }
  }
}
conjunto <- rename(conjunt, producto = V5)
conjunto <- conjunto %>% mutate(Cocientes = (conjunto$probabilidad/conjunto$producto) )

'2. Mediante un procedimiento de boostrap, obtén más cocientes similares a los obtenidos en la tabla del punto 
anterior. Esto para tener una idea de las distribuciones de la cual vienen los cocientes en la tabla anterior. 
Menciona en cuáles casos le parece razonable suponer que los cocientes de la tabla en el punto 1, son iguales a 
1 (en tal caso tendríamos independencia de las variables aleatorias X y Y).'

prom <- c()
for(i in 1:9999){
  set.seed(2*i)
  prom[i] = mean(sample(conjunto$Cocientes, length(conjunto$producto), replace = TRUE))
  
}

t.test(prom, alternative = "two.sided", mu = 1)
#La hipotesis nula va hacia la igualdad, lo que nos dice que la mu (media poblacional) es igual a 1 si se acepta.
#En esta ocasion la p  es de < 2.2e-16, menor a 0.05 por lo que rechazamos la hipotesis nula y aceptamos 
#la alternativa, la mu es diferente a 1. Lo que nos indica que la variable X y Y no son independientes. 
