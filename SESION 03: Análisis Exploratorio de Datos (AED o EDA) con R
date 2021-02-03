###############POSTWORK 3 ################################################################################################

'Ahora graficaremos probabilidades (estimadas) marginales y conjuntas para el número de goles que anotan en un partido el equipo de casa o el equipo visitante.'

'Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
  La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)
La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)'

#Realiza lo siguiente:
# 1. Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo de casa.

library(ggplot2)



###############POSTWORK 3 ################################################################################################

'Ahora graficaremos probabilidades (estimadas) marginales y conjuntas para el número de goles que anotan en un partido el equipo de casa o el equipo visitante.'

'Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
  La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)
La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)'

#Realiza lo siguiente:
# 1. Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo de casa.


Games <- length(BL[,1])


Tabla_BL_FTHG <- table(BL$FTHG)
BL_FTHG_marginal <- Tabla_BL_FTHG[1:length(Tabla_BL_FTHG)]/Games
locales <- data.frame(frecuencia = Tabla_BL_FTHG, probabilidad = BL_FTHG_marginal)
locales <- locales[, -c(3)]
locales <- rename(locales, goles = frecuencia.Var1, frecuencia = frecuencia.Freq, probabilidad = probabilidad.Freq)



ggplot(locales, aes(x= locales$goles , y= locales$probabilidad)) +
  geom_bar(stat="identity", color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+
  xlab('Núm de goles') +  
  ylab('Probabilidad marginal') +
  ggtitle("Probabilidad marginal de goles que anota el equipo de casa") 

# 2. Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo visitante.


Tabla_BL_FTAG <- table(BL$FTAG)
BL_FTAG_marginal <- Tabla_BL_FTAG[1:length(Tabla_BL_FTAG)]/Games
visitantes <- data.frame(frecuencia = Tabla_BL_FTAG, probabilidad = BL_FTAG_marginal)
visitantes <- visitantes[, -c(3)]
visitantes
visitantes <- rename(visitantes, goles = frecuencia.Var1, frecuencia = frecuencia.Freq, probabilidad = probabilidad.Freq)



ggplot(visitantes, aes(x= visitantes$goles , y= visitantes$probabilidad)) +
  geom_bar(stat="identity", color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+
  xlab('Núm de goles') +  
  ylab('Probabilidad marginal') +
  ggtitle("Probabilidad marginal de goles que anota el equipo visitante") 


#3. Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el equipo de casa y el equipo visitante en un partido.



T_conjunt <- unite(BL_df, Goles,c(1:2),  sep = " ", remove = TRUE)
Tabla_conjunta <- Tabla %>%group_by(Goles) %>% summarise(frequency = n())



require(utils)

loc <- locales$goles
vis <- visitantes$goles
conjunt <- expand.grid(loc, vis)
conjunt[, 3] = rep(0, length(conjunt[, 1]))
conjunt <- rename(conjunt, goles_local = Var1, goles_visita = Var2, frecuencia_conjunta = V3)

for(i in 1:length(BL[, 1])){
  for(j in 1:length(conjunt[, 1])){
    if((BL[i,4] == conjunt[j, 1]) & (BL[i,5] == conjunt[j, 2])){
      conjunt[j, 3] = conjunt[j, 3] + 1 
    }
  }
}

conjunt[,4] = conjunt$frecuencia_conjunta / Games
conjunt = rename(conjunt, probabilidad = V4)

ggplot(conjunt, aes(x = goles_local, y = goles_visita, fill = probabilidad)) + 
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "firebrick4") +
  ggtitle("Heatmap de la probabilidad conjunta") +
  xlab("Goles de locales") + 
  ylab("Goles de visitantes")
