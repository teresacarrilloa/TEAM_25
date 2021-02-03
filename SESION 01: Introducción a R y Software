#___________________________________________________________________
#___________________________________________________________________
#POSTWORK SESION: 01 Introduccion a R y Software (Github, Tipos de Datos)
#
#TEAM:25
#INTEGRANTES:
# * Martha Teresa Carrillo Acosta
# * Ana Paula Machargo
# * Angelica Guadalupe Rivera Varela
# * Jose Estefanía Estrada Aguilar
#
#INSTRUCCIONES:
# 1.Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R, 
#  los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
#
# 2.Del data frame que resulta de importar los datos a R, extrae las columnas que contienen:
#  los números de goles anotados por los equipos que jugaron en casa (FTHG) 
#           y los goles anotados por los equipos que jugaron como visitante (FTAG)
#
# 3.Consulta cómo funciona la función table en R al ejecutar en la consola ?table
#
#Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
#  
# La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
# La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
# La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y 
# goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
#___________________________________________________________________
#___________________________________________________________________


#Fijar Directorio de trabajo
getwd() #Comando para ver el directorio en el que encontramos
setwd("D:/01 BEDU Santander/02 Programación y Estadísdtica con R/Postwork") #Coloca el directorio de trabajo correspondiente a la práctica
library(dplyr)

#1 Importar los datos de soccer
url.soccer<-"https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
df.soccer <- read.csv(url.soccer)
str(df.soccer)
View(df.soccer)
summary(df.soccer)

#2 Extraer FTHG y FTAG () NOTA: Descripciones en "https://github.com/woobe/footballytics/blob/master/data/notes.txt"
goles<-select(df.soccer,FTHG,FTAG)


#3 Consultar función table
?table
??table

#3.1 Probailidad de que FTHG anote "x" goles

goles#Columnas con los goles FTHG y FTAG
tbl.goles.fthg<-table(goles[1])#Obtencion de tabla de goles FTHG
tbl.fthg<-prop.table(x = tbl.goles.fthg, margin = NULL)#Obtencion de tabla de frecuencias
df.fthg<-as.data.frame(tbl.fthg)#Conversion a Dataframe de tabla de frecuencias
colnames(df.fthg)=c("goles","frecuencia")#Etiquetas de columnas
df.fthg.prob<-mutate(df.fthg,probabilidad_marginal=frecuencia*100)#Obtención de probabilidades marginales
print.data.frame(df.fthg.prob,digits=2)#Probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
#Falta agregar signo "%" y un print que arroje texto descriptivo y tabla

#3.2 Probailidad de que FTAG anote "y" goles

goles#Columnas con los goles FTHG y FTAG
tbl.goles.ftag<-table(goles[2])#Obtencion de tabla de goles FTAG
tbl.ftag<-prop.table(x = tbl.goles.ftag, margin = NULL)#Obtencion de tabla de frecuencias FTAG
df.ftag<-as.data.frame(tbl.ftag)#Conversion a Dataframe de tabla de frecuencias
colnames(df.ftag)=c("goles","frecuencia")#Etiquetas de columnas
df.ftag.prob<-mutate(df.ftag,probabilidad_marginal=frecuencia*100)#Obtención de probabilidades marginales
print.data.frame(df.ftag.prob,digits=2)#Probabilidad (marginal) de que el equipo que juega en casa anote y goles (x = 0, 1, 2, ...)
#Falta agregar signo "%" y un print que arroje texto descriptivo y tabla

#3.3 Probailidad de que FTHG anote "x" goles y FTAG anote "y" goles

goles#Columnas con los goles FTHG y FTAG
tbl.goles<-table(goles)#Obtencion de tabla de goles FTAG y FTHG
tbl.goles.freq<-prop.table(tbl.goles)#Obtencion de tabla de frecuencias FTAG y FTHG
df.tbl.goles.freq<-as.data.frame(tbl.goles.freq)#Conversion a Dataframe de tabla de frecuencias
colnames(df.tbl.goles.freq)=c("goles_FTHG","goles_FTAG","frecuencia")#Etiquetas de columnas
df.goles.prob<-mutate(df.tbl.goles.freq,probabilidad_conjunta=frecuencia*100)#Obtención de probabilidades marginales
print.data.frame(df.goles.prob,digits=2)#probabilidad (conjunta) de que el FTHG "x" goles y el FTAG "y" goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
#Falta agregar signo "%" y un print que arroje texto descriptivo y tabla
