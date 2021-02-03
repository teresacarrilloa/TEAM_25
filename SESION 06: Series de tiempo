#___________________________________________________________________
#___________________________________________________________________
#POSTWORK SESION: 06 Series de tiempo
#
#TEAM:25
#INTEGRANTES:
# * Martha Teresa Carrillo Acosta
# * Ana Paula Machargo Gordillo
# * Angelica Guadalupe Rivera Varela
# * Jose Estefanía Estrada Aguilar
#
#INSTRUCCIONES:
#Importa el conjunto de datos match.data.csv a R y realiza lo siguiente:
#1. Agrega una nueva columna sumagoles que contenga la suma de goles por partido.
#2. Obtén el promedio por mes de la suma de goles.
#3. Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.
#4. Grafica la serie de tiempo.
#___________________________________________________________________
#___________________________________________________________________

#Fijar Directorio de trabajo 
getwd() #Comando para ver el directorio en el que encontramos
setwd("D://Sesion 06/postwork") #Coloca el directorio de trabajo correspondiente a la práctica
#Cargar Librerias
library(dplyr)
library(TSA)
library(scales)
library(lubridate)
library(zoo)

#Importar los datos de soccer match.data.csv
url.soccer.s6<-"https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-06/Postwork/match.data.csv"
df.soccer.s6 <- read.csv(url.soccer.s6)

#Validamos tipos de dato en match.data.csv
str(df.soccer.s6)

#1. Suma de goles por partido y conversion de fechas a tipo date

df.soccer.goles<-mutate(df.soccer.s6,sumagoles=home.score+away.score,date = floor_date((as.Date(df.soccer.goles$date,"%Y-%m-%d")),"month"))


#Validamos creacion de columna
str(df.soccer.goles)

#2 Promedio Goles por mes

df.goles.mes<-as.data.frame(df.soccer.goles%>%
  group_by(date)%>%
  summarise(Promedio=mean(sumagoles)))


#3 Serie de Tiempo de Promedio hasta 2019-12
df.2019<-filter(df.goles.mes,date<=as.Date("2019-12-01","%Y-%m-%d"))

#Complementando con ceros los meses faltantes

total_meses=(as.yearmon(as.Date("2019-12-01","%Y-%m-%d")) - as.yearmon(min(df.soccer.goles$date)))*12

df.meses12<-data.frame(date=seq(min(df.soccer.goles$date), by="month" ,length=total_meses))

#left join con df.2019 para complementar los meses sin goles promedios
df.mes.promedio <- df.meses12 %>% left_join(df.2019)%>% mutate(Promedio=ifelse(is.na(Promedio),0,Promedio))
  
str(df.mes.promedio)


#Serie de tiempo
ts.goles<-ts(df.mes.promedio$Promedio,freq=12,start=c(2010,8))

#Consulta de caracteristicas de serie de tiempo
str(ts.goles)
class(ts.goles)


#4 Grafica
plot(ts.goles,type="o", ylab="Goles",xlab="Tiempo", main="Goles Promedio" )
