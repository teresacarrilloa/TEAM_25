getwd()
setwd("D:/Archivos/Cursos/BEDU/II.Programación y Estadistica con R/Postworks/files2")
#A partir del conjunto de datos de soccer de la liga española de las temporadas 
#2017/2018, 2018/2019 y 2019/2020, crea el data frame SmallData, que contenga las columnas 
#date, home.team, home.score, away.team y away.score; esto lo puedes hacer con ayuda de la 
#función select del paquete dplyr. Luego crea un directorio de trabajo y con ayuda de la 
#función write.csv guarde el data frame como un archivo csv con nombre soccer.csv. 
library(dplyr)
SmallData<- lapply(dir(),read.csv)
SmallData
str(SmallData)
SmallData <- lapply(SmallData, select, Date, HomeTeam, FTHG, AwayTeam, FTAG )
SmallData
SmallData <- lapply(SmallData[1], mutate, Date = as.Date(Date, "%d/%m/%y"))
SmallData <- lapply(SmallData[2:3], mutate, Date = as.Date(Date, "%d/%m/%Y"))
SmallData <- do.call(rbind, SmallData)
SmallData<-rename(SmallData, home.team=HomeTeam,away.team=AwayTeam, date=Date, home.score=FTHG, away.score=FTAG)
SmallData

getwd()
setwd("D:/Archivos/Cursos/BEDU/II.Programación y Estadistica con R/Postworks/files5")

#Puedes colocar como argumento row.names = FALSE en write.csv. 
write.csv(SmallData,"soccer.csv",row.names = F)
#Con la función 
#create.fbRanks.dataframes del paquete fbRanks importa el archivo soccer.csv a R y al mismo 
#tiempo asignarlo a una variable llamada listasoccer. Se creará una lista con los elementos 
#scores y teams que son data frames listos para la función rank.teams. 

install.packages("fbRanks")
library(fbRanks)
getwd()
listasoccer <- create.fbRanks.dataframes("soccer.csv", na.remove = T)

#Asigna estos data 
#frames a variables llamadas anotaciones y equipos. Con ayuda de la función unique crea un 
#vector de fechas (fecha) que no se repitan y que correspondan a las fechas en las que se 
#jugaron partidos. 
anotaciones<-as.data.frame(listasoccer$scores)
equipos<-as.data.frame(listasoccer$teams)
fechas <- c(unique(listasoccer$scores$date))

fechas
#Crea una variable llamada n que contenga el número de fechas diferentes. 
n<-length(fechas)

#Posteriormente, con la función rank.teams y usando como argumentos los data frames 
#anotaciones y equipos, crea un ranking de equipos usando únicamente datos desde la 
#fecha inicial y hasta la penúltima fecha en la que se jugaron partidos, estas fechas 
#las deberá especificar en max.date y min.date. 
?rank.teams

#Guarda los resultados con el nombre ranking.

ranking <- rank.teams(anotaciones, equipos,min.date=fechas[1], max.date=fechas[n-1])
# Finalmente estima las probabilidades de los eventos, el equipo de casa gana, 
#el equipo visitante gana o el resultado es un empate para los partidos que se jugaron en 
#la última fecha del vector de fechas fecha. Esto lo puedes hacer con ayuda de la función 
#predict y usando como argumentos ranking y fecha[n] que deberá especificar en date.

prediccion<-predict(ranking, min.date=fechas[n], max.date = fechas[n])
