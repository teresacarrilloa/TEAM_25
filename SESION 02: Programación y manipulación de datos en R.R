getwd()
setwd("D:/Archivos/Cursos/BEDU/II.Programación y Estadistica con R/Postworks/files2") # Depende del usuario
dir()

#Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php


df17_20 <- lapply(dir(),read.csv)

#Obten una mejor idea de las características de los data frames al usar las funciones: str, head, View y summary

str(df17_20); head(df17_20); View(df17_20); summary(df17_20)

#Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data frames. (Hint: también puedes usar lapply).
library(dplyr)


#sel17_20 <- lapply(df17_20, select, Date:FTR)
#sel17_20 <- lapply(df17_20[2], select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR )

sel17_20 <- lapply(df17_20, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR )

#Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo (Hint 1: usa as.Date y mutate para arreglar las fechas). 


mut17_18 <- lapply(sel17_20[1], mutate, Date = as.Date(Date, "%d/%m/%y"))
mut18_20 <- lapply(sel17_20[2:3], mutate, Date = as.Date(Date, "%d/%m/%Y"))
mut17_20 <- c(mut17_18, mut18_20)

head(mut17_20)
View(mut17_20)

#Con ayuda de la función rbind forma un único data frame que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).


data17_20 <- do.call(rbind, mut17_20)
head(data17_20)
tail(data17_20)
