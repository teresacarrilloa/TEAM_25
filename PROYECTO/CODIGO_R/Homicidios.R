library(readxl)
df <- read_excel("BEDU/Proyecto/Homicidio Pauchi/Data set.xlsx",  +     sheet = "2019")
View(df)

summary(df)
library(dplyr)

#Pregunta de investigación: ¿que nos dicen los datos de homicidios de mujeres en México?
#Data frame por estados de la República Mexicana
#Hipótesis: Encontraremos estados con un mayor número de homicidios, no habrá diferencias
#significativas regionales, se mostrarán más homogéneas las 8 regiones. 

df[order(-df$Total),] #EdoMex 472, Guanajuato 412, Coahuila 308
df[order(-df$`Habla indígena`),] #Oaxaca 40, Guerrero 19, Puebla 13
df[order(-df$Trabaja),] #Edomex 237, Guanajuato 164, Jalisco 113
df[order(-df$`No trabaja`),] #Guanajuato 196, EdoMex 190, Chihuahua 126
df[order(-df$`Vía pública`),] #EdOmex 216, Guanajuato 207, Coahuila 95
df[order(-df$Hogar),] #Guanajuato 98, EdoMex 92,Jalisco 45
df[order(-df$Trabajo),] #EdoMex 32, Guanajuato 26, Veracruz 22
df[order(-df$`Sin escolaridad`),] #Guerrero, Oaxaca 21, Guanajuato 16
df[order(-df$`Secundaria trunca`),] #EdoMex 43, Guanajuato 27, Michaocan 18
df[order(-df$`Secundaria terminada`),] #Guanajuato 126, Edomex 103, Chihuahua 89
df[order(-df$`Bachillerato o prepa trunca`),] #EdoMex 42, Guanajuato 13, Jalisco 13
df[order(-df$`Bachillerato o prepa completa`),] #EdoMex 58, Guanajuato 32, Guerrero 30
df[order(-df$Profesional),] #Edomex 52, Guanajuato 27, Guerrero 27
df[order(-df$`Parentesco con el agresor`),] #Chiapas 3, Edomex 3, Oaxaca 3

lshap <- lapply(df[,2:16], shapiro.test)
lres <- sapply(lshap, `[`, c("statistic","p.value")) #Poner estadistico y p en lista
lres
t(lres) #Distribucion no parametrica 

library(matrixTests)

col_kruskalwallis(df[,2:16], df$Grupo) #Sin diferencias significativas regionales 

library(Hmisc)

df_plot <- df[2:33,]

describe(df,IQR=T,data=df) #Estadistica descriptiva


g <- ggplot(df_plot, aes(x = reorder (Estado, -Total), y = Total, fill=Total)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("No. de homicidios de mujeres en el 2019", subtitle = "por Estado de la República Mexicana") +
  xlab("Estados") + ylab("No. de muertes") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


g + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#Via publica 
v <- ggplot(df_plot, aes(x = reorder (Estado, -Total), y = Total, fill=`Vía pública`)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
   ggtitle("No. de homicidios en vía pública de mujeres en el 2019", subtitle = "por Estado de la República Mexicana") +
  xlab("Estados") + ylab("No. de muertes") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


v + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size= 13))

#Hogar
h <- ggplot(df_plot, aes(x = reorder (Estado, -Total), y = Total, fill=Hogar)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  ggtitle("No. de homicidios dentro del hogar de mujeres en el 2019", subtitle = "por Estado de la República Mexicana") +
  xlab("Estados") + ylab("No. de muertes") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


h + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size= 13))

library(ggpubr)
ggarrange(v, h,labels = c("A", "B"), ncol = 1, nrow = 2)

#ANALISIS POR EDADES
#Pregunta de investigación: ¿Cuál es la edad más vulnerable de la mujer mexicana?
"Hipótesis: Las adolescentes y jóvenes serán el grupo de edad más vulnerable. 

edad <- read_excel("BEDU/Proyecto/Homicidio Pauchi/Data set.xlsx", 
                   +     sheet = "edadt")

#EDADES TOTALES
Ed_tot <- ggplot(edad, aes(x =Edad, y = Total, fill=Total)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("No. de homicidios de mujeres en el 2019 en México", subtitle = "por rango de edad") +
  xlab("Rangos de edad (años)") + ylab("No. de muertes") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

Ed_tot + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 13),
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13)) + 
  coord_flip()


#Series de tiempo
#Pregunta de investigación: ¿Existe una tendencia en el número de homicidios por mes?
Hipótesis: Observaremos una tendencia estacional. 

library(TSA)
experim.ts <- ts(experim, start = 1990,end= 2019, freq = 12)


experim.decom.M <- decompose(experim.ts, type = "multiplicative")
plot(experim.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de producción de")
Trend <- experim.decom.M$trend
Seasonal <- experim.decom.M$seasonal
Random <- experim.decom.M$random
ts.plot(cbind(Trend, Trend*Seasonal), xlab = "Tiempo", main = "Datos de No. de homicidios de mujeres en México", 
        ylab = "No. de muertes", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")
Trend[7]*Seasonal[7]*Random[7]
experim.ts[7]
ts.plot(experim.ts)
Trend[100]*Seasonal[100]*Random[100]
experim.ts[100]



plot(experim.ts, main = "No. de homicidios de mujeres en México", ylab = "No. de muertes", xlab = "Tiempo", ylim = c(100, 800))
lines(experim.decom.M$trend , col = "darkmagenta", lwd = 2)
lines(experim.decom.M$seasonal * experim.decom.M$trend, col = "deeppink2", lty = 2, lwd = 2 )
legend(1949, 800, c('Serie de tiempo original', 'Tendencia', 'Tendencia x Estacionalidad'),col = c('black', 'purple', 'red'), text.col = "green4", lty = c(1, 1, 2), lwd = c(1, 2, 2), merge = TRUE, bg = 'gray90')

