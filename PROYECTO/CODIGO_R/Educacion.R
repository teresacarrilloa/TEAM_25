library(ggplot2)
library(dplyr)

getwd()
setwd("D:/Archivos/Cursos/BEDU/II.Programación y Estadistica con R/PROYECTO/SCRIPT")
dl_1920 <- read.csv("Lic_19_20.csv", sep=";")
dl_1112<- read.csv("Lic_11_12.csv", sep=";")
dp_1920 <- read.csv("Pos_19_20.csv", sep=";")
dp_1112 <- read.csv("Pos_11_12.csv", sep=";")

str(dl_1920)
str(dl_1112)
str(dp_1920)
str(dp_1112)

Sdl_1920 <- select(dl_1920, ENTIDAD.FEDERATIVA, NOMBRE.PROGRAMA.EDUCATIVO, Matrícula.Mujeres,Nuevo.Ingreso.Mujeres,Egresados.Mujeres)
Sdl_1112 <- select(dl_1112,ENTIDAD.FEDERATIVA, PROGRAMA.DE.ESTUDIOS, Matrícula.Mujeres, Primer.Ingreso.Mujeres, Egresados.Mujeres)
Sdp_1920 <- select(dp_1920,ENTIDAD.FEDERATIVA, NOMBRE.PROGRAMA.EDUCATIVO,Matrícula.Mujeres, Nuevo.Ingreso.Mujeres,Egresados.Mujeres)
Sdp_1112 <- select(dp_1112, ENTIDAD.FEDERATIVA, PROGRAMA.DE.ESTUDIOS,Matrícula.Mujeres, Primer.Ingreso.Mujeres, Egresados.Mujeres )

Sdl_1920 <- rename(Sdl_1920, Estado=ENTIDAD.FEDERATIVA, Carrera=NOMBRE.PROGRAMA.EDUCATIVO, Matricula= Matrícula.Mujeres,Ingreso=Nuevo.Ingreso.Mujeres,Egreso=Egresados.Mujeres)
Sdl_1112 <- rename(Sdl_1112,Estado=ENTIDAD.FEDERATIVA, Carrera=PROGRAMA.DE.ESTUDIOS, Matricula=Matrícula.Mujeres, Ingreso=Primer.Ingreso.Mujeres, Egreso=Egresados.Mujeres)
Sdp_1920 <- rename(Sdp_1920,Estado=ENTIDAD.FEDERATIVA, Carrera=NOMBRE.PROGRAMA.EDUCATIVO, Matricula= Matrícula.Mujeres,Ingreso=Nuevo.Ingreso.Mujeres,Egreso=Egresados.Mujeres)
Sdp_1112 <- rename(Sdp_1112, Estado=ENTIDAD.FEDERATIVA, Carrera=PROGRAMA.DE.ESTUDIOS, Matricula=Matrícula.Mujeres, Ingreso=Primer.Ingreso.Mujeres, Egreso=Egresados.Mujeres)
View(Sdl_1920)
str(Fdl_1112)

Fdl_1920 <- filter(Sdl_1920, Carrera=="TOTAL")
Fdl_1112 <- filter(Sdl_1112, Carrera=="TOTAL")
Fdp_1920 <- filter(Sdp_1920,Carrera=="TOTAL")
Fdp_1112 <- filter(Sdp_1112, Carrera=="TOTAL")
str(Fdl_1920)
#MATRICULAS
#LIC 2011 - 2012
g <- ggplot(Fdl_1112, aes(x = Estado, y = Matricula, fill=Matricula)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("No. de Matricula de Mujeres a Nivel Licenciatura", subtitle = "por Estado de la República Mexicana (2011 - 2012)") +
  xlab("Estados") + ylab("No. de matriculas") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#LIC 2019 - 2020
g2 <- ggplot(Fdl_1112, aes(x = Estado, y = Matricula, fill=Matricula)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("No. de Matricula de Mujeres a Nivel Licenciatura", subtitle = "por Estado de la República Mexicana (2019 - 2020)") +
  xlab("Estados") + ylab("No. de matriculas") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g2 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#POS 2011 - 2012
g3 <- ggplot(Fdp_1112, aes(x = Estado, y = Matricula, fill=Matricula)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("No. de Matricula de Mujeres a Nivel Posgrado", subtitle = "por Estado de la República Mexicana (2011 - 2012)") +
  xlab("Estados") + ylab("No. de matriculas") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g3 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#POS 2019- 2020
g4 <- ggplot(Fdp_1112, aes(x = Estado, y = Matricula, fill=Matricula)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("No. de Matricula de Mujeres a Nivel Posgrado", subtitle = "por Estado de la República Mexicana (2019 - 2020)") +
  xlab("Estados") + ylab("No. de matriculas") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g4 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#INGRESO ################################################################3
#LIC 2011 - 2012

str(Fdl_1112)
g <- ggplot(Fdl_1112, aes(x = Estado, y = Ingreso, fill=Ingreso)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("Ingreso de Mujeres a Nivel Licenciatura", subtitle = "por Estado de la República Mexicana (2011 - 2012)") +
  xlab("Estados") + ylab("Ingreso") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#LIC 2019 - 2020
g2 <- ggplot(Fdl_1112, aes(x = Estado, y = Ingreso, fill=Ingreso)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("Ingreso de Mujeres a Nivel Licenciatura", subtitle = "por Estado de la República Mexicana (2019 - 2020)") +
  xlab("Estados") + ylab("Ingreso") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g2 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#POS 2011 - 2012
g3 <- ggplot(Fdp_1112, aes(x = Estado, y = Ingreso, fill=Ingreso)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("Ingreso de Mujeres a Nivel Posgrado", subtitle = "por Estado de la República Mexicana (2011 - 2012)") +
  xlab("Estados") + ylab("No. de Ingreso") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g3 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#POS 2019- 2020
g4 <- ggplot(Fdp_1112, aes(x = Estado, y = Ingreso, fill=Ingreso)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("Ingreso de Mujeres a Nivel Posgrado", subtitle = "por Estado de la República Mexicana (2019 - 2020)") +
  xlab("Estados") + ylab("No. de Ingreso") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g4 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))
#comparación LIC 2011. 2012 - 2019 2020

str(Fdl_1112)
#EGRESOS ######################################################
#LIC 2011 - 2012
g <- ggplot(Fdl_1112, aes(x = Estado, y = Egreso, fill=Egreso)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("Egresos de Mujeres a Nivel Licenciatura", subtitle = "por Estado de la República Mexicana (2011 - 2012)") +
  xlab("Estados") + ylab("No. de Egresos") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#LIC 2019 - 2020
g2 <- ggplot(Fdl_1112, aes(x = Estado, y = Egreso, fill=Egreso)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("No. de Egresos de Mujeres a Nivel Licenciatura", subtitle = "por Estado de la República Mexicana (2019 - 2020)") +
  xlab("Estados") + ylab("No. de Egresos") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g2 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#POS 2011 - 2012
g3 <- ggplot(Fdp_1112, aes(x = Estado, y = Egreso, fill=Egreso)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("No. de Egresos de Mujeres a Nivel Posgrado", subtitle = "por Estado de la República Mexicana (2011 - 2012)") +
  xlab("Estados") + ylab("No. de Egresos") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g3 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#POS 2019- 2020
g4 <- ggplot(Fdp_1112, aes(x = Estado, y = Egreso, fill=Egreso)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightpink", high = "hotpink4") + 
  theme(legend.position="none") + 
  ggtitle("No. de Egreso de Mujeres a Nivel Posgrado", subtitle = "por Estado de la República Mexicana (2019 - 2020)") +
  xlab("Estados") + ylab("No. de Egresos") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g4 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#comparación LIC INGRESO 2011. 2012 - 2019 2020 ##################################################
head(cLic) 
cLic<-mutate(Fdl_1112, Ingreso2=Fdl_1920$Ingreso, Egreso2=Fdl_1920$Egreso)
cgl <- ggplot(cLic, aes(x = Estado, y = Ingreso)) + geom_point() +
  ggtitle("Comparación de ingreso de mujeres a Licenciatura", subtitle = "por Estado de la República Mexicana (2011-2012/2019-2020") +
  xlab("Estados") + ylab("No. de Ingresos")  +
  geom_point(aes(y=Ingreso2),color="purple")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
cgl + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#comparación LIC EGRESO 2011. 2012 - 2019 2020 ##################################################
cgl2 <- ggplot(cLic, aes(x = Estado, y = Egreso)) + geom_point() +
  ggtitle("Comparación de egreso de mujeres a Licenciatura", subtitle = "por Estado de la República Mexicana (2011-2012/2019-2020") +
  xlab("Estados") + ylab("No. de Egresos")  +
  geom_point(aes(y=Egreso2),color="purple")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
cgl2 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#comparación POS EGRESO 2011. 2012 - 2019 2020 ##################################################
CPos <- mutate(Fdp_1112, Ingreso1=Fdp_1920$Ingreso, Egreso2=Fdp_1920$Egreso)
cgl3 <- ggplot(CPos, aes(x = Estado, y = Egreso)) + geom_point() +
  ggtitle("Comparación de egreso de mujeres a Posgrado", subtitle = "por Estado de la República Mexicana (2011-2012/2019-2020") +
  xlab("Estados") + ylab("No. de Egresos")  +
  geom_point(aes(y=Egreso2),color="purple")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
cgl3 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))

#comparación POS INGRESO 2011. 2012 - 2019 2020 ##################################################
cgl4 <- ggplot(CPos, aes(x = Estado, y = Egreso)) + geom_point() +
  ggtitle("Comparación de ingreso de mujeres a Posgrado", subtitle = "por Estado de la República Mexicana (2011-2012/2019-2020") +
  xlab("Estados") + ylab("No. de Ingresos")  +
  geom_point(aes(y=Egreso2),color="purple")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
cgl4 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),
  plot.subtitle = element_text(color = "hotpink4", size=13))


