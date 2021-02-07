getwd()
setwd("C:/Users/gely_/Documents/Proyecto Fase 2/Trabajando en R/Acoso y violencia en diferentes aspectos")

#Se extraen las bases de datos
v.transporte <- filter(read.csv("Percepcion_inseguridad_transporte.csv"), Entidad != "Nacional" )
colnames(v.transporte)[2] <- "Anio" 

v.ayuda <- filter(read.csv("Violencia_ayuda.csv"), Entidad != "Nacional" )
colnames(v.ayuda)[2] <- "Anio" 

v.agresor <- filter(read.csv("Violencia_cualquier_agresor_largo_de_su_vida.csv"), Entidad != "Nacional" )
colnames(v.agresor)[2] <- "Anio" 

v.cualquierTipo <- filter(read.csv("Violencia_lolargodesuvida.csv"), Entidad != "Nacional" )
colnames(v.cualquierTipo)[2] <- "Anio" 

v.exPareja <- filter(read.csv("Violencia_pareja_ex_pareja_largo_relacion.csv"), Entidad != "Nacional" )
colnames(v.exPareja)[2] <- "Anio" 

v.tiposDanio <- filter(read.csv("Violencia_tipo_daños.csv"), Entidad.federativa != "Nacional" )



library(dplyr)
library(ggplot2)
v.tiposDanio <- filter(v.tiposDanio, Indicador != "Sin daños")

v.Danio2006 <-  ggplot(v.tiposDanio,  aes(x = reorder (Entidad.federativa, -X2006), y = X2006, fill = Indicador)) + geom_bar(stat = "identity", width=.5) +
  scale_fill_manual(values = c("lightpink", "#ea899a","hotpink4")) +  
  ggtitle("Tipos de daños a la mujer en el 2006", subtitle = "Por porcentaje") +
  xlab("") + ylab("Porcentaje") +
  theme(panel.background = element_rect(fill = "#fff3f4"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
v.Danio2006 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),plot.subtitle = element_text(color = "#ea899a", size = 11, face = "bold"))

v.Danio2011 <-  ggplot(v.tiposDanio,  aes(x = reorder (Entidad.federativa, -X2011), y = X2011, fill = Indicador)) + geom_bar(stat = "identity", width=.5) +
  scale_fill_manual(values = c("lightpink", "#ea899a","hotpink4")) + 
  ggtitle("Tipos de daños a la mujer en el 2011", subtitle = "Por porcentaje") +
  xlab(" ") + ylab("Porcentaje") +
  theme(panel.background = element_rect(fill = "#fff3f4"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
v.Danio2011 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),plot.subtitle = element_text(color = "#ea899a", size = 11, face = "bold"))

v.Danio2016 <-  ggplot(v.tiposDanio,  aes(x = reorder (Entidad.federativa, -X2016), y = X2016, fill = Indicador)) + geom_bar(stat = "identity", width=.5) +
  scale_fill_manual(values = c("lightpink", "#ea899a","hotpink4")) + 
  ggtitle("Tipos de daños a la mujer en el 2016", subtitle = "Por porcentaje") +
  xlab("Tipos de daños") + ylab("Porcentaje") +
  theme(panel.background = element_rect(fill = "#fff3f4"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
v.Danio2016 + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),plot.subtitle = element_text(color = "#ea899a", size = 11, face = "bold"))


#
v.ayudaAnio <- ggplot(v.ayuda, aes(x = reorder(Entidad, X._mujeres), y = X._mujeres, fill = X._mujeres, group = Anio)) + 
  geom_line(aes(color = as.character(Anio)), size = 1.2) + 
  ggtitle("Ayuda proporcionada a la mujer por violencia", subtitle = "Por entidad federativa") +
  xlab(" ") + ylab("Cantidad de mujeres ") +
  geom_point( size=2, shape=21, fill="white") +
  theme(panel.background = element_rect(fill = "#fff3f4"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_manual(name = "Año",values = c("lightpink", "hotpink4", "#d8bfd8"))
v.ayudaAnio + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),plot.subtitle = element_text(color = "#ea899a", size = 11, face = "bold"))

v.agresorAnio <- ggplot(v.agresor, aes(x = reorder(Entidad, X._mujeres), y = X._mujeres, fill = X._mujeres, group = Anio)) + 
  geom_line(aes(color = as.character(Anio)), size = 1.2 ) + 
  ggtitle("Violencia de cualquier agresor distinto a la pareja a lo largo de su vida", subtitle = "Por entidad federativa") +
  xlab(" ") + ylab("Cantidad de mujeres ") +
  geom_point( size=2, shape=21, fill="white") +
  theme(panel.background = element_rect(fill = "#fff3f4"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_manual(name = "Año",values = c("lightpink", "hotpink4", "#d8bfd8"))
v.agresorAnio + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),plot.subtitle = element_text(color = "#ea899a", size = 11, face = "bold"))

v.cualquierTipoAnio <- ggplot(v.cualquierTipo, aes(x = reorder(Entidad, X._mujeres), y = X._mujeres, fill = X._mujeres, group = Anio)) + 
  geom_line(aes(color = as.character(Anio)), size = 1.2 ) + 
  ggtitle("Violencia de cualquier ambito y agresor", subtitle = "Por entidad federativa") +
  xlab(" ") + ylab("Cantidad de mujeres ") +
  geom_point( size=2, shape=21, fill="white") +
  theme(panel.background = element_rect(fill = "#fff3f4"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_manual(name = "Año",values = c("lightpink", "hotpink4", "#d8bfd8"))
v.cualquierTipoAnio + theme(
  plot.title = element_text(color = "black", size = 15, face = "bold"),plot.subtitle = element_text(color = "#ea899a", size = 11, face = "bold"))


v.tiposDanioFyE <- 

#library(cowplot)
#bottom_row <- plot_grid(v.ayudaAnio, v.agresorAnio, labels = c('B', 'C'), label_size = 12)
#plot_grid(v.cualquierTipoAnio, bottom_row, label_size = 12, ncol = 1)
