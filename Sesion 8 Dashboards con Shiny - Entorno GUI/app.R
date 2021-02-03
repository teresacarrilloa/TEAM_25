#Para este postwork genera un dashboard en un solo archivo app.R, para esto realiza lo siguiente: 

#Ejecuta el código momios.R Almacena los gráficos resultantes en formato png 

#Crea un dashboard donde se muestren los resultados con 4 pestañas: 
#install.packages("shinythemes")
#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)


library(shinythemes)

#Esta parte es el análogo al ui.R
ui <- 
  
  fluidPage(
    
    dashboardPage(
      
      dashboardHeader(title = "Dasboard PW8"),
      
      dashboardSidebar(
        
        sidebarMenu(
          #Una con las gráficas de barras, donde en el eje de las x se muestran los goles de local y visitante con un menú de selección, 
          #con una geometría de tipo barras además de hacer un facet_wrap con el equipo visitante 
          
          menuItem("Gráfica de barras", tabName = "gBarras", icon = icon("area-chart")),
          #Realiza una pestaña donde agregues las imágenes de las gráficas del postwork 3 
          
          menuItem("Gráficas PW3", tabName = "gPW3", icon = icon("area-chart")),
          
          #En otra pestaña coloca el datatable del fichero match.data.csv 
          menuItem("Data Table", tabName = "data_table", icon = icon("table")),
          
          #Por último en otra pestaña agrega las imágenes de las gráficas de los factores de ganancia mínimo y máximo 
          
          menuItem("Imágenes F Min y MAX", tabName = "img", icon = icon("file-picture-o"))
        )
        
      ),
      
      dashboardBody(
        
        tabItems(
          
          #Una con las gráficas de barras, donde en el eje de las x se muestran los goles de local y visitante con un menú de selección, 
          #con una geometría de tipo barras además de hacer un facet_wrap con el equipo visitante 
          
          tabItem(tabName = "gBarras",
                  
                  fluidRow(
                    titlePanel("Gráfico de barras: goles de local y visitante"), 
                    
                    sidebarPanel(
                      p("Goles x equipo"), 
                      selectInput("x", "Seleccione: local o visitante",
                                  choices = c("Goles local"= "home.score","Goles visitante" = "away.score")),
                      box(plotOutput("plot1", height = 500, width = 500)),
                      ),
                    
                  
                  )
          ),
          
          #Realiza una pestaña donde agregues las imágenes de las gráficas del postwork 3 
          
          tabItem(tabName = "gPW3", 
                          fluidRow(
                            titlePanel(h3("Probabilidades Marginales y Conjuntas")),
                            img( src = "Rplot3_1.png", 
                                 height = 350, width = 350),
                            img( src = "Rplot3_2.png", 
                                 height = 350, width = 350),
                            img( src = "Rplot3_3.png", 
                                 height = 350, width = 350)
                          )
          ),
          
          
          #En otra pestaña coloca el datatable del fichero match.data.csv 
          tabItem(tabName = "data_table",
                  fluidRow(        
                    titlePanel(h3("Data Table")),
                    dataTableOutput ("data_table")
                  )
          ), 
          
          #Por último en otra pestaña agrega las imágenes de las gráficas de los factores de ganancia mínimo y máximo 
          
          tabItem(tabName = "img",
                  fluidRow(
                    titlePanel(h3("Imágen con secuencia de apuestas")),
                    img( src = "Rplot1.png", 
                         height = 350, width = 350),
                    img( src = "Rplot2.png", 
                         height = 350, width = 350)
                    
                  )
          )
          
        )
      )
    )
  )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
  library(ggplot2)
  
  datasetImput <- reactive(
    switch(input$dataset, 
           "away.team" = away.team, 
           "home.team" = home.team)
  )
  match.table <- read.csv("match.data.csv")
  
    output$plot1 <- renderPlot({
      data <-  read.csv("match.data.csv", header = T)
      x <- data[,input$x]
      data %>% ggplot(aes(x, fill = "lightblue")) + 
        geom_bar() + 
        facet_wrap("away.team", scales = "free") +
        labs(x =input$x, y = "Goles anotados") +
        ylim(0,76)
    
    
    })
  
  
  
  dir()
  data<-read.csv("data.csv")
  #Data Table
  output$data_table <- renderDataTable( {data}, 
                                        options = list(aLengthMenu = c(5,25,50),
                                                       iDisplayLength = 5)
  )
  
}

shinyApp(ui, server)

#Una con las gráficas de barras, donde en el eje de las x se muestran los goles de local y visitante con un menú de selección, 
#con una geometría de tipo barras además de hacer un facet_wrap con el equipo visitante 

#Realiza una pestaña donde agregues las imágenes de las gráficas del postwork 3 

#En otra pestaña coloca el datatable del fichero match.data.csv 

#Por último en otra pestaña agrega las imágenes de las gráficas de los factores de ganancia mínimo y máximo 

#Nota: recuerda que si tienes problemas con el codificado guarda tu archivo app.R con la codificación UTF-8