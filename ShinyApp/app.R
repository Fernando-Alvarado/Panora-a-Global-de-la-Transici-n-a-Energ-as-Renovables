library(shiny)
library(ggplot2)
library(tidyverse)
library(here)
library(countrycode)
library(broom)
library(esquisse)
library(forcats)
library(rpart)
library(rpart.plot)
library(Metrics)
library(knitr)
#Paqueta
source(here("ShinyApp", "modulos", "funcion_main.R")) # Funcion para traer las graficas

#--------Carga de datos---------------------------------------------------------------------
datos <- read.csv(here("Data", "Clean", "data.csv")) 
#------------------------------------------------------------------------------------------------ 

#    shiny::runApp("ShinyApp")


ui <- fluidPage(
  titlePanel("Titulo tentativo (Producción de energia )"),

  sidebarLayout(
    sidebarPanel(
        h3("Filtros"),
        #Para seleccionar el año
        numericInput( "anio", "Año", value = 2024, min = 2000, max = 2024 ),
        #Para seleccionar que medida
        selectizeInput( "medida", "Selecciona medidas descriptivas", list("Media" = "mean", "Desviacion" = "sd"), multiple = FALSE )
    ),

    mainPanel(
      #Graficas que hiciero Diego y Brayan
      fluidRow(
        column(6, plotOutput("graf1")),
        column(6, plotOutput("graf2"))
    ),

    fluidRow(
        column(6, plotOutput("graf3")),
        column(6, plotOutput("graf4"))
    ) 
  )
)
)

server <- function(input, output, session) {
     # Llama a tu funcion_main usando los inputs
  graficas <- reactive({
    funcion_main(
      datos,
      año1   = input$anio,     # viene del numericInput
      medida = input$medida    # viene del selectInput
    )
  })

  #Haciendo la primera grafica:
  output$graf1 <- renderPlot({
    graficas()[[1]]            
  })

  # Y la segunda:
  output$graf2 <- renderPlot({
    graficas()[[2]]            
  })

  output$graf3 <- renderPlot({
    graficas()[[3]]            
  })
  
  output$graf4 <- renderPlot({
    graficas()[[4]]            
  })

 
    
 


}

shinyApp(ui, server)
