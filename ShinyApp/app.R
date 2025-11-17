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

#-----------------------------------------------------------------------------
datos <- read.csv(here("Data", "Clean", "data.csv"))



ui <- fluidPage(
  titlePanel("Titulo tentativo (Producción de energia )"),

  sidebarLayout(
   
    sidebarPanel(
        h3("Filtros"),
        #Para seleccionar el año
        numericInput( "anio", "Año", value = 2024, min = 2000, max = 2024 ),
        #Para seleccionar que medida
        selectizeInput( "medida", "Selecciona medidas descriptivas", list("Media" = "mean", "Desviacion" = "sd"), multiple = FALSE ),
    ),

    mainPanel(
      plotOutput("graf1"),   # <--- AQUÍ se mostrará output$graf1
      plotOutput("graf2")    # <--- y aquí output$graf2 (si lo usas)
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

  # Si antes hacías:  prueba <- funcion_main(...); prueba[1]
  # ahora es:
  output$graf1 <- renderPlot({
    graficas()[[1]]            # [[1]] para obtener la primera gráfica
  })

  # Y la segunda:
  output$graf2 <- renderPlot({
    graficas()[[2]]            # [[2]] para la segunda gráfica
  })
    
 


}

shinyApp(ui, server)
