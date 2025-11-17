
ui <- fluidPage(
  titlePanel("Titulo tentativo (Produccion de energia )"),

  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribución:",
                  choices = c("Normal", "Uniforme", "Exponencial")),
      sliderInput("n", "n:", min = 50, max = 2000, value = 500, step = 50),
      sliderInput("bins", "Número de bins:", min = 5, max = 50, value = 30)
    ),

    mainPanel(
      plotOutput("hist"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session) {

  # 1) Objeto reactivo: se recalcula cuando cambian sus inputs
  datos <- reactive({
    n <- input$n

    if (input$dist == "Normal") {
      rnorm(n)
    } else if (input$dist == "Uniforme") {
      runif(n)
    } else {
      rexp(n)
    }
  })

  # 2) Gráfica
  output$hist <- renderPlot({
    x <- datos()
    ggplot(data.frame(x), aes(x)) +
      geom_histogram(bins = input$bins)
  })

  # 3) Resumen numérico
  output$summary <- renderPrint({
    summary(datos())
  })
}

shinyApp(ui, server)
