library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Visualización de Distribuciones de Probabilidad Discretas y Continuas"),
  sidebarLayout(
    sidebarPanel(
      selectInput("group", "Selecciona un grupo de distribuciones:",
                  choices = c("Discretas", "Continuas")),
      uiOutput("distUI"),
      sliderInput("nvar", "Número de variables a simular", 
                  min = 100, max = 1000, value = 500, step = 100),
      sliderInput("n", "Número de ensayos (sólo para Binomial):", 
                  value = 10, min = 1, max = 50, step = 1),
      sliderInput("lambda", "Lambda (sólo para Poisson y exponencial):", 
                  value = 1, min = 0, max = 10, step = 0.1),
      sliderInput("p", "Probabilidad de éxito (sólo para binomial y geométrica):", 
                  value = 0.5, min = 0, max = 1, step = 0.01),
      sliderInput("mu", "Media (sólo para normal y gamma):", 
                  value = 0, min = -10, max = 10, step = 0.1),
      sliderInput("sd", "Desviación estándar (sólo para normal y t-student):", 
                  value = 1, min = 0.1, max = 5, step = 0.1),
      sliderInput("alpha", "Alfa (sólo para t-student):", 
                  value = 1, min = 0.1, max = 10, step = 0.1),
      sliderInput("beta", "Beta (sólo para beta y gamma):", 
                  value = 1, min = 0.1, max = 10, step = 0.1)
    ),
    mainPanel(
      plotOutput("plot",height = "650px",width = "650px")
    )
  )
)

server <- function(input, output, session) {
  output$distUI <- renderUI({
    if (input$group == "Discretas") {
      selectInput("dist", "Selecciona una distribución:",
                  choices = c("Binomial", "Poisson", "Geométrica"))
    } else {
      selectInput("dist", "Selecciona una distribución:",
                  choices = c("Exponencial", "Normal", "Gamma", "Beta", "T-Student"))
    }
  })
  
  output$plot <- renderPlot({
    dist <- switch(input$dist,
                   "Binomial"    = rbinom(input$nvar, input$n, input$p),
                   "Poisson"     = rpois(input$nvar, input$lambda),
                   "Geométrica"  = rgeom(input$nvar, input$p),
                   "Exponencial" = rexp(input$nvar, input$lambda),
                   "Normal"      = rnorm(input$nvar, input$mu, input$sd),
                   "Gamma"       = rgamma(input$nvar, input$alpha, input$beta),
                   "Beta"        = rbeta(input$nvar, input$alpha, input$beta),
                   "T-Student"   = rt(input$nvar, input$alpha)
    )
    ggplot(data.frame(x = dist), aes(x)) +
      geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
      labs(title = paste("Distribución", input$dist),
           x = "Valor", y = "Frecuencia") +
      theme(plot.title = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.2)),
            axis.text = element_text(size = rel(1.2)),
            axis.line = element_line(size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border     = element_blank(),
            plot.background  = element_blank(),
            plot.margin      = unit(c(1, 1, 1, 1), "cm"))
  })
}
shinyApp(ui = ui, server = server)