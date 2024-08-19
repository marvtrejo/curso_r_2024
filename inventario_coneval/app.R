library(shiny)
library(bslib)
library(bsicons)

# source("contenido_app")


# UI ####
ui <- page_sidebar(
  theme = bslib::bs_theme(bootswatch = "minty"),
  title = tags$div(
    style = "display: flex; text-align: center; font-weight: bold; font-size: 36px; padding-right:20px; justify-content: center",
    "Programas y acciones federales de desarrollo social (2009-2014)",
    tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQoCwyJ5FDG3rlTp-n_qX9TB603Adrk8Od14w&s", 
             height = "100px", 
             style = "display: flex; align-items: right")
    ), 
  sidebar = sidebar(
      radioButtons("dist", "Distribución",
                   c("Normal"="norm",
                     "Uniform"="unif",
                     "Log-Normal"="lnorm",
                     "Exponential"="exp")),
    br(),
    sliderInput("n", 
                "Observaciones", 
                value = 100,
                min = 1, 
                max = 1000)
  ),
  navset_pill(
    header = br(),
    footer = tags$div(
      style = "font-size: 10px; padding-right:20px",
      "Curso de visualización de información con R para la evaluación de políticas públicas 2024. Evaluación final.",
      br(),
      "Elaborado por Marvin Ivan Trejo Mendez"), 
  # navset_card_underline(
    nav_panel("Datos generales",
              id_cards[[1]],
              layout_columns(id_vb[[1]],id_vb[[2]], id_vb[[3]]),
              layout_columns(id_cards[[2]], id_cards[[3]])
              ), 
    nav_panel("Poblaciones", plotOutput("población")), 
    nav_panel("Presupuesto", plotOutput("presupuesto")), 
    nav_panel("Evolución", plotOutput("evolucion"))
  )
)

# Server ####
server <- function(input, output, session) {
  d <- reactive({
    dist <- switch(input$dist, 
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm, 
                   exp = rexp, 
                   rnorm)
    dist(input$n)
  })
  
# Datos generales ####  
  output$descripcion <- renderText({
    "Descripción del programa"
    })
  output$normatividad <- renderText({
    "Normatividad del programa"
  })
  output$apoyos <- renderText({
    "Apoyos que entrega el programa"
  })
  
# Presupuesto ####  
  output$presupuesto <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#007bc2", border = "white")
  })
  
  output$evolucion <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#007bc2", border = "white")
  })
  
  output$poblacion <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#007bc2", border = "white")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
