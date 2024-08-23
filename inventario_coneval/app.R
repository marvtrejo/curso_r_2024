library(shiny)
library(bslib)
library(bsicons)

# source("contenido_app.R")

# UI ####
ui <- page_sidebar(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  title =  fluidRow(
    column(12,
           tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/5/5b/Logo_CONEVAL.svg", 
                    style = "height:10vh; max-height:10vh;"),
           column(12, 
                  HTML("<h1>Programas y acciones federales de desarrollo social</h1>"),
                  HTML("<h2>Información histórica 2009-2024</h2>"))
           )
  ),
  footer = tags$div(style = "font-size: 10px; padding-right:20px",
                    "Curso de visualización de información con R para la evaluación de políticas públicas 2024. Evaluación final.",
                    br(),"Elaborado por Marvin Ivan Trejo Mendez"), 
  sidebar = sidebar(
    uiOutput("selCiclo"),
    uiOutput("selRamo"),
    uiOutput("selPP")),
  uiOutput("titulo_dg"),
  uiOutput("subtitulo_dg"),
  layout_column_wrap(
    width = 1/3,
    heights_equal = "row",
    datosgrales_card, poblacion_card, presupuesto_card) 
    # layout_column_wrap(
    #   width = 1,
    #   heights_equal = "row",
    #   poblacion_card, presupuesto_card))
)


# Server ####
server <- function(input, output, session) {

# Controles ####
  
  output$selCiclo <- renderUI({
    selectizeInput(inputId = "selCiclo",
                label = "Seleccione un año:",
                choices = c(2009:2022), 
                selected = 2022)
  })
  
  output$selRamo <- renderUI({
    req(input$selCiclo)
    selectizeInput(inputId = "selRamo",
                label = "Seleccione la dependencia:",
                choices = ramos_disp(input$selCiclo))
  })
  
  output$selPP <- renderUI({
    req(input$selCiclo, input$selRamo)
    selectInput(inputId = "selPP",
                label = "Seleccione el programa:",
                choices = pp_disp(input$selCiclo, input$selRamo))
  })
    
# Datos generales ####  
  output$titulo_dg <- renderUI({
    req(input$selCiclo, input$selRamo,input$selPP)
    h2(paste(modcve(input$selCiclo, input$selRamo,input$selPP), 
             " ", 
             nombre(input$selCiclo, input$selRamo,input$selPP)),
      style = "color: #2c3e50; font-weight: bold; text-align: center;")
  })
  
  output$subtitulo_dg <- renderUI({
    req(input$selCiclo, input$selRamo,input$selPP)
    h4(ramo(input$selCiclo, input$selRamo,input$selPP),
       style = "color: #A9A9A9; font-weight: bold; text-align: center;")
  })

  output$ainicio_box<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    ainicio(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$descr_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    descripcion(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$derecho_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    derecho(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$etapav_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    etapav(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$gpoatn_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    gpoatn(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$apoyos_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    apoyos(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$tabla_mir<-renderTable({
    req(input$selCiclo, input$selRamo,input$selPP)
    
    mir_f = fin(input$selCiclo, input$selRamo,input$selPP)
    mir_p = proposito(input$selCiclo, input$selRamo,input$selPP)
    mir_c = componentes(input$selCiclo, input$selRamo,input$selPP)
    mir_a = actividades(input$selCiclo, input$selRamo,input$selPP)
    
    gen_mir(mir_f, mir_p, mir_c, mir_a)
  })
  
  output$tabla_eval<-renderTable({
    req(input$selCiclo, input$selRamo,input$selPP)

    eval_anio = anio_eval(input$selCiclo, input$selRamo,input$selPP)
    eval_titulo = titulo_eval(input$selCiclo, input$selRamo,input$selPP)
    eval_enlace = vinculo_eval(input$selCiclo, input$selRamo,input$selPP)

    gen_eval(eval_anio, eval_titulo, eval_enlace)
  })

  # normatividad
  # norma_v
# Poblaciones ####
  output$umpp_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    unidad_pp(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$defpp_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    def_pp(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$umpo_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    unidad_po(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$defpo_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    def_po(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$umpa_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    unidad_pa(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$defpa_ac<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    def_pa(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$graf_pob <- renderPlot({
    req(input$selCiclo, input$selRamo, input$selPP)
    grafica_poblaciones(inv_fed, input$selCiclo, input$selRamo, input$selPP)
  })

# Presupuesto ####  
  output$graf_pres <- renderPlot({
    req(input$selCiclo, input$selRamo, input$selPP)
    grafica_presupuesto(inv_fed, input$selCiclo, input$selRamo, input$selPP)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)