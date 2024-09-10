
# source("contenido_app.R")

# UI ####

 ui <- page_sidebar(
   theme = bslib::bs_theme(bootswatch = "flatly"),
   title =  fluidRow(
     column(3,
            tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/5/5b/Logo_CONEVAL.svg",
                     style = "height:10vh; max-height:10vh;")
            ),
     column(9,
            HTML("<h2 style = 'margin:0;'>Programas y acciones federales de desarrollo social</h3>"),
            HTML("<h3 style = 'margin-top: 5px;'>Información histórica 2009-2022</h3>"))
            ),

   sidebar = sidebar(
     uiOutput("selCiclo"),
     uiOutput("selRamo"),
     uiOutput("selPP")),
   
   fluidRow(column(5, uiOutput("titulo_dg"), uiOutput("subtitulo_dg")),
            column(7, header_card)),
 
   fluidRow(
     column(5, datosgrales_card, evolucion_card), 
     column(7, poblacion_card, presupuesto_card)),

   fluidRow(
     column(12,
            tags$div(style = "font-size: 10px; padding-right:20px",
                     "Elaborado por Marvin Ivan Trejo Mendez"))
   )
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
                choices = ent_disp(input$selCiclo))
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
      style = "color: #2c3e50; font-weight: bold; text-align: left;")
  })
  
  output$subtitulo_dg <- renderUI({
    req(input$selCiclo, input$selRamo,input$selPP)
    h4(ramo(input$selCiclo, input$selRamo,input$selPP),
       style = "color: #A9A9A9; font-weight: bold; text-align: left;")
  })

  output$ainicio<-renderText({
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

  output$normatividad_p<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    normatividad(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$normatividad_v<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    norma_v(input$selCiclo, input$selRamo,input$selPP)
  })


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
  
  output$cobert <- renderText({
    req(input$selCiclo, input$selRamo, input$selPP)
    cobertura(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$efic <- renderText({
    req(input$selCiclo, input$selRamo, input$selPP)
    eficiencia(input$selCiclo, input$selRamo,input$selPP)
  })

# Presupuesto ####  
  output$graf_pres <- renderPlot({
    req(input$selCiclo, input$selRamo, input$selPP)
    grafica_presupuesto(inv_fed, input$selCiclo, input$selRamo, input$selPP)
  })
  
  output$prop_pres <- renderText({
    req(input$selCiclo, input$selRamo, input$selPP)
    porcentaje_pres(input$selCiclo, input$selRamo, input$selPP)
  })


  output$pres_tab<- renderDT({
    req(input$selCiclo, input$selRamo, input$selPP)
    tabla_pres(input$selCiclo, input$selRamo, input$selPP)
  })
  
  datos_pob <- reactive({
    req(input$selCiclo, input$selRamo, input$selPP)
    tabla_pob(input$selCiclo, input$selRamo, input$selPP)
  })
  
  output$pob_tab<- renderDT({
    df <- datos_pob()
    if(is.null(df)) return(NULL)
      
    datatable(df,
              options = list(pageLength = 50,
                             searching = FALSE,
                             language = list(
                               # paginate = list(previous = "Anterior", next = "Siguiente"),
                               info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
                               infoEmpty = "Mostrando 0 a 0 de 0 registros",
                               lengthMenu = "Mostar _MENU_ registros por página",
                               loadingRecords = "Cargando...",
                               emptyTable = "No hay datos disponibles"
                               )
              )
    ) %>%
      formatRound(columns = c("Población potencial", "Población objetivo", "Población atendida"),
                  digits = 0, mark = ",") %>%
      formatStyle(
        columns = c("Población potencial", "Población objetivo", "Población atendida"),
        backgroundColor = styleEqual(NA, "lightgray"),
        color = styleEqual(NA, "gray"),
        `text-align` = "center"
        )
  })
  
  output$evprog <- renderPlot({
    req(input$selCiclo, input$selRamo, input$selPP)
    gen_evoprog(input$selCiclo, input$selRamo, input$selPP)
  })
  
  output$downloadPoblaciones <- downloadHandler(
    filename = function() {
      paste("poblaciones_", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos_pob(), file, row.names = FALSE)
    }
  )
  

  
}


# Run the application 
shinyApp(ui = ui, server = server)