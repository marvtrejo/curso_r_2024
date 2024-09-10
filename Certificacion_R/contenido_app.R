# Base de datos ####
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(bslib)
library(scales)
library(plotly)
library(DT)
library(shinycssloaders)
library(shiny)
library(networkD3)
library(bsicons)
library(timevis)
library(tidyr)

setwd("C:/Users/mitrejo/Mi unidad/SPPC/Github/curso_r_2024/Certificacion_R")

inv_fed <- read_excel("inv_fed.xlsx", sheet = "inv_fed") %>% 
  mutate(
    modcve = paste0(mod, "-", cve),
    ciclo  = factor(ciclo),
    pp_cantidad = as.numeric(pp_cantidad),
    po_cantidad = as.numeric(po_cantidad),
    pa_cantidad = as.numeric(pa_cantidad),
    p_aprobado = as.numeric(p_aprobado),
    p_ejercido = as.numeric(p_ejercido),
    cobertura = as.numeric(cobertura),
    eficiencia = as.numeric(eficiencia),
    no_entidades = as.numeric(entidades_atn),
    no_municipios = as.numeric(municipios_atn),
    no_localidades = as.numeric(localidades_atn)
  )
    # ,

    # pp_text = if_else(is.na(pp_cantidad), "No disponible", as.character(pp_cantidad)),
    # po_text = if_else(is.na(po_cantidad), "No disponible", as.character(po_cantidad)),
    # pa_text = if_else(is.na(pa_cantidad), "No disponible", as.character(pa_cantidad)),
    # aprobado_text = if_else(is.na(p_aprobado), "No disponible", as.character(p_aprobado)),
    # ejercido_text = if_else(is.na(p_ejercido), "No disponible", as.character(p_ejercido)),
    # cobertura_text = if_else(is.na(cobertura), "No disponible", as.character(cobertura)),
    # eficiencia_text = if_else(is.na(eficiencia), "No disponible", as.character(eficiencia)),
    # ent_text = if_else(is.na(no_entidades), "No disponible", as.character(no_entidades)),
    # mpio_text = if_else(is.na(no_municipios), "No disponible", as.character(no_municipios)),
    # loc_text = if_else(is.na(no_localidades), "No disponible", as.character(no_localidades))
  # ) %>%
  # mutate(
  #   pp_cantidad = number(pp_cantidad, big.mark = ",", decimal.mark = ".", accuracy = 2),
  #   po_cantidad = number(po_cantidad, big.mark = ",", decimal.mark = ".", accuracy = 2),
  #   pa_cantidad = number(pa_cantidad, big.mark = ",", decimal.mark = ".", accuracy = 2),
  #   p_aprobado = number(p_aprobado, big.mark = ",", decimal.mark = ".", accuracy = 2),
  #   p_ejercido = number(p_ejercido, big.mark = ",", decimal.mark = ".", accuracy = 2)
  # )
  
evaluaciones <- read_excel("inv_fed.xlsx", sheet = "evaluaciones")

evolucion <- read_excel("inv_fed.xlsx", sheet = "evolucion") %>%
  arrange(id_if, ciclo) %>% 
  group_by(id_if) %>% 
  mutate(cambio_lag = lag(cambio),
         ciclo_lag = lag(ciclo),
         grupo = cumsum(cambio != cambio_lag | ciclo != ciclo_lag +1 | is.na(cambio_lag))) %>% 
  group_by(id_if, cambio, grupo) %>% 
  mutate(año_fin = if_else(n()>1, max(ciclo), NA_integer_),
         año_fin = coalesce(año_fin,ciclo)) %>% 
  ungroup() %>% 
  select(-cambio_lag, -ciclo_lag, -grupo)



## Funciones ####
anio_sel = 2022
ent_disp <- function(anio_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel) %>% 
    pull(entidad) %>% 
    unique() %>% 
    sort()
}

pp_disp <- function(anio_sel, ent_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel) %>% 
    pull(nombre) %>% 
    unique() %>% 
    sort()
}

# UI ####
header_card <- layout_column_wrap(
  value_box(
    title = "Año de inicio",
    value = textOutput("ainicio"),
    showcase = bs_icon("calendar3"),
    theme = "text-info",
    showcase_layout = "left center",
    full_screen = FALSE, 
    fill = TRUE,
    height = 150L
  ),
  value_box(
    title = "Derecho social asociado",
    value = textOutput("derecho_ac"),
    theme = "text-info",
    showcase_layout = "top right",
    full_screen = FALSE, 
    fill = TRUE,
    height = 150L
  ),
  value_box(
    title = "Normatividad principal",
    value = textOutput("normatividad_p"),
    p(markdown("Disponible en: [enlace](textOutput(normatividad_v))")),
    theme = "text-info",
    showcase_layout = "top right",
    full_screen = FALSE, 
    fill = TRUE,
    height = 150L
  )
)


caracteristicas_card<- layout_column_wrap(
    value_box(
    title = "Porcentaje del presupuesto",
    value = textOutput("prop_pres"),
    p("Presupuesto de desarrollo social"),
    theme = "text-info",
    showcase_layout = "left center",
    full_screen = FALSE, 
    fill = TRUE,
    height = 150L
  ),
  value_box(
    title = "Cobertura",
    value = textOutput("cobert"),
    p("Población atendida / Población potencial"),
    theme = "text-info",
    showcase_layout = "left center",
    full_screen = FALSE, 
    fill = TRUE,
    height = 150L
  ),
  value_box(
    title = "Eficiencia",
    value = textOutput("efic"),
    p("Población atendida / Población objetivo"),
    theme = "text-info",
    showcase_layout = "left center",
    full_screen = FALSE, 
    fill = TRUE,
    height = 150L
  )
)

## Datos generales ####
datosgrales_card <- navset_card_tab(
  full_screen = TRUE,
  title = "Datos generales",
  nav_panel(
    "Descripción",
    textOutput("descr_ac")
  ),
  nav_panel(
    "MIR",
    tableOutput("tabla_mir")),
  nav_panel(
    "Evaluaciones",
    tableOutput("tabla_eval")
  )
) %>% withSpinner()

evolucion_card <- card(
  card_title("Evolución programática"),
  timevisOutput("evprog")
  )


poblacion_card <- navset_card_tab(
  full_screen = TRUE,
  title = "Poblaciones", 
  nav_panel("Cuantificación",
            plotOutput("graf_pob")),
  nav_panel("Definiciones",
              card_body(
                layout_column_wrap(
                width = 1/3,
                card(card_header("Población potencial"), textOutput("defpp_ac")),
                card(card_header("Población objetivo"), textOutput("defpo_ac")),
                card(card_header("Población atendida"), textOutput("defpa_ac"))
                )
              )
            ),
  nav_panel("Datos",
            downloadButton("downloadPoblaciones", "Descargar tabla"), br(), DT::DTOutput("pob_tab")),
  nav_panel(shiny::icon("circle-info"),"Información")
) %>% withSpinner()

presupuesto_card <- navset_card_tab(
  full_screen = TRUE,
  title = "Presupuesto", 
  nav_panel("Evolución",
            plotOutput("graf_pres")),
  nav_panel("Distribución geográfica"),
  nav_panel("Datos",
            DT::DTOutput("pres_tab"))
) %>% withSpinner()


## Funciones ####

ramo <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(entidad) %>% 
    unique() %>% 
    sort()
}

modcve <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(modcve) %>% 
    unique() %>% 
    sort()
}

nombre <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(nombre) %>% 
    unique() %>% 
    sort()
}

ainicio <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(inicio) %>% 
    unique() %>% 
    sort()
}

descripcion <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(descripcion) %>% 
    unique() %>% 
    sort()
}

derecho <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(derecho_directo) %>% 
    unique() %>% 
    sort()
}

etapav <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(etapa_vida) %>% 
    unique() %>% 
    sort()
}

gpoatn <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(gpo_atencion) %>% 
    unique() %>% 
    sort()
}

apoyos <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(nombre) %>% 
    unique() %>% 
    sort()
}

normatividad <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(normatividad) %>% 
    unique() %>% 
    sort()
}

norma_v <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(normatividad_v) %>% 
    unique() %>% 
    sort()
}

fin <- function(anio_sel, ent_sel, pp_sel){
  resultado <- inv_fed %>%
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(fin) %>% 
    unique() %>% 
    sort()
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}

proposito <- function(anio_sel, ent_sel, pp_sel){
  resultado <- inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(proposito) %>% 
    unique() %>% 
    sort()
  
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}


componentes <- function(anio_sel, ent_sel, pp_sel){
  resultado <- inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(componente) %>% 
    unique() %>% 
    sort()
  
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}

actividades <- function(anio_sel, ent_sel, pp_sel){
  resultado <- inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(actividad) %>% 
    unique() %>% 
    sort()
  
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}

gen_mir <- function(fin, proposito, componentes, actividades) {
  
  tabla_mir <- data.frame(
    Nivel = c("Fin", "Propósito", "Componentes", "Actividades"),
    Objetivo = c(fin, proposito, componentes, actividades),
    stringsAsFactors = FALSE 
  )
  
  return(tabla_mir)
}

anio_eval <- function(anio_sel, ent_sel, pp_sel){
  id <- evaluaciones %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>%
    arrange(ciclo) %>% 
    pull(id_if) %>% 
    unique()
  
  if (length(id) > 0) {
    id <- id[1]
  } else {
    return("Información no disponible")
  }
  
  resultado <- evaluaciones %>% 
    filter(id_if == id) %>% 
    pull(ciclo) %>% 
    sort()
  
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}

titulo_eval <- function(anio_sel, ent_sel, pp_sel){
  id <- evaluaciones %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(id_if) %>% 
    unique() %>% 
    sort()
  
  if (length(id) > 0) {
    id <- id[1]
  } else {
    return("Información no disponible")
  }
  
  resultado <- evaluaciones %>% 
    filter(id_if == id) %>%
    select (ciclo, tipo) %>% 
    arrange (ciclo) %>% 
    pull(tipo)
  
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}

vinculo_eval <- function(anio_sel, ent_sel, pp_sel){
  id <- evaluaciones %>%
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique() %>%
    sort()

  if (length(id) > 0) {
    id <- id[1]
  } else {
    return("Información no disponible")
  }

  resultado <- evaluaciones %>%
    filter(id_if == id) %>%
    arrange(ciclo) %>% 
    pull(vinculo)

  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  } 
    
  return(resultado)
  }

gen_eval <- function(anio_eval, titulo_eval, vinculo_eval) {
  
  tabla_eval <- data.frame(
    Año = as.integer(anio_eval),
    Tipo = titulo_eval,
    Vínculo = vinculo_eval,
    stringsAsFactors = FALSE 
  )
  
  return(tabla_eval)
}

status_colors <- c("07BEB8", "3DCCC7", "68D8D6", "9CEAEF", "C4FFF9", "EECF6D", "D5AC4E", "8B6220", "720E07", "45050C", "56494E", "A29C9B", "8A8E91")
status_levels <- unique(evolucion$cambio)

gen_evoprog <- function(anio_sel, ent_sel, pp_sel){
  
  id <- evolucion %>%
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique() %>%
    sort()

  if (length(id) == 0) {
    return(NULL)
  }

  id <- id[1]
   
  evol_tl<-evolucion %>% 
    filter(id_if == id) %>%
    mutate(as.numeric(año_fin)) %>%
    data.frame(year = ciclo, 
               milestone = cambio,
               status = cambio,
               direction = rep(c(1,-1), count(cambio))) %>% 
    mutate(
      status = factor(status, levels = status_levels),
      position = sample(c((1:5))/20, count(cambio), replace = TRUE)*direction,
      text_position = 0.03*direction + position
    )
  
  ggplot(evol_tl, aes(x = year, y = 0, col = status, label = milestone)) +
    labs(col = "Cambio programático") +
    scale_color_manual(values = status_colors,
                       labels = status_levels, 
                       drop = FALSE) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "black", size = 0.3) +
    geom_segment(data = evol_tl, aes(y = position, yend = 0, xend = year),
                 color = "black", size = 0.2) +
    geom_point(data = evol_tl, aes(y = 0), size = 3) +
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x =element_blank(),
          axis.ticks.x =element_blank(),
          axis.line.x =element_blank(),
          legend.position = "bottom") +
    geom_text(aes(x = year, y = -0.03, label = year, fontface = "bold"), size = 3, color = "black") +
    geom_text_repel(data = milestonedf, aes(y = text_position,label = milestone),size = 3.2, point.size = NA)  +
    scale_x_continuous(expand = expansion(mult = 0.1))



}


# Poblaciones ####

## Value boxes ####
### Cobertura con gráfico y valor del año en curso
### Eficacia con gráfico y valor del año en curso

## Funciones ####
unidad_pp <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(pp_unidad) %>% 
    unique() %>% 
    sort()
}

def_pp <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(pp_definicion) %>% 
    unique() %>% 
    sort()
}

unidad_po <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(po_unidad) %>% 
    unique() %>% 
    sort()
}

def_po <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(po_definicion) %>% 
    unique() %>% 
    sort()
}

unidad_pa <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(pa_unidad) %>% 
    unique() %>% 
    sort()
}

def_pa <- function(anio_sel, ent_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(pa_definicion) %>% 
    unique() %>% 
    sort()
}

cobertura <- function(anio_sel, ent_sel, pp_sel){
  numero <- inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(cobertura) %>% 
    unique() %>% 
    sort()
}

eficiencia <- function(anio_sel, ent_sel, pp_sel){
  numero <- inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(eficiencia) %>% 
    unique() %>% 
    sort()
}


grafica_poblaciones <- function(inv_fed, anio_sel, ent_sel, pp_sel) {
  id <- inv_fed %>%
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique()
  
  if(length(id) == 0) {
    return(NULL)
  }

  df_filtrado <- inv_fed %>%
    filter(id_if %in% id) %>%
    select(ciclo, pp_cantidad, po_cantidad, pa_cantidad) %>%
    pivot_longer(cols = c(pp_cantidad, po_cantidad, pa_cantidad),
                 names_to = "variable", values_to = "cantidad")
  
  if(nrow(df_filtrado) == 0) {
    return(NULL)
  }

  
  ggplot(df_filtrado, aes(x = ciclo, y = cantidad, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("pp_cantidad" = "#1f77b4", "po_cantidad" = "#ff7f0e", "pa_cantidad" = "#2ca02c")) +
    labs(title = paste("Serie histórica de poblaciones de", pp_sel),
         x = "Ciclo",
         y = "Cantidad",
         fill = "Variable") +
    theme_minimal()
}

tabla_pob <- function(anio_sel, ent_sel, pp_sel){
  id <- inv_fed %>%
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique()
  
  if(length(id) == 0) {
    return(NULL)
  }
  
  inv_fed %>%
    filter(id_if %in% id) %>%
    select(ciclo, nombre, pp_cantidad, po_cantidad, pa_cantidad) %>%
    rename("Año" = ciclo,
           "Programa o acción federal" = nombre,
           "Población potencial" = pp_cantidad,
           "Población objetivo" = po_cantidad,
           "Población atendida" = pa_cantidad) 
#   %>% 
#   options = list(pageLength = 50,
#                  searching = FALSE,
#                  language = list(
#                    paginate = list(previous = "Anterior", next = "Siguiente"),
#                    info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
#                    infoEmpty = "Mostrando 0 a 0 de 0 registros",
#                    lengthMenu = "Mostar _MENU_ registros por página",
#                    loadingRecords = "Cargando...",
#                    emptyTable = "No hay datos disponibles"
#                    )
#                  ) %>% 
#   rownames = FALSE %>% 
#     formatRound(c("Población potencial", "Población objetivo", "Población atendida"), 
#                  digits = 0, mark = ",") %>% 
#     formatStyle(
#       columns = c("Población potencial", "Población objetivo", "Población atendida"),
#       valueColumns = c("Población potencial", "Población objetivo", "Población atendida"),
#       backgroundColor = styleEqual(NA, "lightgray"),
#       color = styleEqual(NA, "gray")
#     ) %>% 
#     formatStyle(
#       columns = c("Población potencial", "Población objetivo", "Población atendida"),
#       `text-align` = "center"
#     ) 
}



# Gráficos de cobertura y eficiencia


# Presupuesto ####
grafica_presupuesto <- function(inv_fed, anio_sel, ent_sel, pp_sel) {
  id <- inv_fed %>%
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique()
  
  if(length(id) == 0) {
    return(NULL)
  }
  
  df_filtrado <- inv_fed %>%
    filter(id_if %in% id) %>%
    select(ciclo, p_aprobado, p_ejercido) %>%
    pivot_longer(cols = c(p_aprobado, p_ejercido),
                 names_to = "variable", values_to = "cantidad")
  
  ggplot(df_filtrado, aes(x = ciclo, y = cantidad, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("p_aprobado" = "#1f77b4", "p_ejercido" = "#ff7f0e")) +
    labs(title = paste("Serie histórica de presupuesto de", pp_sel),
         x = "Ciclo",
         y = "Presupuesto",
         fill = "Variable") +
    theme_minimal()
}

porcentaje_pres <- function(anio_sel, ent_sel, pp_sel){
  total_ejercido <- inv_fed %>% 
    filter(ciclo == anio_sel) %>% 
    pull(p_ejercido) %>% 
    sum(na.rm = T)
  
  pp_ejercido <- inv_fed %>% 
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>% 
    pull(p_ejercido) %>% 
    sum(na.rm = TRUE)
  
  if(total_ejercido == 0 || is.na(pp_ejercido)) {
    return("Información no disponible")
  }
  
  porcentaje <- percent(pp_ejercido/total_ejercido, accuracy = 0.1)
  
  return(porcentaje)
}

tabla_pres <- function(anio_sel, ent_sel, pp_sel){
  id <- inv_fed %>%
    filter(ciclo == anio_sel, entidad == ent_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique()
  
  if(length(id) == 0) {
    return(NULL)
  }
  
  datatable(inv_fed %>%
              filter(id_if %in% id) %>%
              select(ciclo, nombre, p_aprobado, p_ejercido),
            options = list(pageLength = 10))
}



## Tarjetas y value boxes####
### Value box con gráfico de variaciones en el presupuesto y valor del año en curso vs el anterior

## Gráficos ####
### Mapa a nivel federal con base de subsidios por municipio
### Gráfico de líneas con presupuesto aprobado vs ejercido con botón para deflactor