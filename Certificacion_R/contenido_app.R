# Base de datos ####
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(bslib)

setwd("C:/Users/mitrejo/Mi unidad/SPPC/Github/curso_r_2024/Certificacion_R")

inv_fed <- read_excel("inv_fed.xlsx", sheet = "inv_fed") %>% 
  mutate(
    modcve = paste0(mod, "-", cve),
    ciclo  = factor(ciclo),
    pp_cantidad = as.numeric(pp_cantidad),
    po_cantidad = as.numeric(po_cantidad),
    pa_cantidad = as.numeric(pa_cantidad),
    p_aprobado = as.numeric(p_aprobado),
    p_ejercido = as.numeric(p_ejercido)
  )
  
evaluaciones <- read_excel("inv_fed.xlsx", sheet = "evaluaciones")  

evolucion <- read_excel("inv_fed.xlsx", sheet = "evolucion") 

## Funciones ####
anio_sel = 2022
ramos_disp <- function(anio_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel) %>% 
    pull(ramo) %>% 
    unique() %>% 
    sort()
}

pp_disp <- function(anio_sel, ramo_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel) %>% 
    pull(nombre) %>% 
    unique() %>% 
    sort()
}

# UI ####
## Datos generales ####
datosgrales_card <- card(
  card_header(
    class = "bg-dark",
    "Datos generales"),
  height = NULL,
  full_screen = FALSE,
  accordion_panel(
    title = "Descripción",
    textOutput("descr_ac")
  ),
  accordion_panel(
    title = "Derecho social asociado",
    textOutput("derecho_ac")
  ),
  accordion_panel(
    title = "Normatividad principal",
    textOutput("Normatividad y link")
  ),
  accordion_panel(
    title = "Matriz de Indicadores para Resultados",
    tableOutput("tabla_mir")
  ),
  accordion_panel(
    title = "Evaluaciones realizadas al programa",
    tableOutput("tabla_eval")
  )
)

## Poblaciones ####
poblacion_card <- navset_card_tab(
  full_screen = T,
  title = "Poblaciones",
  nav_panel(
    "Cuantificación",
    card_title("Evolución de las poblaciones del programa"), 
    plotOutput("graf_pob")
  ),
  nav_panel(
    "Definiciones",
    card(
      height = NULL,
      full_screen = FALSE,
      accordion_panel(
        title = "Población potencial",
        textOutput("defpp_ac")
      ),
      accordion_panel(
        title = "Población objetivo",
        textOutput("defpo_ac")
      ),
      accordion_panel(
        title = "Población atendida",
        textOutput("defpa_ac")
      )
    )
  ),
  nav_panel(
    shiny::icon("circle-info"),
    markdown("Información")
  )
)

## Presupuesto ####
presupuesto_card <- card(
  card_header(
    class = "bg-dark",
    "Presupuesto"),
  height = NULL,
  full_screen = FALSE,
  plotOutput("graf_pres")
    )

# Datos generales ####


## Funciones ####

ramo <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(ramo) %>% 
    unique() %>% 
    sort()
}

modcve <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(modcve) %>% 
    unique() %>% 
    sort()
}

nombre <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(nombre) %>% 
    unique() %>% 
    sort()
}

ainicio <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(inicio) %>% 
    unique() %>% 
    sort()
}

descripcion <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(descripcion) %>% 
    unique() %>% 
    sort()
}

derecho <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(derecho_directo) %>% 
    unique() %>% 
    sort()
}

etapav <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(etapa_vida) %>% 
    unique() %>% 
    sort()
}

gpoatn <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(gpo_atencion) %>% 
    unique() %>% 
    sort()
}

apoyos <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(nombre) %>% 
    unique() %>% 
    sort()
}

normatividad <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(normatividad) %>% 
    unique() %>% 
    sort()
}

norma_v <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(normatividad_v) %>% 
    unique() %>% 
    sort()
}

fin <- function(anio_sel, ramo_sel, pp_sel){
  resultado <- inv_fed %>%
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(fin) %>% 
    unique() %>% 
    sort()
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}

proposito <- function(anio_sel, ramo_sel, pp_sel){
  resultado <- inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(proposito) %>% 
    unique() %>% 
    sort()
  
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}


componentes <- function(anio_sel, ramo_sel, pp_sel){
  resultado <- inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(componente) %>% 
    unique() %>% 
    sort()
  
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}

actividades <- function(anio_sel, ramo_sel, pp_sel){
  resultado <- inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
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

anio_eval <- function(anio_sel, ramo_sel, pp_sel){
  id <- evaluaciones %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
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
    pull(ciclo) %>% 
    sort()
  
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}

titulo_eval <- function(anio_sel, ramo_sel, pp_sel){
  id <- evaluaciones %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
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
    pull(titulo) %>% 
    sort()
  
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}

vinculo_eval <- function(anio_sel, ramo_sel, pp_sel){
  id <- evaluaciones %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
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
    pull(vinculo) %>% 
    sort()
  
  if(length(resultado) == 0) {
    resultado <- "Información no disponible"
  }
  
  return(resultado)
}

gen_eval <- function(anio_eval, titulo_eval, vinculo_eval) {
  
  tabla_eval <- data.frame(
    Año = anio_eval,
    'Título de la evaluación' = titulo_eval,
    'Vínculo a la evaluación' = vinculo_eval,
    stringsAsFactors = FALSE 
  )
  
  return(tabla_eval)
}

# Poblaciones ####

## Value boxes ####
### Cobertura con gráfico y valor del año en curso
### Eficacia con gráfico y valor del año en curso

## Funciones ####
unidad_pp <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(pp_unidad) %>% 
    unique() %>% 
    sort()
}

def_pp <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(pp_definicion) %>% 
    unique() %>% 
    sort()
}

unidad_po <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(po_unidad) %>% 
    unique() %>% 
    sort()
}

def_po <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(po_definicion) %>% 
    unique() %>% 
    sort()
}

unidad_pa <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(pa_unidad) %>% 
    unique() %>% 
    sort()
}

def_pa <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(pa_definicion) %>% 
    unique() %>% 
    sort()
}

cobertura <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(cobertura) %>% 
    unique() %>% 
    sort()
}

eficiencia <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(eficiencia) %>% 
    unique() %>% 
    sort()
}


grafica_poblaciones <- function(inv_fed, ciclo_sel, ramo_sel, pp_sel) {
  id <- inv_fed %>%
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique()
  
  if(length(id) == 0) {
    return(NULL)
  }

  df_filtrado <- inv_fed %>%
    filter(id_if %in% id) %>%
    select(ciclo, pp_cantidad, po_cantidad, pa_cantidad) %>%
    gather(key = "variable", value = "cantidad", pp_cantidad, po_cantidad, pa_cantidad)
  
  ggplot(df_filtrado, aes(x = ciclo, y = cantidad, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("pp_cantidad" = "#1f77b4", "po_cantidad" = "#ff7f0e", "pa_cantidad" = "#2ca02c")) +
    labs(title = paste("Serie histórica de poblaciones de", pp_sel),
         x = "Ciclo",
         y = "Cantidad",
         fill = "Variable") +
    theme_minimal()
}

# Gráficos de cobertura y eficiencia


# Presupuesto ####
grafica_presupuesto <- function(inv_fed, ciclo_sel, ramo_sel, pp_sel) {
  id <- inv_fed %>%
    filter(ciclo == anio_sel, ramo == ramo_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique()
  
  if(length(id) == 0) {
    return(NULL)
  }
  
  df_filtrado <- inv_fed %>%
    filter(id_if %in% id) %>%
    select(ciclo, p_aprobado, p_ejercido) %>%
    gather(key = "variable", value = "cantidad", p_aprobado, p_ejercido)
  
  ggplot(df_filtrado, aes(x = ciclo, y = cantidad, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("p_aprobado" = "#1f77b4", "p_ejercido" = "#ff7f0e")) +
    labs(title = paste("Serie histórica de presupuesto de", pp_sel),
         x = "Ciclo",
         y = "Presupuesto",
         fill = "Variable") +
    theme_minimal()
}

ejercido_ciclo <- function(anio_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel) %>% 
    pull(p_ejercido) %>% 
    sum(na.rm = T)
}
## Tarjetas y value boxes####
### Tarjeta con porcentaje del presupuesto social
### Value box con gráfico de variaciones en el presupuesto y valor del año en curso vs el anterior

## Gráficos ####
### Mapa a nivel federal con base de subsidios por municipio
### Gráfico de líneas con presupuesto aprobado vs ejercido con botón para deflactor