# Tarjetas de datos generales ####
id_cards <- list(
  card(
    full_screen = TRUE,
    card_header("Descripción"),
    textOutput("descripcion")
  ),
  card(
    full_screen = TRUE,
    card_header("Normatividad"),
    textOutput("normatividad")
  ),
  card(
    full_screen = TRUE,
    card_header("Apoyos que entrega"),
    textOutput("apoyos")
  )
)

# Value boxes de datos generales ####
id_vb<- list(
  value_box(
    title = "Año de inicio", 
    value = "2024", 
    showcase = bsicons::bs_icon("calendar-event")
  ),
  value_box(
    title = "Porcentaje del presupuesto de desarrollo social", 
    value = scales::unit_format(unit = "%")(0.3), 
    showcase = bsicons::bs_icon("opencollective")
  ),
  value_box(
    title = "Variación del presupuesto", 
    value = scales::unit_format(unit = "%")(6.0), 
    showcase = bsicons::bs_icon("plus-slash-minus")
  )
)
