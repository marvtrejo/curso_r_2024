library(shiny)
library(bslib)

# Define UI for random distribution app ----
# Sidebar layout with input and output definitions ----
ui <- page_sidebar(
  
  # App title ----
  title ="Tabsets",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    # Input: Select the random distribution type ----
    radioButtons("dist", "Distribution type:",
                 c("Normal" = "norm",
                   "Uniform" = "unif",
                   "Log-normal" = "lnorm",
                   "Exponential" = "exp")),
    # br() element to introduce extra vertical spacing ----
    br(),
    # Input: Slider for the number of observations to generate ----
    sliderInput("n",
                "Number of observations:",
                value = 500,
                min = 1,
                max = 1000)
  ),
  
  # Main panel for displaying outputs ----
  # Output: A tabset that combines three panels ----
  navset_card_underline(
    title = "Visualizations",
    # Panel with plot ----
    nav_panel("Plot", plotOutput("plot")),
    
    # Panel with summary ----
    nav_panel("Summary", verbatimTextOutput("summary")),
    
    # Panel with table ----
    nav_panel("Table", tableOutput("table"))
  )
)