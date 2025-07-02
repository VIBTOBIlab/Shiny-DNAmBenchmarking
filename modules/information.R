# Metrics Tab UI
informationTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Information",
    title = "Information",
    value = "Information",

    # Deconvolution tools Section
    h3("Deconvolution tools", style = "font-weight: bold; color: #343a40;", id="shiny-tab-Information" ),
    p("Include information..."),
    br(),br(),
    
    
    
    # DMR tools Section
    h3("DMR tools", style = "font-weight: bold; color: #343a40;"),
    p("Include information..."),


    
    # Go to top of the page
    lapply(1: 100, function(x) br()),
    spsGoTop("default")
  )
}


informationTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  }) # Close moduleServer
} # Close contactTabServer    
    
    
    
    
    