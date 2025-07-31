# Metrics Tab UI
informationTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Information",

    # Deconvolution tools Section
    h3("Deconvolution tools", style = "font-weight: bold;", id="shiny-tab-Information" ),
    p("Include information..."),
    br(),br(),
    
    # DMR tools Section
    h3("DMR tools", style = "font-weight: bold;"),
    p("Include information...")
    
    ## End
  )
}


informationTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  }) # Close moduleServer
} # Close contactTabServer    
    
    
    
    
    