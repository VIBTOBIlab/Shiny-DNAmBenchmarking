# Metrics Tab UI
contactTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Contact",
    # Wrap Contact Us and Contributors in a centered div
    div(
      style = "text-align: center; margin-top: 20px;",
      
      # Contact Section
      h3("Contact Us", style = "font-weight: bold; color: #343a40;"),
      
      # Social media icons or email
      tags$div(
        style = "margin-top: 10px; text-align: center",
        tags$a(
          href = "https://github.com/VIBTOBIlab",
          HTML("<i class='fa fa-github'></i> GitHub"),
          style = "text-decoration: none; color: #555;"
        ),
        tags$a(
          href = "https://depreterlab.sites.vib.be/en#/",
          HTML("<i class='fa fa-link'></i> TOBI Website"),
          style = "margin-left: 10px; text-decoration: none; color: #555;"
        ),
        tags$a(
          href = "mailto:sofvdvel.vandevelde@ugent.be",
          HTML("<i class='fa fa-envelope'></i> Email"),
          style = "margin-left: 10px; text-decoration: none; color: #555;"
        )
      ),
      
      br(),
      # Contributors Section
      h3("Contributors", style = "font-weight: bold; color: #343a40;"),
      
      # List of contributors
      tags$div(
        style = "margin-top: 10px; text-align: center;",
        tags$p(
          style = "color: #555; text-align: center;font-size: 12px;",
          HTML("<b>Shiny App</b><br>Sofie Van de Velde")
        ),
        tags$p(
          style = "color: #555; text-align: center;font-size: 12px;",
          HTML("<b>R Code</b><br>Edoardo Giuili<br>Maísa Renata Ferro Dos Santos")
        ),
        tags$p(
          style = "color: #555; text-align: center; font-size: 12px;",
          HTML("<b>Supervision</b><br>prof. Katleen De Preter<br> Dr. Celine Everaert")
        )
      )
    ),
    
    # Go to top of the page
    lapply(1: 100, function(x) br()),
    spsGoTop("default")
  )
}


contactTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    

  }) # Close moduleServer
} # Close contactTabServer    
    
    
    
    
    