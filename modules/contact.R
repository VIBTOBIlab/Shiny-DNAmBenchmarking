# Contact Tab UI
contactTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Contact",
    # Wrap Contact Us and Contributors in a centered div
    div(
      style = "text-align: center;",
      
      # Contact Section
      h3("Contact Us", style = "font-weight: bold;"),
      
      # Social media icons or email
      tags$div(
        style = "margin-top: 10px; text-align: center;",
        tags$a(
          href = "https://github.com/VIBTOBIlab/Shiny-DNAmBenchmarking",
          HTML("<i class='fa fa-github'></i> GitHub"),
          style = "text-decoration: none; color: #ec642c;"
        ),
        tags$a(
          href = "https://depreterlab.sites.vib.be/en#/",
          HTML("<i class='fa fa-link'></i> TOBI Website"),
          style = "margin-left: 10px; text-decoration: none; color: #ec642c;"
        ),
        tags$a(
          href = "mailto:sofvdvel.vandevelde@ugent.be",
          HTML("<i class='fa fa-envelope'></i> Email"),
          style = "margin-left: 10px; text-decoration: none; color: #ec642c;"
        )
      ),
      
      br(), br(),
      
      # Contributors Section
      h3("Contributors", style = "font-weight: bold;"),
      
      # List of contributors
      tags$div(
        style = "margin-top: 10px; text-align: center;",
        tags$p(
          style = "text-align: center;font-size: 14px;",
          HTML("<b>Shiny App</b><br>Sofie Van de Velde")
        ),
        tags$p(
          style = "text-align: center;font-size: 14px;",
          HTML("<b>R Code</b><br>Edoardo Giuili")
        ),
        tags$p(
          style = "text-align: center; font-size: 14px;",
          HTML("<b>Supervision</b><br>Prof. Dr. Ir. Katleen De Preter<br> Prof. Dr. Celine Everaert")
        )
      ),
      
      # Logos in 3-2 layout, centered
      tags$div(
        style = "margin: 30px auto; max-width: 700px;",
        
        # First row: bottom-aligned
        tags$div(
          style = "display: flex; justify-content: center; align-items: flex-end; gap: 30px; margin-bottom: 20px;",
          tags$img(src = "VIB_logo.png", style = "max-height: 80px; width: auto; height: auto;"),    
          tags$img(src = "FWO_logo.png", style = "max-height: 48px; width: auto; height: auto;"),
          tags$img(src = "KOTK_logo.png", style = "max-height: 72px; width: auto; height: auto;")
        ),
        
        # Second row: top-aligned
        tags$div(
          style = "display: flex; justify-content: center; align-items: flex-start; gap: 18px;",
          tags$img(src = "ERC_logo.png", style = "max-height: 100px; width: auto; height: auto;"),
          tags$img(src = "UGhent_logo.png", style = "max-height: 100px; width: auto; height: auto;")
        )
    )
    )
    
    
 )   ## End
}


contactTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    

  }) # Close moduleServer
} # Close contactTabServer    
    
    
    
    
    