# Home Tab UI
homeTabUI <- function(id) {
  ns <- NS(id)  
  tabPanel(
    "Home",
    fluidPage(
      h2(HTML("<b>Welcome to DecoNFlow Benchmarking</b>"), style = "text-align: center;"),
      br(),
      br(),
      br(),
      
      # Add the animated .svg
      tags$div(
        style = "text-align: left;",
        tags$object(
          data = "decoNFlow_animated.map.svg",  # Relative path from the www folder
          type = "image/svg+xml", 
          width = "80%", height = "400px",
          style = "display: block;"
        )
      ),
      
      br(),
      
      h3("Citation"),
      br(),br(),

      tags$hr(),
      
      # Wrap Contact Us and Contributors in a centered div
      div(
        style = "text-align: center; margin-top: 20px;",

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
        )
        )
      ),
    
      # Go to top of the page
      lapply(1: 100, function(x) br()),
      spsGoTop("default")
    )
}


# Home Tab Server
homeTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server-side logic for Home tab if needed (currently none)
  })
}
