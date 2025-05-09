# Home Tab UI
homeTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Home",
    fluidPage(
      h2(HTML("<b>Welcome to DecoNFlow Benchmarking</b>"), style = "text-align: center;"),
      br(), br(),
      
      fluidRow(
        column(
          width = 12,
          align = "center",
          tags$img(
            src = "rank_static_homepage_2025-05-09.svg", 
            style = "max-width: 50%; height: auto; display: block; margin-left: auto; margin-right: auto;"
          )
        )
      ),
      
      br(), h3("Citation"), br(), br(), tags$hr(),
      
      div(
        style = "text-align: center; margin-top: 20px;",
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
      ),
      
      # Optional: Replace with CSS-based scrolling behavior if needed
      br(), br(), spsGoTop("default")
    )
  )
}



# Home Tab Server
homeTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server-side logic for Home tab if needed (currently none)
    

    
  })
}
