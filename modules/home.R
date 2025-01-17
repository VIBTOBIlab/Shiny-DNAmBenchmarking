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
        style = "text-align: center;",
        tags$object(
          data = "decoNFlow_animated.map.svg",  # Relative path from the www folder
          type = "image/svg+xml", 
          width = "80%", height = "400px",
          style = "display: block;"
        )
      ),
      
      # Add horizontal line above Citation
      tags$hr(),
      h3("Citation"),
      # Go to top of the page
      lapply(1: 100, function(x) br()),
      spsGoTop("default")
      
    )
  )
}



# Home Tab Server
homeTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server-side logic for Home tab if needed (currently none)
  })
}
