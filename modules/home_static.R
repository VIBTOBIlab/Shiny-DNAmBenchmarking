# Home Tab UI
homeTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Home",
    h2(
      HTML("<b>Welcome to DecoNFlow Benchmarking</b>"),
      class = "welcome-title",
      style = "text-align: center;"),

    fluidRow(
      class = "funkyheatmap",
      column(
        width = 12,
        align = "center",
        tags$img(
          src = "funkyheatmap_cropped.png",
          style = "max-width: 90%; height: auto; display: block; margin-left: auto; margin-right: auto;"
          )
        )
      ),
  
    br(), br(), br(), h3("Citation")

  )
}



# Home Tab Server
homeTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server-side logic for Home tab if needed (currently none)
    

    
  })
}
