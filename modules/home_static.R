# Home Tab UI
homeTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Home",
      h2(HTML("<b>Welcome to DecoNFlow Benchmarking</b>"), style = "text-align: center;"),

      br(),br(),
      div(h3("RRBS"), style = "text-align: center;"),  

      fluidRow(
        column(
          width = 12,
          align = "center",
          tags$img(
            src = "rank_static_homepage_RRBS_2025-06-27.svg", #"rank_static_homepage_2025-05-09.svg", 
            style = "max-width: 50%; height: auto; display: block; margin-left: auto; margin-right: auto;"
          )
        )
      ),
      
      br(), br(),
      div(h3("WGBS"), style = "text-align: center;"),  
      
      fluidRow(
        column(
          width = 12,
          align = "center",
          tags$img(
            src = "rank_static_homepage_WGBS_2025-06-27.svg", #"rank_static_homepage_2025-05-09.svg", 
            style = "max-width: 50%; height: auto; display: block; margin-left: auto; margin-right: auto;"
          )
        )
      ),
      
      
      br(), br(),br(), h3("Citation"),
      
      # footer
      footer_citation(),
      
      # Optional: Replace with CSS-based scrolling behavior if needed
    lapply(1:100, function(x) br()),
    spsGoTop("default")
    
  )
}



# Home Tab Server
homeTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server-side logic for Home tab if needed (currently none)
    

    
  })
}
