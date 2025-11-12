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
          src = "funky.png",
          style = "max-width: 80%; height: auto; display: block; margin-left: auto; margin-right: auto;"
          )
        )
      ),
    br(), #h3("Citation"),

    # tags$blockquote(class = "paper-cite",
    #                 tags$p(
    #                   class = "title",
    #                   "XXX"
    #                 ),
    #                 tags$p(
    #                   class = "authors",
    #                   paste(
    #                     "XXX"
    #                   )
    #                 ),
    #                 tags$p(
    #                   class = "doi",
    #                   "doi: ",
    #                   tags$a(href = "XXX", target = "_blank", rel = "noopener", "XXXX")
    #                 )
    # )
    
  )
}



# Home Tab Server
homeTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server-side logic for Home tab if needed (currently none)
})
}
