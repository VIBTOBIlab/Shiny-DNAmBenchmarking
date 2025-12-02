################################################################################
# home_static.R – Welcome screen for DecoNFlow Benchmarking Shiny App
# This module is part of the DecoNFlow Shiny app
################################################################################

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
    br(), 
    #h3("Citation"),

    tags$blockquote(class = "paper-cite",
                    tags$p(
                      class = "title",
                      "A benchmark of DNA methylation deconvolution methods for tumoral fraction estimation using DecoNFlow"
                    ),
                    tags$p(
                      class = "authors",
                      paste(
                        "Edoardo Giuili, Sofie Van de Velde, Sam Kint, Maísa R Ferro dos Santos, Lotte Cornelli, Sofie Roelandt, Kathleen Schoofs, Renske Imschoot, Ruben Van Paemel, Leander Meuris, Celine Everaert, Katleen De Preter"
                      )
                    ),
                    tags$p(
                      class = "doi",
                      "bioRxiv 2025 Nov 27. doi: ",
                      tags$a(
                        href = "https://www.biorxiv.org/content/10.1101/2025.11.27.688590v1", 
                        target = "_blank", 
                        rel = "noopener", 
                        "10.1101/2025.11.27.688590")
                    )
    )
    
  )
} # end homeTabUI

# Home Tab Server
homeTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
})
} # end homeTabServer
