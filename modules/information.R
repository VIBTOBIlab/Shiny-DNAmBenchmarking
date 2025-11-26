################################################################################
# information.R – Info about used DMRtools and Deconvolution tools for DecoNFlow Benchmarking Shiny App
# This module is part of the DecoNFlow Shiny app
################################################################################

# Information Tab UI
informationTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Information",

    # Deconvolution tools Section
    h3("Deconvolution tools", style = "font-weight: bold;", id="shiny-tab-Information" ),
    div(
      style = "margin-bottom: 40px;",
      DTOutput(ns("deconvtools_table"))
    ),
  
    # DMR tools Section
    h3("DMR tools", style = "font-weight: bold;"),
    div(
      style = "margin-bottom: 20px;",
      DTOutput(ns("dmrtools_table"))
    ),
    ## End
  )
}

# Information Tab server
informationTabServer <- function(id) {
 moduleServer(id, function(input, output, session) {

   # deconvolution tools information table 
   output$deconvtools_table <- renderDT({
     datatable(deconvtools_data %>% select(-GitHubnames),  # Exclude the column
               escape = FALSE,  # Important for clickable links
               options = list(pageLength = 12,
                              autoWidth = TRUE,
                              scrollX = TRUE,
                              searchHighlight = TRUE
                              ))
     
   })
   
   # dmr tools information table 
   output$dmrtools_table <- renderDT({
     datatable(dmrtools_data,
               escape = FALSE,  # Important for clickable links
               options = list(pageLength = 3,
                              autoWidth = TRUE,
                              scrollX = TRUE,
                              searchHighlight = TRUE
               ))
     
   })   
   
   
    
   }) # Close moduleServer
 } # Close contactTabServer    
    
    
    
    
    