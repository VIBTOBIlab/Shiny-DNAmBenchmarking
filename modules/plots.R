# ==============================================================================
# metrics.R – UI and Server logic for Plot tab
# This module is part of the DecoNFlow Shiny app
# ==============================================================================

# Source modules
source("modules/rrbs.R")
source("modules/wgbs.R")

metricsTabUI <- function(id) {
  ns <- NS(id)
  navbarMenu(
    "Plots",
    rrbsTabUI(ns("RRBS")),
    wgbsTabUI(ns("WGBS"))
  ) #Close navbarMenue
  
} # Close metricsTabUI


metricsTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {


    ## 1. Import Data and Preprocessing
    # Code moved to global.R
    
    ## 2. Functions
    # Code moved to global.R
    
    # Extra modules
    rrbsTabServer("RRBS")
    wgbsTabServer("WGBS")
      
       
  }) # Close moduleServer
} # Close metricsTabServer    
    
    
    
    