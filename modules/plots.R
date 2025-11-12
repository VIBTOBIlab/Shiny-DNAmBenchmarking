# ==============================================================================
# plots.R – UI and Server logic for Plot tab
# This module is part of the DecoNFlow Shiny app
# ==============================================================================

# Source modules
source("modules/wgbs_tt.R")
source("modules/rrbs_tt.R")
source("modules/rrbs_cl.R")

plotsTabUI <- function(id) {
  ns <- NS(id)
  navbarMenu(
    "Plots",
    wgbsttTabUI(ns("WGBS-TT"), label = "WGBS-TT"),
    rrbsttTabUI(ns("RRBS-TT"), label = "RRBS-TT"),
    rrbsclTabUI(ns("RRBS-CL"), label = "RRBS-CL")
  ) #Close navbarMenu
  
} # Close metricsTabUI


plotsTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {


    ## 1. Import Data and Preprocessing
    # Code moved to global.R
    
    ## 2. Functions
    # Code moved to global.R
    
    # Extra modules
    wgbsttTabServer("WGBS-TT")
    rrbsttTabServer("RRBS-TT")
    rrbsclTabServer("RRBS-CL")

  }) # Close moduleServer
} # Close metricsTabServer    
    
    
    
    