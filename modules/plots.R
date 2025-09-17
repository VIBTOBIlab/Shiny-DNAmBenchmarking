# ==============================================================================
# plots.R – UI and Server logic for Plot tab
# This module is part of the DecoNFlow Shiny app
# ==============================================================================

# Source modules
source("modules/cfrrbs_insilico.R")
source("modules/cfrrbs_invitro.R")
source("modules/rrbs.R")
source("modules/wgbs.R")

plotsTabUI <- function(id) {
  ns <- NS(id)
  navbarMenu(
    "Plots",
    cfrrbsinsilicoTabUI(ns("cfRRBS_in_silico"), label = "cfRRBS in-silico"),
    cfrrbsinvitroTabUI(ns("cfRRBS_in_vitro"), label = "cfRRBS in-vitro"),
    rrbsTabUI(ns("RRBS"), label = "RRBS"),
    wgbsTabUI(ns("WGBS"), label = "WGBS")
  ) #Close navbarMenu
  
} # Close metricsTabUI


plotsTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {


    ## 1. Import Data and Preprocessing
    # Code moved to global.R
    
    ## 2. Functions
    # Code moved to global.R
    
    # Extra modules
    cfrrbsinsilicoTabServer("cfRRBS_in_silico")
    cfrrbsinvitroTabServer("cfRRBS_in_vitro")
    rrbsTabServer("RRBS")
    wgbsTabServer("WGBS")
      
       
  }) # Close moduleServer
} # Close metricsTabServer    
    
    
    
    