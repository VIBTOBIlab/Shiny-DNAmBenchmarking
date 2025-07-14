# Source rrbs module
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
    # RMSE
    rmse <- function(actual, predicted) {
      round(sqrt(mean((actual - predicted)^2)),4)
    }
    # Spearman's rank correlation coefficient (SCC)
    scc <- function(actual, predicted) {
      cor(actual, predicted, method = "spearman")
    }
    
    roc.obj <- function(true_labels, predicted_scores) {
      true_labels[true_labels>0] <- 1
      roc_obj <- roc(true_labels, predicted_scores)
      return(roc_obj)
    }
    
    # Extra modules
    rrbsTabServer("RRBS")  
    wgbsTabServer("WGBS")
      
       
  }) # Close moduleServer
} # Close metricsTabServer    
    
    
    
    
    