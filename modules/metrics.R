# Source rrbs module
source("modules/rrbs.R")
source("modules/wgbs.R")

metricsTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Metrics",
    fluidPage(
      h2("Benchmarking Metrics"),
      p("We considered 3 different key metrics: the root-mean-squared error (RMSE), the area under the curve (AUC-ROC) and the Spearman's rank correlation coefficient (ρ). To create an overall benchmarking score against which to compare the deconvolution tools, we min-max scaled the metrics and computed the geometric mean of the three metrics to obtain the final benchmarking scores. Finally, we ranked the tools based on these scores."),
      p("Below, you can find the computed metrics and visualizations."),
      br(),
      tabsetPanel(
      rrbsTabUI(ns("RRBS") ),
      wgbsTabUI(ns("WGBS") )
      
      ),# Close tabsetPanel
      
      # Go to top of the page
      lapply(1:100, function(x) br()),
      spsGoTop("default")
    )# Close fluidPage
  ) # close TabPanel 
  
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
    
    
    
    
    