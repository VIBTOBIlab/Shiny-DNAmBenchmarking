####################################################################################
# This is a Shiny web application for DecoNFlow Benchmarking
# More information on the pipeline: https://github.ugent.be/DePreterLab
####################################################################################

# Install packages
# install.packages(c(
#   "tidyverse", "plotly", "shiny", "lubridate", "dplyr", "pROC",
#   "philentropy", "Metrics", "reshape2", "htmltools", "bslib",
#   "stringr", "funkyheatmap", "spsComps", "conflicted", "LaplacesDemon",
#   "shinytoastr", "cowplot"
# ))

# Set library packages directory
readRenviron(".Renviron")
.libPaths(c(path.expand(Sys.getenv("R_LIBS_USER")), .libPaths()))
print(.libPaths())
#.libPaths("/mnt/c/Users/Sofie/OneDrive - UGent/Documents/Projects/DecoNFlow_Benchmarking/4.3.1")
#.libPaths("~/Projects/DecoNFlow_Benchmarking/4.3.1")

#find.package("shiny") 
# Load necessary libraries
#library(R6)
library(shiny)
library(bslib)
library(reshape2)
library(plotly) 
library(philentropy)
library(LaplacesDemon)
library(Metrics)
#library(funkyheatmap) 
#library(tidyverse)  # Includes dplyr, ggplot2, stringr, etc.
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(forcats)
library(pROC)
library(spsComps) 
library(conflicted)
library(htmltools)
library(quarto)
library(sever)

#library(funkyheatmap)
#library(tidyverse)  # Includes dplyr, ggplot2, stringr, etc.

# Source the module files
source("modules/home.R")
source("modules/metrics.R")
source("modules/contact.R")

#Source other files
source("global.R")

# Extra settings
options(shiny.maxRequestSize=50*1024^2) # Increase limit to 50MB
conflict_prefer("filter", "dplyr")   # Prefers dplyr::filter() over stats::filter()
conflict_prefer("auc", "pROC")       # Prefers pROC::auc() over Metrics::auc()  
conflict_prefer("layout","plotly") # Prefers plotly::layout() over graphics::layout()

# UI
ui <- navbarPage(

  # App title
  title = "DecoNFlow Benchmarking",
  
  # Include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Add modules
  homeTabUI("home"),  
  metricsTabUI("metrics"),
  contactTabUI("contact"),
  
  # tags$footer(
  #   class = "footer",
  #   tags$img(src = "VIB.png", alt = "Logo", title = "VIB")
  # ) 
  
)



# Server
server <- function(input, output, session) {
  # Activate module tab servers
  homeTabServer("home")
  metricsTabServer("metrics")
  contactTabServer("contact")
  #session$onSessionEnded(stopApp) # Automatically stop a Shiny app when closing the browser tab: https://github.com/daattali/advanced-shiny/tree/master/auto-kill-app

}

# Run the app
shinyApp(ui = ui, server = server)
