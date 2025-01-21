####################################################################################
# This is a Shiny web application for DecoNFlow Benchmarking
# More information on the pipeline: https://github.ugent.be/DePreterLab
####################################################################################
# Load necessary libraries
library(shiny)
library(reshape2)
library(plotly) 
library(philentropy)
library(LaplacesDemon)
library(Metrics)
library(funkyheatmap)  # Only load it once
library(tidyverse)  # Includes dplyr, ggplot2, stringr, etc.
library(pROC)
library(spsComps) 
library(conflicted)

library(spsComps)

# Source the module files
source("modules/home.R")
source("modules/metrics.R")
source("modules/contact.R")

#Source other files
source("global.R")

# Extra settings
options(shiny.maxRequestSize=50*1024^2) # Increase limit to 50MB
conflicts_prefer(dplyr::filter) #conflict between the filter functions from the dplyr and stats packages. 

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
  # tabPanel("Overview"),
  # tabPanel("Datasets"),
  # tabPanel("Methods"),
  # tabPanel("Usability"),
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
  #overviewTabServer("overview")  # Activate Overview tab server
  session$onSessionEnded(stopApp) # Automatically stop a Shiny app when closing the browser tab: https://github.com/daattali/advanced-shiny/tree/master/auto-kill-app

}

# Run the app
shinyApp(ui = ui, server = server)
