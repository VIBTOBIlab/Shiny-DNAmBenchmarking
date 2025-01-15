####################################################################################
# This is a Shiny web application for DecoNFlow
# More information on the pipeline: https://github.ugent.be/DePreterLab
####################################################################################
# Load necessary libraries
library(shiny)
library(ggplot2)
library(reshape2)
library(plotly) 

library(philentropy)
library(LaplacesDemon)
library(Metrics)
library(dplyr)
library(funkyheatmap)
library(tidyverse)
library(stringr)
library(funkyheatmap)
library(pROC)

# Source the module files
source("modules/home.R")
source("modules/metrics.R")
#source("modules/overview.R")

# Extra settings
options(shiny.maxRequestSize=50*1024^2) # Increase limit to 50MB

# UI
ui <- navbarPage(
  # App title
  title = "DecoNFlow Benchmarking",
  
  # Include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
    
  homeTabUI("home"),  # Add Home module
  metricsTabUI("metrics"),
  #overviewTabUI("Overview"), # Add Overview module

  # Panel 2
  tabPanel("Overview"),
  
  # Panel 3
  tabPanel("Datasets"),
  
  # Panel 4
  tabPanel("Methods"),
  
  # Panel 5
  tabPanel("Usability"),

)

# Server
server <- function(input, output, session) {
  # Activate module tab servers
  homeTabServer("home")
  metricsTabServer("metrics")
  
  #overviewTabServer("overview")  # Activate Overview tab server

}

# Run the app
shinyApp(ui = ui, server = server)
