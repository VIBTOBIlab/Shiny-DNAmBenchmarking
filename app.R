####################################################################################
# This is a Shiny web application for DecoNFlow Benchmarking
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

library(spsComps)
# Source the module files
source("modules/home.R")
source("modules/metrics.R")
source("modules/contact.R")

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
  
  # Add modules
  homeTabUI("home"),  
  metricsTabUI("metrics"),
  tabPanel("Overview"),
  tabPanel("Datasets"),
  tabPanel("Methods"),
  tabPanel("Usability"),
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

}

# Run the app
shinyApp(ui = ui, server = server)
