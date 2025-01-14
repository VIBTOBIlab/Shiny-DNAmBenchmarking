####################################################################################
# This is a Shiny web application for DecoNFlow
# More information on the pipeline: https://github.ugent.be/DePreterLab
####################################################################################
# Load necessary libraries
library(shiny)
library(ggplot2)
library(reshape2)
library(plotly)  

# Source the module files
source("modules/home.R")
#source("modules/overview.R")

# UI
ui <- navbarPage(
  # App title
  title = "DecoNFlow Benchmarking",
  
  # Include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
    
  homeTabUI("home"),  # Add Home module
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
  homeTabServer("home")  # Activate  Home tab server
  #OverviewTabServer("Overview")  # Activate Overview tab server

}

# Run the app
shinyApp(ui = ui, server = server)
