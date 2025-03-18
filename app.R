####################################################################################
# This is a Shiny web application for DecoNFlow Benchmarking
# More information on the pipeline: https://github.ugent.be/DePreterLab
####################################################################################

# Install packages
packages <- c("shiny", "bslib", "reshape2", "plotly", "philentropy", "LaplacesDemon", 
              "Metrics", "dplyr", "ggplot2", "readr", "tidyr", "stringr", "forcats", 
              "pROC", "spsComps", "conflicted", "htmltools", "quarto", "sever")

# Check and install missing packages
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Set shiny server options
#options("shiny.host"='10.32.8.17')
#options("shiny.port"=8888)

# Set library packages directory
readRenviron(".Renviron")
.libPaths(c(path.expand(Sys.getenv("R_LIBS_USER")), .libPaths()))
print(.libPaths())


# Load necessary libraries
library(shiny)

#library(tidyverse)  # Includes dplyr, ggplot2, stringr, etc. --> problems on CMB shiny server
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(forcats)
library(ggpubr)
library(rstatix)

library(bslib)
library(reshape2)
library(plotly) 


library(philentropy)
library(LaplacesDemon)
library(Metrics)
#library(funkyheatmap) 


# library(pROC)
# library(spsComps) 
library(conflicted)
library(htmltools)
library(quarto)
library(sever)


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
}

# Run the app
shinyApp(ui = ui, server = server)
