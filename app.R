#########################################################################################
# DecoNFlow Benchmarking - Shiny Web Application
# More information on the pipeline: https://github.com/VIBTOBIlab/Shiny-DNAmBenchmarking/
#########################################################################################

#### 1. Source Application Files ####

# Load UI/server module components
source("modules/home_static.R")
source("modules/metrics.R")
source("modules/information.R")
source("modules/contact.R")

# Load global settings and helper functions
source("global.R")

# Resource path and CSS
includeCSS("www/styles.css")

#### 2. UI Definition ####

ui <- navbarPage(
    id = "mainTabset",
    
    # App title
    title = "DecoNFlow Benchmarking",
    
    # Theme (from shinythemes)
    theme = shinythemes::shinytheme("paper"),
    # Valid themes are: cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, 
    # sandstone, simplex, slate, spacelab, superhero, united, yeti.
    
    # Mobile responsiveness
    collapsible = TRUE,
    fluid = TRUE,  
    
    # Custom CSS styling
    includeCSS("www/styles.css"),
  
    # Tabs (UI Modules)
    homeTabUI("home"),
    metricsTabUI("plots"),
    informationTabUI("information"),
    contactTabUI("contact")
)


#### 3. Server Logic ####

server <- function(input, output, session) {

  # Activate server logic for each module
  homeTabServer("home")
  metricsTabServer("plots")
  informationTabServer("information")
  contactTabServer("contact")
  
}

#### 4. Run the Application ####

shinyApp(ui = ui, server = server)

