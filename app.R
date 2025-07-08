################################################################################
# DecoNFlow Benchmarking - Shiny Web Application
# More information on the pipeline: https://github.ugent.be/DePreterLab
################################################################################

#### 1. Shiny Configuration and Environment Setup ####

# Set the port for the Shiny app
#options("shiny.host"='10.32.8.17')
options("shiny.port" = 8888)

# Configure library paths
.libPaths(c(path.expand(Sys.getenv("R_LIBS_USER")), .libPaths()))
print(.libPaths())

# Increase maximum upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)

# Package conflict resolution (specify preferred functions)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("auc", "pROC")
conflicted::conflict_prefer("layout", "plotly")

#### 2. Source Application Files ####

# Load UI/server module components
source("modules/home_static.R")
source("modules/metrics.R")
source("modules/information.R")
source("modules/contact.R")

# Load global settings and helper functions
source("global.R")


#### 3. UI Definition ####

ui <- navbarPage(
  id = "mainTabset",
  
  # App title
  title = "DecoNFlow Benchmarking",
  
  # Theme (from shinythemes)
  theme = shinythemes::shinytheme("paper"),
  # Valid themes are: cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, 
  # sandstone, simplex, slate, spacelab, superhero, united, yeti.
  
  # Custom CSS
  includeCSS("www/styles.css"),
  
  # Tabs (UI Modules)
  homeTabUI("home"),
  metricsTabUI("plots"),
  informationTabUI("information"),
  contactTabUI("contact")
)


#### 4. Server Logic ####

server <- function(input, output, session) {
  
  # Activate server logic for each module
  homeTabServer("home")
  metricsTabServer("plots")
  informationTabServer("information")
  contactTabServer("contact")
}


#### 5. Run the Application ####

shinyApp(ui = ui, server = server)