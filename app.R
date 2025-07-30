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




#### 2. UI Definition ####
ui <- bslib::page_navbar(
  id = "mainTabset",
  
  # App title
  title = "DecoNFlow Benchmarking",

  # Apply updated theme
  theme = theme,
  
  header = tagList(
    includeCSS("www/styles.css")
  ),
  
  # sticky navbar
  position = c("fixed-top"),

  # Mobile responsiveness
  collapsible = TRUE,
  fluid = TRUE,
  
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
  
  print(sessionInfo())
}

#### 4. Run the Application ####

#run_with_themer
shinyApp(ui = ui, server = server)

