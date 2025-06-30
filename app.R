####################################################################################
# This is a Shiny web application for DecoNFlow Benchmarking
# More information on the pipeline: https://github.ugent.be/DePreterLab
####################################################################################

# Set shiny server options
#options("shiny.host"='10.32.8.17')
options("shiny.port"=8888)

# Set library packages directory
#readRenviron(".Renviron")
.libPaths(c(path.expand(Sys.getenv("R_LIBS_USER")), .libPaths()))
print(.libPaths())


# Source the module files
source("modules/home_static.R")
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
