#########################################################################################
# DecoNFlow Benchmarking - Shiny Web Application
# More information on the pipeline: https://github.com/VIBTOBIlab/Shiny-DNAmBenchmarking/
#########################################################################################

#### 1. Source Application Files ####

# Load UI/server module components
source("modules/home_static.R")
source("modules/plots.R")
source("modules/information.R")
source("modules/contact.R")

# Load global settings and helper functions
source("global.R")

#### 2. UI Definition ####
ui <- fluidPage(
  class = "app-body",
  tags$head(
    includeCSS("www/styles.css")
  ),
  div(
    class = "app-container",
    
    # Main content 
    div(
      class = "app-main",
      bslib::page_navbar(
        id = "mainTabset",
        title = "DecoNFlow Benchmarking",
        theme = theme,
        
        navbar_options = navbar_options(
          position = "fixed-top",
          collapsible = TRUE
        ),
        
        # Tabs
        homeTabUI("home"),
        plotsTabUI("plots"),
        informationTabUI("information"),
        contactTabUI("contact"),
        
        # GitHub icon
        nav_spacer(),
        nav_item(
          tags$a(
            href = "https://github.com/VIBTOBIlab/Shiny-DNAmBenchmarking/",
            target = "_blank",
            class = "github-icon-button",
            title = "View on GitHub",
            HTML("<i class='fa fa-github fa-lg'></i>")
          )
        )
      )
    ),
    
    # Footer area (conditionally rendered via uiOutput)
    uiOutput("conditionalFooter")
    
    # Footer area
    # div(
    #   class = "app-footer-wrapper",
    #   lapply(1:5, function(x) tags$br()),
    #   spsGoTop("default"),
    #   footer_citation()
    # )
  )
)


#### 3. Server Logic ####

server <- function(input, output, session) {
  
  # Activate server logic for each module
  homeTabServer("home")
  plotsTabServer("plots")
  informationTabServer("information")
  contactTabServer("contact")
  
  output$conditionalFooter <- renderUI({
    if (input$mainTabset != "Contact") {
      div(
        class = "app-footer-wrapper",
        lapply(1:5, function(x) tags$br()),
        spsGoTop("default"),
        footer_citation()
      )
    }
  })
  
  #print(sessionInfo())
}

#### 4. Run the Application ####

#run_with_themer
shinyApp(ui = ui, server = server)

