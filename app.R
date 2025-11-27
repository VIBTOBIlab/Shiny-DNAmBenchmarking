#########################################################################################
# DecoNFlow Benchmarking - Shiny Web Application
# More information on the pipeline: https://github.com/VIBTOBIlab/Shiny-DNAmBenchmarking/
#########################################################################################

# -----------------------------------------------------------------------
# Packages used in this script (loaded in global.R)
# -----------------------------------------------------------------------
# shiny            : Shiny app framework (UI + server, inputs/outputs, reactivity)
# shinycssloaders  : withSpinner() to show loaders around plots
# plotly           : Interactive plots via plotlyOutput(), renderPlotly(), ggplotly()
# ggplot2          : Static plotting backend for all ggplot-based figures
# DT               : Interactive tables (DT::datatable, DT::renderDataTable)
# dplyr            : Data manipulation (filter, mutate, group_by, summarise, arrange, etc.)
# purrr            : Functional helpers, e.g. map2_dfr() for combining AUC-ROC results
# stringr          : String helpers, e.g. str_replace_all() for nicer labels
# scales           : Labelling and scaling helpers, e.g. label_scientific() for legends
# pROC             : ROC/AUC calculations used in AUC-ROC plots
# ggpubr           : Plot annotations, e.g. stat_pvalue_manual() for LoD p-values
# stats (base)     : Statistical tests and p.adjust(), e.g. wilcox.test()
# utils (base)     : I/O helpers, e.g. write.csv() in download handlers
# grid (base)      : Low-level graphics utilities, e.g. unit() for legend sizing


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
  
  # Good mobile viewpoint
  tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
  # App stylesheet
  tags$head(includeCSS("www/styles.css")),
  
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
        
        # Right side icons
        nav_spacer(),
        nav_item(
          div(class = "navbar-icons",
              tags$a(
                href   = "https://github.com/VIBTOBIlab/DecoNFlow/",
                target = "_blank", rel = "noopener",
                class  = "nextflow-icon-button",
                title  = "Check out Nextflow pipeline TOBIlab/DecoNFlow",
                tags$img(src = "nextflow.png", alt = "Nextflow")
              ),
              tags_a <- tags$a(
                href   = "https://github.com/VIBTOBIlab/Shiny-DNAmBenchmarking/",
                target = "_blank",
                class  = "github-icon-button",
                title  = "View the code for Shiny app on GitHub",
                HTML("<i class='fa fa-github fa-lg'></i>")
              )
          )
        )
      )
    ),
    # Footer area 
    uiOutput("conditionalFooter")
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
  
}

#### 4. Run the Application ####

shinyApp(ui = ui, server = server)

