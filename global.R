################################################################################
# global.R – Global configuration and setup for DecoNFlow Benchmarking Shiny App
# This file is sourced once at application startup.
################################################################################

#### 1. Shiny Configuration and Environment Setup ####

# Set the port and host for the Shiny app
options("shiny.host" ='0.0.0.0')           # Specify host '10.32.8.17'
options("shiny.port" = 8888)                  # Specify port 
options(shiny.maxRequestSize = 50 * 1024^2) # Increase maximum upload size to 50MB

# Set global spinner style (instead of repeating in every withSpinner call)
options(
  spinner.type = 8,             # Choose a type (1-8). You used type = 8, so you can change this as needed.
  spinner.color = "#343a40",  # Matches your theme color
  spinner.delay = "1000"       # Specify a delay (in milliseconds) before the spinner is displayed. 
)

# Package conflict resolution (specify preferred functions)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("auc", "pROC")
conflicted::conflict_prefer("layout", "plotly")

#### 2. Load Required Packages ####

# Shiny framework & UI
library(shiny)           # Web application framework for R
library(shinythemes)     # Prebuilt themes for Shiny apps
library(bslib)           # Bootstrap themes for Shiny/Quarto
library(shinycssloaders) # Custom loading spinners
library(sever)           # Custom disconnect screen
library(spsComps)        # Extra UI components from systemPipeShiny
library(DT)              # Interactive datatables

# HTML and web rendering
library(htmltools)       # HTML generation tools
library(quarto)          # Render Quarto documents
library(fontawesome)     # Font Awesome icons (if used)

# Data visualization
library(ggplot2)         # Core plotting system
library(ggpubr)          # Publication-ready plots
library(plotly)          # Interactive plots
library(patchwork)       # Combine ggplot2 plots
library(scales)          # Scaling functions
library(philentropy)     # Distance/similarity metrics
library(pROC)            # ROC curve analysis

# Data handling & transformation
library(dplyr)           # Data manipulation
library(purrr)           # Functional programming tools
library(readr)           # Read text files
library(tidyr)           # Reshape tidy data
library(stringr)         # String manipulation
library(forcats)         # Factor handling
library(tibble)          # Modern data frames
library(data.table)      # High-performance tables
library(reshape2)        # Legacy reshaping
library(jsonlite)        # JSON input/output
library(hms)             # Time-of-day handling
library(Metrics)         # Model evaluation metrics

# Modeling and statistics
library(car)             # Applied regression support
library(rstatix)         # Statistical test wrappers
library(broom)           # Tidy model outputs
library(LaplacesDemon)   # Bayesian inference and MCMC

# Reporting & document generation
library(rmarkdown)       # Dynamic documents
library(knitr)           # Knitting engine
library(xtable)          # LaTeX/HTML tables
library(evaluate)        # Code evaluation
library(xfun)            # Miscellaneous utilities

# Process and parallel support
library(furrr)           # Parallel purrr
library(future)          # Parallel backend
library(parallelly)      # Helper for future
library(processx)        # Run system processes
library(ps)              # Process management
library(later)           # Async scheduling
library(httpuv)          # HTTP/WebSocket server
library(callr)           # Creates new background R processes

# System utilities
library(conflicted)      # Resolve masking conflicts
library(rstudioapi)      # Interface with RStudio
library(cachem)          # Cache management
library(fastmap)         # Fast key-value store
library(cli)             # Command line interface
library(crayon)          # Colored terminal output
library(glue)            # String interpolation
library(lifecycle)       # Lifecycle tools
library(magrittr)        # Pipe operator (%>%)
library(rlang)           # Tidy evaluation
library(assertthat)      # Lightweight assertions
library(backports)       # Backward compatibility
library(abind)           # Combine arrays
library(lazyeval)        # Quoted expressions

#### 3. Define Custom Plot Theme and Aesthetics ####

# Global ggplot2 theme
theme_benchmarking <- theme_classic() +
  theme(
    plot.title = element_text(size = 12, color = "gray10"),
    plot.subtitle = element_text(size = 8, color = "gray30"),
    axis.text.x = element_text(size = 12, color = "gray10"),
    axis.text.y = element_text(size = 12, color = "gray10"),
    axis.title.x = element_text(size = 14, color = "gray10"),
    axis.title.y = element_text(size = 14, color = "gray10"),
    axis.line = element_line(color = "gray50"),
    axis.ticks = element_line(color = "gray50")
  )

# Manual color scale
custom_color_manual <- scale_color_manual(
  name = "DMRtool",
  values = c(
    "limma" = "#d95f02",
    "wgbstools" = "#1b9e77",
    "DMRfinder" = "#7570b3"
  ),
  labels = function(x) gsub("_", " ", x)
)

# Manual fill scale
custom_fill_manual <- scale_fill_manual(
  name = "DMRtool",
  values = c(
    "limma" = "#d95f02",
    "wgbstools" = "#1b9e77",
    "DMRfinder" = "#7570b3"
  ),
  labels = function(x) gsub("_", " ", x)
)

# Manual shape scale
custom_shape_manual <- scale_shape_manual(
  values = c(
    "limma" = 16,       # Circle
    "wgbstools" = 17,   # Triangle
    "DMRfinder" = 15    # Square
  ),
  labels = function(x) gsub("_", " ", x)
)


#### 4. Footer UI Component ####

footer_citation <- function() {
  tagList(
    tags$hr(),
    div(
      style = "text-align: center; margin-top: 20px;",
      tags$div(
        style = "margin-top: 10px; text-align: center;",
        tags$a(
          href = "https://github.com/VIBTOBIlab",
          HTML("<i class='fa fa-github'></i> GitHub"),
          style = "text-decoration: none; color: #555;"
        ),
        tags$a(
          href = "https://depreterlab.sites.vib.be/en#/",
          HTML("<i class='fa fa-link'></i> TOBI Website"),
          style = "margin-left: 10px; text-decoration: none; color: #555;"
        ),
        tags$a(
          href = "mailto:sofvdvel.vandevelde@ugent.be",
          HTML("<i class='fa fa-envelope'></i> Email"),
          style = "margin-left: 10px; text-decoration: none; color: #555;"
        )
      )
    )
  )
}


#### 5. Load and Preprocess Benchmarking Data ####

# Path to raw CSV and cache
data_csv_path <- "results/final_benchmarking_dataset.csv"
cache_rds_path <- "cache/tot_bench.rds"

# Create cache directory if it doesn't exist
if (!dir.exists("cache")) dir.create("cache")

# Load from cache if available, otherwise read and preprocess
if (file.exists(cache_rds_path)) {
  tot_bench <- readRDS(cache_rds_path)
} else {
  message("Reading and preprocessing raw benchmarking dataset...")
  
  # Load dataset
  combined_data <- read.csv(data_csv_path)
  
  # Filter and clean data
  tot_bench <- subset(
    combined_data,
    expected_tf %in% c(0, 0.0001, 0.001, 0.003, 0.007, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5) &
      deconv_tool != "Methyl_Resolver" &
      mixture_type == "in_silico"
  )
  
  # Rename tumor types
  tot_bench$tumor_type[tot_bench$tumor_type == "neuroblastoma"] <- "NBL"
  
  # Convert selected columns to factors
  tot_bench <- tot_bench %>%
    mutate(across(
      c(deconv_tool, dmr_tool, seq_depth, tumor_type, collapse_approach,
        mixture_type, seq_method),
      as.factor
    ))
  
  # Rename selected deconvolution tool levels
  tool_map <- c(
    "EpiDISH_CP_eq" = "Houseman's CP/QP w/equality",
    "EpiDISH_CP_ineq" = "Houseman's CP/QP w/inequality",
    "EpiDISH_RPC" = "EpiDISH RPC",
    "meth_atlas" = "MethAtlas",
    "Methyl_Resolver" = "MethylResolver"
  )
  
  levels(tot_bench$deconv_tool) <- ifelse(
    levels(tot_bench$deconv_tool) %in% names(tool_map),
    tool_map[levels(tot_bench$deconv_tool)],
    levels(tot_bench$deconv_tool)
  )
  
  # Save processed data for future runs
  saveRDS(tot_bench, cache_rds_path)
  
}