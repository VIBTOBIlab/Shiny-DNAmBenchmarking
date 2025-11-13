################################################################################
# global.R – Global configuration and setup for DecoNFlow Benchmarking Shiny App
# This file is sourced once at application startup.
################################################################################

#### 1. Shiny Configuration and Environment Setup ####

# Set the port and host for the Shiny app
options("shiny.host" ='0.0.0.0')           # Specify host '10.32.8.17'
options("shiny.port" = 8889)                  # Specify port 
#options(shiny.maxRequestSize = 50 * 1024^2) # Increase maximum upload size to 50MB

# Set global spinner style (instead of repeating in every withSpinner call)
options(
  spinner.type = 8,             # Choose a type (1-8). You used type = 8, so you can change this as needed.
  spinner.color = "#1C2C48",  # Matches your theme color
  spinner.delay = "1000"       # Specify a delay (in milliseconds) before the spinner is displayed. 
)

# Package conflict resolution (specify preferred functions)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("auc", "pROC")
conflicted::conflict_prefer("layout", "plotly")

#### 2. Load Required Packages ####

# ── Shiny framework & UI components ──
library(shiny)             # Core package for building interactive web apps in R
library(shinythemes)       # Add pre-built Bootstrap themes to Shiny apps
library(bslib)             # Full theming system for Bootstrap 4 & 5 in Shiny/Quarto
library(shinycssloaders)   # Adds animated spinners while outputs are loading
library(sever)             # Customizes Shiny's disconnect/reconnect screen
library(spsComps)          # Additional UI components, e.g., image carousel, cards
library(shinyWidgets)      # Extended widgets: sliders, dropdowns, pickers, etc.
library(DT)                # DataTables (sortable, filterable HTML tables)
library(fontawesome)       # Access to Font Awesome icons (used in footer links)

# ── HTML rendering and dynamic document support ──
library(htmltools)         # Lower-level HTML generation helpers (tags, scripts)
library(quarto)            # Render and preview Quarto documents within the app
library(rmarkdown)         # Render R Markdown reports
library(knitr)             # Engine that converts R Markdown into HTML/PDF/Word
library(xtable)            # Convert R objects to LaTeX or HTML tables
library(evaluate)          # Evaluate R expressions and capture output
library(xfun)              # Utility functions (e.g., for file paths, OS detection)

# ── Data visualization ──
library(ggplot2)           # Main plotting system (grammar of graphics)
library(ggpubr)            # Publication-quality plots + stats annotation for ggplot2
library(plotly)            # Interactive graphs (zoom, hover) for ggplot2/plotly
library(patchwork)         # Combine multiple ggplot2 plots into one layout
library(scales)            # Formatting and transformation tools for axes/legends
library(philentropy)       # Compute distances (e.g., cosine, KL) between distributions
library(pROC)              # Create and analyze ROC curves (classification)
library(ggsignif)          # Add significance brackets (e.g., p-values) to ggplots
library(viridisLite)       # Colorblind-safe and perceptually uniform color palettes

# ── Data import, cleaning, and manipulation ──
library(dplyr)             # Data manipulation: filter, mutate, group_by, summarize
library(purrr)             # Functional programming: map, walk, reduce, etc.
library(readr)             # Fast, user-friendly CSV/TSV reading
library(tidyr)             # Reshape data: pivot_longer, pivot_wider, nest, etc.
library(stringr)           # String manipulation: detect, replace, extract, etc.
library(forcats)           # Factor manipulation: reorder, lump, rename levels
library(tibble)            # A modern, clean data.frame structure with printing improvements
library(data.table)        # Extremely fast data manipulation and aggregation
library(reshape2)          # Legacy reshaping tools: melt, cast
library(jsonlite)          # Parse and generate JSON files
library(hms)               # Store and manipulate time-of-day values
library(Metrics)           # Implements evaluation metrics like RMSE, MAE, etc.

# ── Modeling and statistical summaries ──
library(car)               # Companion to regression textbook (vif, Anova, etc.)
library(rstatix)           # Pipe-friendly statistical tests (t-test, ANOVA, etc.)
library(broom)             # Convert model objects (lm, glm, etc.) into tidy data.frames
library(LaplacesDemon)     # Bayesian analysis framework including MCMC algorithms

# ── Parallel computing and process control ──
library(furrr)             # Parallel versions of purrr functions
library(future)            # Unified API for parallel, sequential, or multicore execution
library(parallelly)        # Manages resources and backends for parallel jobs
library(processx)          # Start/manage external system processes
library(ps)                # Inspect system processes (used by processx)
library(callr)             # Run R scripts in a separate R process (e.g., background jobs)
library(later)             # Schedule delayed operations (used in async UI/reactivity)
library(httpuv)            # Core HTTP and WebSocket server used by Shiny

# ── System utilities and dev support ──
library(conflicted)        # Explicit resolution of conflicting function names
library(rstudioapi)        # Communicate with RStudio IDE (e.g., get active file)
library(cachem)            # Manage memory/disk caches
library(fastmap)           # Fast, pointer-based data structures (used in cachem)
library(cli)               # Styled CLI messages, progress bars, status output
library(crayon)            # Colored text for console/logging
library(glue)              # Fast and readable string interpolation (e.g., glue("Value: {x}"))
library(lifecycle)         # Mark functions as experimental/deprecated, etc.
library(magrittr)          # Pipe operator `%>%`, plus aliases for base functions
library(rlang)             # Underlying system for tidy evaluation and metaprogramming
library(assertthat)        # Simple, readable assertions (e.g., assert_that(x > 0))
library(backports)         # Compatibility for older R versions (used internally)
library(abind)             # Combine multidimensional arrays
library(lazyeval)          # Quote/capture expressions lazily (used in older tidyverse code)

# Theme selection
theme_benchmarking <- theme_classic() +
  theme(
    plot.title = element_text(color = "gray10"),
    plot.subtitle = element_text(color = "gray30"),
    axis.text.x = element_text(color = "gray10"),
    axis.text.y = element_text(color = "gray10"),
    axis.title.x = element_text(color = "gray10"),
    axis.title.y = element_text(color = "gray10"),
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
    div(
      class = "app-footer",
      tags$hr(),
      div(
        class = "footer-links",
        tags$a(
          href = "https://github.com/VIBTOBIlab/Shiny-DNAmBenchmarking",
          HTML("<i class='fa fa-github footer-icon'></i> GitHub"),
          style = "text-decoration: none;"
        ),
        tags$a(
          href = "https://depreterlab.sites.vib.be/en#/",
          HTML("<i class='fa fa-link footer-icon'></i> TOBI Website"),
          style = "margin-left: 10px; text-decoration: none;"
        ),
        tags$a(
          href = "mailto:sofvdvel.vandevelde@ugent.be",
          HTML("<i class='fa fa-envelope footer-icon'></i> Email"),
          style = "margin-left: 10px; text-decoration: none;"
        )
      )
    )
  )
}


#### 5. Load and Preprocess Benchmarking Data ####

# Define path to input dataset (must be mounted or present in repo)
data_csv_path <- "results/final_benchmarking_dataset.csv"

if (!file.exists(data_csv_path)) {
  stop("Dataset not found at 'results/benchmarking_dataset.csv'. Please run the container with:\n  -v /path/to/your.csv:results/benchmarking_dataset.csv")
}

# Load dataset
combined_data <- read.csv(data_csv_path)

# Filter and clean data
tot_bench <- combined_data %>%
  filter(!(deconv_tool %in% c("MeDeCom","RefFreeCellMix", "Methyl_Resolver" )))

# Convert selected columns to factors
tot_bench <- tot_bench %>%
  mutate(across(
    c(deconv_tool, dmr_tool, tumor_type,
      mixture_type, seq_method),
    as.factor
  ))

# Remove rows with NA values for further analysis 
tot_bench <- tot_bench %>%
  filter(!is.na(predicted_tf))


# Save processed data for future runs
#saveRDS(tot_bench, cache_rds_path)
  

#### 6. Helper Functions ####
# RMSE
rmse <- function(actual, predicted) {
  round(sqrt(mean((actual - predicted)^2)),4)
}
# Spearman's rank correlation coefficient (SCC)
scc <- function(actual, predicted) {
  cor(actual, predicted, method = "spearman", use = "pairwise.complete.obs")
}

roc.obj <- function(true_labels, predicted_scores) {
  true_labels[true_labels>0] <- 1
  roc_obj <- roc(true_labels, predicted_scores)
  return(roc_obj)
}


#### 7. Create and Update Theme ####
theme <- bs_theme(
  version = 5,
  fg = "#1c2c48", 
  primary = "#456CB0",
  font_scale = NULL,
  preset = "shiny",
  bg = "#FFFFFF",
  navbar_bg ="#EFF1F6" 
)


#### 8. Read in documentation excel file #####
# Load excel data
deconvtools_data <- read_csv("www/deconvtools.csv", show_col_types = FALSE)
dmrtools_data <- read_csv("www/dmrtools.csv", show_col_types = FALSE)

# Convert GitHub and Publication columns into clickable links
deconvtools_data$GitHub <- paste0("<a href='", deconvtools_data$GitHub, "' target='_blank'>", deconvtools_data$GitHubnames, "</a>")
deconvtools_data$Publication <- paste0("<a href='", deconvtools_data$Publication, "' target='_blank'>Paper</a>")

dmrtools_data$GitHub <- paste0("<a href='", dmrtools_data$GitHub, "' target='_blank'>", dmrtools_data$Tool, "</a>")
dmrtools_data$Publication <- paste0("<a href='", dmrtools_data$Publication, "' target='_blank'>Paper</a>")

