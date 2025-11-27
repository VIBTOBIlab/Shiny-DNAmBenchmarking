################################################################################
# global.R – Global configuration and setup for DecoNFlow Benchmarking Shiny App
# This file is sourced once at application startup.
################################################################################

#### 1. Shiny Configuration and Environment Setup ####

# Set the port and host for the Shiny app
options("shiny.host" ='0.0.0.0')           
options("shiny.port" = 8889)                 

# Set global spinner style 
options(
  spinner.type = 8,             # Choose a type (1-8). 
  spinner.color = "#1C2C48",  # Matches your theme color
  spinner.delay = "1000"       # In milliseconds
)

#### 2. Load Required Packages ####
# Core Shiny + UI
library(shiny)
library(shinycssloaders)
library(bslib)
library(spsComps)


# Plotting
library(ggplot2)
library(plotly)
library(ggpubr)

# Tables
library(DT)

# Data manipulation
library(dplyr)
library(purrr)
library(stringr)
library(readr)

# Scales / labels
library(scales)

# ROC / AUC
library(pROC)

# Needed for unit()
library(grid)

# Standard packages (loaded by default in R; no library() really needed)
# stats
# utils


#### 3. Themes ####

# Main theme
theme <- bslib::bs_theme(
  version = 5,
  fg = "#1c2c48", 
  primary = "#456CB0",
  font_scale = NULL,
  preset = "shiny",
  bg = "#FFFFFF",
  navbar_bg ="#EFF1F6" 
)

# Plotting theme 
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
data_csv_path <- "results/benchmarking_dataset.csv"

if (!file.exists(data_csv_path)) {
  stop("Dataset not found at 'results/benchmarking_dataset.csv'. Please run the container with:\n  -v /path/to/your.csv:results/benchmarking_dataset.csv")
}

# Load dataset
combined_data <- utils::read.csv(data_csv_path)

# Filter and clean data
tot_bench <- combined_data %>%
  dplyr::filter(!(deconv_tool %in% c("MeDeCom","RefFreeCellMix", "Methyl_Resolver" )))

# Convert selected columns to factors
tot_bench <- tot_bench %>%
  dplyr::mutate(across(
    c(deconv_tool, dmr_tool, tumor_type,
      mixture_type, seq_method),
    as.factor
  ))

# Remove rows with NA values for further analysis 
tot_bench <- tot_bench %>%
  dplyr::filter(!is.na(predicted_tf))


# Save processed data for future runs
#saveRDS(tot_bench, cache_rds_path)
  

#### 6. Helper Functions ####
# RMSE
rmse <- function(actual, predicted) {
  round(sqrt(mean((actual - predicted)^2)),4)
}
# Spearman's rank correlation coefficient (SCC)
scc <- function(actual, predicted) {
  stats::cor(actual, predicted, method = "spearman", use = "pairwise.complete.obs")
}

# AUC
roc.obj <- function(true_labels, predicted_scores) {
  true_labels[true_labels>0] <- 1
  roc_obj <- pROC::roc(
    response = true_labels,
    predictor = predicted_scores,
    quiet = TRUE
  )
  return(roc_obj)
}


#### 7. Read in documentation #####
# Load csv data
deconvtools_data <- readr::read_csv("www/deconvtools.csv", show_col_types = FALSE)
dmrtools_data <- readr::read_csv("www/dmrtools.csv", show_col_types = FALSE)

# Convert GitHub and Publication columns into clickable links
deconvtools_data$GitHub <- paste0("<a href='", deconvtools_data$GitHub, "' target='_blank'>", deconvtools_data$GitHubnames, "</a>")
deconvtools_data$Publication <- paste0("<a href='", deconvtools_data$Publication, "' target='_blank'>Paper</a>")

dmrtools_data$GitHub <- paste0("<a href='", dmrtools_data$GitHub, "' target='_blank'>", dmrtools_data$Tool, "</a>")
dmrtools_data$Publication <- paste0("<a href='", dmrtools_data$Publication, "' target='_blank'>Paper</a>")

