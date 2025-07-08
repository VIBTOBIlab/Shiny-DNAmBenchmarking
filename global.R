################################################################################
# global.R – Global configuration and setup for DecoNFlow Benchmarking Shiny App
# This file is sourced once at application startup.
################################################################################


#### 1. Load Required Packages ####

library(shiny)
# library(tidyverse)  # Avoided due to conflicts on CMB Shiny server
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(forcats)
library(ggpubr)
library(rstatix)
library(patchwork)
library(bslib)
library(reshape2)
library(plotly)
library(scales)
library(philentropy)
library(LaplacesDemon)
library(Metrics)
library(furrr)
library(future)
library(purrr)
library(spsComps)
library(pROC)
library(conflicted)
library(htmltools)
library(quarto)
library(sever)
library(shinythemes)
library(DT)
# Set future plan if needed
# plan(multicore)


#### 2. Define Custom Plot Theme and Aesthetics ####

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


#### 3. Footer UI Component ####

footer_citation <- function() {
  tagList(
    br(), br(), br(),
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


#### 4. Load and Preprocess Benchmarking Data ####

# Load dataset
combined_data <- read.csv("results/final_benchmarking_dataset.csv")

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