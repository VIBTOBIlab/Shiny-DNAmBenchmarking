# This file is sourced once when the app starts

# Load necessary packages
library(dplyr)
library(stringr)

## 1. Import Data and Preprocessing
# Load the data
combined_data <- read.csv("results/results_nbl_cfRRBS.csv")

metadata <- read.csv("files/samples_metadata_nbl_cfRRBS.csv",sep = "\t")[,c("Sample","Exp.nbl","Depth")]
colnames(metadata) <- c("sample","expected_fraction","depth")

# Combine data with metadata 
bench <- as.data.frame(merge(combined_data, metadata, by = "sample"))

# Subset bench
bench <- subset(bench,bench$reference=="reference_11healthy_9nbl" &
                  bench$expected_fraction %in% c(0,0.0001,0.001,0.003,0.007,0.01,0.025,0.05,0.1,0.25,0.5))

# Clean and format data 
bench$nbl <- round(bench$nbl, 4) 
bench$sample <- str_trim(bench$sample, side = c("both", "left", "right"))
bench$tool <- str_trim(bench$tool, side = c("both", "left", "right"))
bench <- as.data.frame(unique(bench))

# Convert depth to Millions notation and add "M"
bench$depth <- paste0(bench$depth / 1e6, "M")

# Sort the levels of depth
depth_levels <- unique(bench$depth)
sorted_depth_levels <- depth_levels[order(as.numeric(sub("M", "", depth_levels)))]
bench$depth <- factor(bench$depth, levels = sorted_depth_levels)

# Convert to factor 
bench <- bench %>%
  mutate(across(c(reference, DMRtool, direction, top, collapse_approach, 
                  min_cpgs, min_counts), as.factor))




# Define the custom theme
theme_benchmarking <- theme_classic() +
      theme(
        plot.title = element_text(size = 12,color = "gray10"),
        plot.subtitle = element_text(size=8, color= "gray 30"),
        axis.text.x = element_text(size = 12, color = "gray10"),
        axis.text.y = element_text(size = 12, color = "gray10"),
        axis.title.x = element_text(size = 14 , color = "gray10"),
        axis.title.y = element_text(size = 14, color = "gray10"),
        #legend.title = element_text(size = 14, color = "gray10"),
        #legend.text = element_text(size = 12, color = "gray10"),
        axis.line = element_line(color = "gray50"),
        axis.ticks = element_line(color = "gray50"),
      ) 

# Specificy custom colors
custom_color_manual <- scale_color_manual( name = "DMRtool", 
      values = c("limma" = "#d95f02", 
                 "wgbs_tools" = "#1b9e77", 
                 "DMRfinder" = "#7570b3"),
      labels = function(x) gsub("_", " ", x)
    )

custom_fill_manual <- scale_fill_manual( name = "DMRtool", 
                                          values = c("limma" = "#d95f02", 
                                                     "wgbs_tools" = "#1b9e77", 
                                                     "DMRfinder" = "#7570b3"),
                                          labels = function(x) gsub("_", " ", x)
)


# Specify custom shapes
custom_shape_manual <- scale_shape_manual(
  values = c("limma" = 16,  # Circle
             "wgbs_tools" = 17,  # Triangle
             "DMRfinder" = 15), # Square
  labels = function(x) gsub("_", " ", x)
)
