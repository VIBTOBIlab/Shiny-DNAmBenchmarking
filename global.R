library(ggplot2)
# global.R
# Define the custom theme
theme_benchmarking <- theme_classic() +
      theme(
        plot.title = element_text(size = 12,color = "gray10"),
        plot.subtitle = element_text(size=8, color= "gray 30"),
        axis.text.x = element_text(size = 12, color = "gray10"),
        axis.text.y = element_text(size = 12, color = "gray10"),
        axis.title.x = element_text(size = 14 , color = "gray10"),
        axis.title.y = element_text(size = 14, color = "gray10"),
        legend.title = element_text(size = 14, color = "gray10"),
        legend.text = element_text(size = 12, color = "gray10"),
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




#c("limma" = "#F8766D", "wgbs_tools" = "#00BA38",  "DMRfinder" = "#619CFF")
