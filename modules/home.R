# Home Tab UI
homeTabUI <- function(id) {
  ns <- NS(id)  
  tabPanel(
    "Home",
    fluidPage(
      h2(HTML("<b>Welcome to DecoNFlow Benchmarking</b>"), style = "text-align: center;"),
      br(),
      br(),
      

      #Final static plot
      div(
        style = "width: 70%; margin: 0 auto;",  # 80% width and centered
         plotOutput(ns("rank_static"), height = "700px"),
        div(
          style = "text-align: center;",
          downloadButton(ns("download_rank_static_svg"), "Download as SVG", style = "display: inline-block; margin-right: 5px;"),
          downloadButton(ns("download_rank_static_pdf"), "Download as PDF", style = "display: inline-block; margin-right: 5px;"),
          downloadButton(ns("download_rank_static_df"), "Download data", style = "display: inline-block;")
        )
      ),

      br(),
      h3("Citation"),
      br(),br(),
      
      tags$hr(),
      # Wrap Contact Us and Contributors in a centered div
      div(
        style = "text-align: center; margin-top: 20px;",
        # Social media icons or email
        tags$div(
          style = "margin-top: 10px; text-align: center",
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
      ),
    
      # Go to top of the page
      lapply(1: 100, function(x) br()),
      spsGoTop("default")
    )
}


# Home Tab Server
homeTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server-side logic for Home tab if needed (currently none)
    
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
    
    ## 2. Functions
    # RMSE
    rmse <- function(actual, predicted) {
      round(sqrt(mean((actual - predicted)^2)),4)
    }
    # Spearman's rank correlation coefficient (SCC)
    scc <- function(actual, predicted) {
      cor(actual, predicted, method = "spearman")
    }
    roc.obj <- function(true_labels, predicted_scores) {
      true_labels[true_labels>0] <- 1
      roc_obj <- roc(true_labels, predicted_scores)
      return(roc_obj)
    }
    
    
    ## 3. Visualizations Benchmarking Results
    ############################################################################ 
    ## Final ranking of the tools static
    
    # Merge AUC-ROC, RMSE, SCC
    merge_metrics_rank_static <- reactive({
      
      filt_df <- bench %>% 
        filter(!is.na(tool), !is.na(DMRtool))
      
      # Initialize an empty dataframe
      aucroc_data <- data.frame()
      fractions_auc <- unique(bench[bench$expected_fraction != 0, "expected_fraction"])
      miss <- c() # Initialize missing tool tracker
      
      # Loop only through selected user inputs
      for (dmrtool in unique(bench$DMRtool)) {
        for (tool in unique(bench$tool)) {
          for (fraction in fractions_auc) {
            
            filt_df2 <- filt_df %>%
              filter(expected_fraction %in% c(0, fraction),
                     DMRtool == dmrtool,
                     tool == !!tool)
            
            if (length(unique(filt_df2$expected_fraction)) != 2) {
              miss <- c(miss, tool)
              next
            }
            
            roc_curve <- roc.obj(filt_df2$expected_fraction, filt_df2$nbl)
            tmp <- data.frame(
              fpr = 1-rev(roc_curve$specificities),  # False Positive Rate
              tpr = rev(roc_curve$sensitivities),  # True Positive Rate
              thresholds = rev(roc_curve$thresholds),
              auc = rev(roc_curve$auc),
              fraction = fraction,
              tool = tool,
              DMRtool = dmrtool
            )
            aucroc_data <- rbind(aucroc_data, tmp)
          }
        }
      }
      
      # Compute mean AUC Grouped Performance
      classif_performance_auc <- aucroc_data %>%
        group_by(fraction, tool, DMRtool) %>%
        summarize(AUC = mean(auc), .groups = 'drop') %>% 
        group_by(tool, DMRtool) %>%
        summarize(meanAUC = mean(AUC), .groups = 'drop')
      
      # Compute RMSE for fractions > 0
      nonzero_fraction <- filt_df %>%
        filter(expected_fraction != 0) %>%
        group_by(tool, DMRtool, depth, collapse_approach) %>%
        summarize(RMSE = replace_na(rmse(expected_fraction, nbl),1), .groups = 'drop') %>%
        group_by(tool,DMRtool) %>%
        summarize(meanRMSE = mean(RMSE), .groups = 'drop')
      
      # Compute Spearman's rank correlation coefficient (SCC) on all fractions
      all_fractions <- filt_df %>%
        group_by(tool, DMRtool, depth, collapse_approach) %>%
        summarize(SCC = replace_na(scc(expected_fraction, nbl),0), .groups = 'drop') %>%         
        group_by(tool,DMRtool) %>%         
        summarize(meanSCC = mean(SCC))
      
      # Merge all computed metrics
      merged_metrics <- merge(all_fractions, nonzero_fraction, by = c("tool", "DMRtool"))
      merged_metrics <- merge(merged_metrics, classif_performance_auc, by = c("tool", "DMRtool"))
      return(merged_metrics)
    })
    
    normalize_metrics_static <- reactive({ 
      # Retrieve merged metrics from reactive function
      merged_metrics <- merge_metrics_rank_static()
      
      # Normalize all the metrics based on user-selected inputs
      normalized_list <- list()
      miss <- list()
      
      for (dmrtool in unique(bench$DMRtool)) {
        tmp <- merged_metrics %>%
          filter(DMRtool == dmrtool)
        
        # Skip if df is empty
        if (dim(tmp)[1] == 0) {
          miss <- c(miss, dmrtool)
          next
        }
        
        tmp <- tmp %>%
          mutate(
            SCC = ifelse(is.na(meanSCC), 0, meanSCC),  # Replace NAs in SCC with 0
            RMSE = ifelse(is.na(meanRMSE), 1, meanRMSE),   # Replace NAs in RMSE with 1
            AUC = ifelse(is.na(meanAUC), 0, meanAUC)
          )
        
        tmp$normSCC <- (tmp$SCC - min(tmp$SCC)) / (max(tmp$SCC) - min(tmp$SCC))
        tmp$normRMSE <- 1 - (tmp$RMSE - min(tmp$RMSE)) / (max(tmp$RMSE) - min(tmp$RMSE))
        tmp$normAUC <- (tmp$meanAUC - min(tmp$meanAUC)) / (max(tmp$meanAUC) - min(tmp$meanAUC))
        
        # Append the normalized subset to the list
        normalized_list[[dmrtool]] <- tmp[, c("tool", "DMRtool", 
                                              "meanAUC", "meanRMSE", "meanSCC", "normAUC", "normSCC", "normRMSE" )]
      }
      
      # Combine all normalized subsets into a single data frame
      normalized_df <- do.call(rbind, normalized_list)
      
      normalized_df <- normalized_df %>%
        rename(RMSE = meanRMSE, SCC = meanSCC)
      
      # Create a combined metric score
      nzeros <- nrow(bench[bench$expected_fraction == 0,])
      nnonzeros <- nrow(bench[bench$expected_fraction != 0,])
      tot <- nzeros + nnonzeros
      
      normalized_df$Score <- 
        normalized_df$normAUC +
        (nnonzeros / tot) * (normalized_df$normRMSE) + 
        normalized_df$normSCC
      
      return(normalized_df)
      
    })
    
    create_metric_plot <- function(metric_name, data) {
      # Determine sort direction
      desc_order <- metric_name != "RMSE"
      
      metric_data <- data %>%
        select(tool, DMRtool, !!sym(metric_name)) %>%
        rename(Value = !!sym(metric_name))
      
      # Sort tools manually
      tool_levels <- metric_data %>%
        group_by(tool) %>%
        summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
        arrange(if (desc_order) Value else desc(Value)) %>%
        pull(tool)
      
      # Apply tool order explicitly
      metric_data$tool <- factor(metric_data$tool, levels = tool_levels)
      
      ggplot(metric_data, aes(x = Value, y = tool, color = DMRtool)) +
        geom_point(size = 3, alpha = 0.85) +
        labs(
          #title = metric_name,
          x = metric_name,
          y = ""
        ) +
        theme_benchmarking +
        #theme(panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.5)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +  
        scale_y_discrete(labels = function(x) str_replace_all(x, "_", " ")) +
        custom_color_manual+
        theme(legend.text = element_text(size = 13),
              legend.title = element_text(size = 14),
              axis.title.x = element_text(size = 15),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              panel.grid.major.y = element_line(color = "lightgrey", linewidth = 0.4),
              panel.grid.minor.y = element_blank()
        )
    }
    
    create_static_ranking_plot <- function() {
      merged_metrics <- merge_metrics_rank_static()
      normalized_df <- normalize_metrics_static()
      
      p1 <- create_metric_plot("meanAUC", normalized_df)
      p2 <- create_metric_plot("RMSE", normalized_df)
      p3 <- create_metric_plot("SCC", normalized_df)
      p4 <- create_metric_plot("Score", normalized_df)
      
      (p1 + p2) / 
        patchwork::plot_spacer() / 
        (p3 + p4) +
        plot_layout(
          guides = "collect",
          heights = c(1, 0.05, 1)  # Row 1, spacer, Row 2
        ) & 
        theme(legend.position = "top")
      
    }
    
    output$rank_static <- renderPlot({
      create_static_ranking_plot()
      
    })
    
    # Save rank static using the function
    download_rank_static_plot <- function(ext) {
      downloadHandler(
        filename = function() paste("rank_static_",Sys.Date(),".", ext, sep = ""),
        content = function(file) {
          
          plot <- create_static_ranking_plot()
          ggsave(file, plot = plot, width = 14, height = 10, dpi = 300, device = ext)
          
        }
      )
    }
    output$download_rank_static_svg <- download_rank_static_plot("svg")
    output$download_rank_static_pdf <- download_rank_static_plot("pdf")    
    
    # Download data of rank plots
    output$download_rank_static_df <- downloadHandler(
      filename = function() {
        paste("rank_static_",Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        merged_metrics <- merge_metrics_rank_static()
        normalized_df <- normalize_metrics_static()
        # df <- normalized_df %>% select(-normAUC, -normSCC, -normRMSE)
        write.csv(normalized_df, file, row.names = FALSE)
      }
    )
    
    
  })
}
