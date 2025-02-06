metricsTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Metrics",
    fluidPage(
      h2("Benchmarking Metrics"),
      p("We considered 3 different key metrics: the root-mean-squared error (RMSE), the area under the curve (AUC-ROC) and the Spearman's rank correlation coefficient (ρ). To create an overall benchmarking score against which to compare the deconvolution tools, we min-max scaled the metrics and computed the geometric mean of the three metrics to obtain the final benchmarking scores. Finally, we ranked the tools based on these scores."),
      p("Below, you can find the computed metrics and visualizations."),
      br(),
      tabsetPanel(
        tabPanel(title = "General",
                  # Boxplots section
                  h3("Boxplots of the predictions for each tumoral fraction"),
                  sidebarLayout(
                    sidebarPanel( width = 3,
                      selectInput(
                        ns("boxplot_fraction_select"), 
                        label = "Select Tumoral Fraction:",
                        choices = NULL,
                        selected = NULL
                      ),
                      checkboxGroupInput(
                        ns("boxplot_tools_select"),
                        label = "Select Deconvolution Tools:",
                        choices = NULL,  
                        selected = NULL
                      ),
                      checkboxGroupInput(
                        ns("boxplot_dmrtools_select"),
                        label = "Select DMR Tools:",
                        choices = NULL,  
                        selected = NULL
                      )
                    ),
                    mainPanel( width = 8,
                      plotlyOutput(ns("boxplot_TF"), height = "600px"),
                      br(),
                      downloadButton(ns("download_boxplot_TF_svg"), "Download as SVG"),
                      downloadButton(ns("download_boxplot_TF_pdf"), "Download as PDF"),
                      downloadButton(ns("download_boxplot_TF_df"), "Download data"),
                      br(), br(), br()
                    )
                  ),
                  
                  tags$hr(), br(), br(),
                  
                  # RMSE section
                  h3("Performance (nRMSE) per Tumoral Fractions"),
                  sidebarLayout(
                    sidebarPanel(width = 3,
                      selectInput(
                        ns("rmse_tool_select"), 
                        label = "Select Deconvolution Tool:",
                        choices = NULL,  
                        selected = NULL
                      ),
                      checkboxGroupInput(
                        ns("rmse_dmrtools_select"),
                        label = "Select DMR Tools:",
                        choices = NULL,  
                        selected = NULL
                      )
                    ),
                    mainPanel(width = 8,
                      plotlyOutput(ns("rmse_plot"), height = "600px"),
                      br(),
                      downloadButton(ns("download_rmse_plot_svg"), "Download as SVG"),
                      downloadButton(ns("download_rmse_plot_pdf"), "Download as PDF"),
                      downloadButton(ns("download_rmse_plot_df"), "Download data"),
                      br(), br(), br()
                    )
                  ),
                  
                  tags$hr(), br(), br(),
                  

                  # AUC-ROC of tools at 4 low tumoral fractions
                  h3("AUC-ROC at different tumoral fractions"),
                  sidebarLayout(
                    sidebarPanel(width = 3,
                      checkboxGroupInput(
                        ns("aucroc_fractions_select"),
                        label = "Select Tumoral Fraction:",
                        choices = c(0.0001, 0.001, 0.01, 0.05),
                        selected = c(0.0001, 0.001, 0.01, 0.05)
                      ),
                      selectInput(
                        ns("aucroc_tool_select"),
                        label = "Select Deconvolution Tools:",
                        choices = NULL,
                        selected = NULL
                      ),
                      selectInput(
                        ns("aucroc_dmrtool_select"),
                        label = "Select DMR Tool:",
                        choices = NULL,
                        selected = NULL
                      )
                    ),
                    
                    mainPanel(width = 8,
                      plotOutput(ns("aucroc"), height = "600px"),
                      downloadButton(ns("download_aucroc_svg"), "Download as SVG"),
                      downloadButton(ns("download_aucroc_png"), "Download as PNG"),
                      br(), br(),
                    )
                  )
                  ), # close 'General' tab 
        
        tabPanel(title = "Tools",
                  # RMSE Comparison section
                  h3("Tool ranks based on RMSE per tumoral fraction"),
                  sidebarLayout(
                    sidebarPanel(width = 3,
                      selectInput(
                        ns("rmse_comparison_fraction_select"), 
                        label = "Select Tumoral Fraction:",
                        choices = NULL,
                        selected = NULL
                      ),
                      checkboxGroupInput(
                        ns("rmse_comparison_tools_select"),
                        label = "Select Deconvolution Tools:",
                        choices = NULL,  
                        selected = NULL
                      ),
                      checkboxGroupInput(
                        ns("rmse_comparison_dmrtools_select"),
                        label = "Select DMR Tools:",
                        choices = NULL, 
                        selected = NULL
                      )
                    ),
                    mainPanel(width = 8,
                      plotlyOutput(ns("rmse_comparison"), height = "600px"),
                      br(),
                      downloadButton(ns("download_rmse_comparison_svg"), "Download as SVG"),
                      downloadButton(ns("download_rmse_comparison_pdf"), "Download as PDF"),
                      downloadButton(ns("download_rmse_comparison_df"), "Download data"),
                      br(), br(),
                    )
                  ),
                 tags$hr(), br(), br(),
                 h3("General ranking of the tools"),
                 sidebarLayout(
                   sidebarPanel(width = 3,
                     checkboxGroupInput(
                       ns("rank_tools_select"),
                       label = "Select Deconvolution Tools:",
                       choices = NULL,  
                       selected = NULL
                     ),
                     checkboxGroupInput(
                       ns("rank_dmrtools_select"),
                       label = "Select DMR Tools:",
                       choices = NULL, 
                       selected = NULL
                     ),
                     selectInput(
                       ns("rank_metric_select"), 
                       label = "Tools Ranked by:",
                       choices = c("normAUC", "normRMSE", "normSCC", "meanScore"),
                       selected = "normAUC"
                     )
                   ),
                   mainPanel(width = 8,
                     plotOutput(ns("rank"), height = "600px"),
                     downloadButton(ns("download_rank_svg"), "Download as SVG"),
                     downloadButton(ns("download_rank_pdf"), "Download as PDF"),
                     br(), br()
                   )
                 )
        ), # Close 'Tools' section
        tabPanel(title = 'DMRtool',
                 # Heatmap section
                 h3("Heatmap of Tumoral Fraction vs Tools"),
                 sidebarLayout(
                   sidebarPanel(width = 3,
                     checkboxGroupInput(
                       ns("heatmap_tools_select"),
                       label = "Select Deconvolution Tools:",
                       choices = NULL,  
                       selected = NULL
                     ),
                     selectInput(
                       ns("heatmap_dmrtool_select"),
                       label = "Select DMR Tool:",
                       choices = NULL, 
                       selected = NULL
                     )
                   ),
                   mainPanel(width = 8,
                     plotOutput(ns("heatmap"), height = "600px"),
                     br(),
                     downloadButton(ns("download_heatmap_svg"), "Download as SVG"),
                     downloadButton(ns("download_heatmap_pdf"), "Download as PDF"),
                     downloadButton(ns("download_heatmap_df"), "Download data"),
                     br(), br()
                   )
                 )

      )# Close 'DMRtool' section 
      ),# Close tabsetPanel
      
      # Go to top of the page
      lapply(1:100, function(x) br()),
      spsGoTop("default")
    )# Close fluidPage
  ) # close TabPanel 
  
} # Close metricsTabUI


metricsTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## 1. Import the dataset and preprocess
    # Load the data
    res_limma <- read.csv("data/Results_20M_limma.csv", row.names = 1)
    res_dmrfinder <- read.csv("data/Results_20M_DMRfinder.csv", row.names = 1)
    res_wgbstools <- read.csv("data/Results_20M_wgbstools.csv", row.names = 1)
    res_cibersort_dmrfinder <- read.csv("data/Results_20M_DMRfinder_cibersort.csv", row.names = 1)
    metadata <- read.csv("data/SamplesMetadata.csv", sep = "\t")[, c("Sample", "Exp.nbl")]

    # Combine datasets with metadata
    bench <- rbind(res_dmrfinder, res_limma, res_wgbstools, res_cibersort_dmrfinder)
    colnames(metadata) <- c("sample", "expected_fraction")
    bench <- as.data.frame(merge(bench, metadata, by = "sample"))
    
    bench <- subset(
      bench,
      bench$reference == "reference_11healthy_9nbl" & 
        bench$expected_fraction %in% c(0, 0.0001, 0.001, 0.003, 0.007, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5)
      )
    
    bench$nbl <- round(bench$nbl, 4)
    bench$sample <- str_trim(bench$sample, side = c("both", "left", "right"))
    bench$tool <- str_trim(bench$tool, side = c("both", "left", "right"))
    bench$DMRtool <- str_trim(bench$DMRtool, side = c("both", "left", "right"))
    bench$sample <- bench$sample %>% str_replace("_R1_001_val_1_bismark_bt2_pe", "")
    bench <- as.data.frame(unique(bench))
    
    print(head(bench))
    print(str(bench))
    
    # 2. Functions
    # RMSE
    rmse <- function(actual, predicted) {
      round(sqrt(mean((actual - predicted)^2)),4)
    }
    # Normalized RMSE (NRMSE)
    nrmse <- function(actual, predicted) {
      round(sqrt(mean((actual - predicted))^2)/mean(actual),4)
    }
    # Spearman's rank correlation coefficient (SCC)
    scc <- function(actual, predicted) {
      cor(actual, predicted, method = "spearman")
    }
    # MCC
    mcc <- function(y_true,y_pred) {
      TP <- as.numeric(sum(y_true > 0 & y_pred > 0 ))
      TN <- as.numeric(sum(y_true == 0 & y_pred == 0))
      FP <- as.numeric(sum(y_true == 0 & y_pred > 0))
      FN <- as.numeric(sum(y_true > 0 & y_pred == 0))
      
      numerator <- (TP * TN) - (FP * FN)
      denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
      if (denominator == 0) return(0)
      return(numerator / denominator)
    }
    aupr.obj <- function(true_labels, predicted_scores) {
      true_labels[true_labels>0] <- 1
      
      # Calculate precision-recall curve and AUPR
      pr <- pr.curve(scores.class0 = predicted_scores, weights.class0 = true_labels, curve = T)
      
      return(pr)
    }
    roc.obj <- function(true_labels, predicted_scores) {
      true_labels[true_labels>0] <- 1
      roc_obj <- roc(true_labels, predicted_scores)
      return(roc_obj)
    }

    
    # Populate dropdowns and checkboxes
    updateSelectInput(session, "aucroc_tool_select", 
                      choices = sort(unique(bench$tool)), 
                      selected = sort(unique(bench$tool))[1])
    
    updateSelectInput(session, "aucroc_dmrtool_select", 
                      choices = sort(unique(bench$DMRtool)), 
                      selected = sort(unique(bench$DMRtool))[1])
    
    updateCheckboxGroupInput(session, "rank_tools_select", 
                             choices = sort(unique(bench$tool)), 
                             selected = sort(unique(bench$tool)))
    updateCheckboxGroupInput(session, "rank_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))

        ## 3. Visualisations
    ############################################################################
    ## Boxplot predictions for each tumoral fraction
    # Dropdowns and checkboxes boxplot 
    updateSelectInput(session, "boxplot_fraction_select", 
                      choices = sort(unique(bench$expected_fraction[bench$expected_fraction != 0])), 
                      selected = sort(unique(bench$expected_fraction[bench$expected_fraction != 0]))[1])
    updateCheckboxGroupInput(session, "boxplot_tools_select", 
                             choices = sort(unique(bench$tool)), 
                             selected = sort(unique(bench$tool)))
    updateCheckboxGroupInput(session, "boxplot_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    
    # Reactive expression for filtered data boxplot
    filtered_data_boxplot <- reactive({
      req(input$boxplot_fraction_select, input$boxplot_tools_select, input$boxplot_dmrtools_select)
      bench %>% 
        filter(expected_fraction == as.numeric(input$boxplot_fraction_select),
               tool %in% input$boxplot_tools_select,
               DMRtool %in% input$boxplot_dmrtools_select,
               expected_fraction != 0)  # Exclude expected_fraction == 0
    })
    
    # Function to create the boxplot
    create_boxplot_TF <- function(data, fraction) {
      # Rank the tools by median difference
      median_diff <- bench %>%
        filter(expected_fraction == fraction) %>%
        group_by(tool, DMRtool) %>%
        summarise(Diff = abs(median(expected_fraction) - median(nbl))) %>%
        group_by(tool) %>%
        summarise(Mean = mean(Diff, na.rm = TRUE)) %>%
        arrange(Mean)
      
      # Reorder the tools globally
      data <- data %>%
        mutate(tool = factor(tool, levels = median_diff$tool))

       # Ensure DMRtool is a factor
      data$DMRtool <- as.factor(data$DMRtool)

      # Barplot
      ggplot(data, aes(x = tool, y = nbl, fill = DMRtool, color = DMRtool)) +
        geom_boxplot(position = position_dodge(width = 0.75), outlier.color = "gray40", alpha = 0.6) +
        geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), size = 0.8, alpha = 0.5) +
        geom_hline(yintercept = fraction, color = "red", linetype = "dashed") +
        scale_x_discrete(labels = function(x) str_replace_all(x, "_", " ")) + 
        labs(
          x = "",
          y = "Tumoral fraction"
        ) + theme_benchmarking + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        custom_color_manual + custom_fill_manual 
      }

    # Render the boxplot in UI using the function
    output$boxplot_TF <- renderPlotly({
      data <- filtered_data_boxplot()
      req(nrow(data) > 0)
      plot <- create_boxplot_TF(data, as.numeric(input$boxplot_fraction_select))
      ggplotly(plot) %>% 
        layout(boxmode= "group")
      })
    
    # Save boxplot as svg and pdf
    download_boxplot <- function(ext) {
      downloadHandler(
        filename = function() paste("boxplots_tools_fraction_", input$boxplot_fraction_select, "_", Sys.Date(), ".", ext, sep = ""),
        content = function(file) {
          data <- filtered_data_boxplot()
          req(nrow(data) > 0)
          ggsave(file, plot = create_boxplot_TF(data, as.numeric(input$boxplot_fraction_select)),
                 width = 10, height = 6, dpi = 300, device = ext)
        }
      )
    }
    output$download_boxplot_TF_svg <- download_boxplot("svg")
    output$download_boxplot_TF_pdf <- download_boxplot("pdf")  
    
    # Save dataframe boxplot as csv
    output$download_boxplot_TF_df <- downloadHandler(
      filename = function() {
        paste("boxplot_data_fraction_", as.numeric(input$boxplot_fraction_select), "_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- filtered_data_boxplot()
        req(nrow(data) > 0)  
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    ############################################################################ 
    ## RMSE plot
    # Dropdowns and checkboxes RMSE plot 
    updateSelectInput(session, "rmse_tool_select", 
                      choices = sort(unique(bench$tool)), 
                      selected = sort(unique(bench$tool))[1])
    updateCheckboxGroupInput(session, "rmse_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    
    # RMSE Data Filtering
    filtered_data_rmse <- reactive({
      req(input$rmse_tool_select, input$rmse_dmrtools_select)
      bench %>%
        filter(tool == input$rmse_tool_select, 
               expected_fraction != 0)  # Exclude expected_fraction == 0
    })    
    
    compute_rmse_data <- function(data, dmrtools) {
      data %>%
        filter(DMRtool %in% dmrtools) %>%
        group_by(DMRtool, expected_fraction) %>%
        summarise(RMSE = nrmse(expected_fraction, nbl), .groups = "drop")
    }
    
    # Create a function to generate the RMSE plot
    create_plot_rmse <- function(data, tool, dmrtools) {

      data <- compute_rmse_data(data, dmrtools) %>%
        mutate(tooltip_text = paste("DMRtool:", DMRtool, "<br>Expected Fraction:", expected_fraction, "<br>nRMSE:", round(RMSE, 3)))
      
      # Plot 
      ggplot(data, aes(x = factor(expected_fraction), y = RMSE, color = DMRtool, shape = DMRtool, text = tooltip_text)) +
        geom_point(size = 3, alpha = 0.8) +
        labs(
          #title = paste("nRMSE for Tool:", tool),
          x = "Expected Fraction",
          y = "nRMSE",
          color = "DMRtool",
          shape = "DMRtool"
        ) + theme_benchmarking +  
        scale_y_continuous(expand = expansion(mult = 0.05))  +
        custom_color_manual + 
        custom_shape_manual
    }

    # Render the RMSE plot in UI using the function
    output$rmse_plot <- renderPlotly({
      data <- filtered_data_rmse()
      req(nrow(data) > 0)
      plot <- create_plot_rmse(data, input$rmse_tool_select, input$rmse_dmrtools_select)
      ggplotly(plot, tooltip = "text") 
    })
    
    # Save rmse plot as svg and pdf
    download_rmse_plot <- function(ext) {
      downloadHandler(
        filename = function() paste(input$rmse_tool_select, "_vs_fractions_rmse_", Sys.Date(), ".", ext, sep = ""),
        content = function(file) {
          data <- filtered_data_rmse()
          req(nrow(data) > 0)
          ggsave(file, plot = create_plot_rmse(data, input$rmse_tool_select, input$rmse_dmrtools_select),
                 width = 10, height = 6, dpi = 300, device = ext)
        }
      )
    }
    output$download_rmse_plot_svg <- download_rmse_plot("svg")
    output$download_rmse_plot_pdf <- download_rmse_plot("pdf")
    
    # Save dataframe rmse plot as csv
    output$download_rmse_plot_df <- downloadHandler(
      filename = function() {
        paste(input$rmse_tool_select, "_vs_fractions_rmse_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- filtered_data_rmse()
        req(nrow(data) > 0)  
        rmse_data <- compute_rmse_data(data, input$rmse_dmrtools_select)
        write.csv(rmse_data, file, row.names = FALSE)
      }
    )
    

    ############################################################################ 
    ## RMSE comparison Plot
    # Dropdowns and checkboxes RMSE comparison plot 
    updateSelectInput(session, "rmse_comparison_fraction_select", 
                      choices = sort(unique(bench$expected_fraction[bench$expected_fraction != 0])), 
                      selected = sort(unique(bench$expected_fraction[bench$expected_fraction != 0]))[1])
    updateCheckboxGroupInput(session, "rmse_comparison_tools_select", 
                             choices = sort(unique(bench$tool)), 
                             selected = sort(unique(bench$tool)))
    updateCheckboxGroupInput(session, "rmse_comparison_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    
    # RMSE tool comparison Data Filtering
    filtered_data_rmse_comparison <- reactive({
      req(input$rmse_comparison_fraction_select, input$rmse_comparison_tools_select, input$rmse_comparison_dmrtools_select)
      bench %>%
        filter(expected_fraction == as.numeric(input$rmse_comparison_fraction_select),
               expected_fraction != 0, # Exclude expected_fraction == 0
               DMRtool %in% input$rmse_comparison_dmrtools_select,
               tool %in% input$rmse_comparison_tools_select
        )
    })
    
    
    # Create a function to generate the RMSE comparison plot
    create_rmse_comparison_plot <- function(data, tool, fraction, dmrtools) {
      
      # Filter data based on tool and DMRtools
      plot_data <- data %>%
        filter(expected_fraction == fraction) %>%
        #filter(tool != 'Methyl_Resolver') %>%
        group_by(tool, DMRtool) %>%
        summarise(RMSE = rmse(expected_fraction, nbl), .groups = "drop")
      
      # Rank the tools by mean RMSE across DMRtools
      median_diff <- data %>% 
        filter(expected_fraction == fraction) %>%
        group_by(tool, DMRtool) %>% 
        summarise(RMSE = rmse(expected_fraction, nbl)) %>%
        group_by(tool) %>%
        summarise(Mean = mean(RMSE, na.rm = TRUE)) %>%
        arrange(Mean)
      
      # Reorder the tools globally
      plot_data <- plot_data %>%
        mutate(tool = factor(tool, levels = median_diff$tool)) %>%
        mutate(tooltip_text = paste("DMRtool:", DMRtool, "<br>nRMSE:", round(RMSE, 3)))
      
      # Generate the plot
      ggplot(plot_data, aes(x = RMSE, y = fct_reorder(tool, RMSE), color = DMRtool, shape = DMRtool, text = tooltip_text)) +
        geom_point(size = 3, alpha = 0.8) +
        scale_y_discrete(labels = function(y) str_replace_all(y, "_", " ")) +
        labs(
          #title = paste("RMSE vs Tool (Expected Fraction:", fraction, ")"),
          x = "nRMSE",
          y = "",
          color = "DMRtool",
          shape = "DMRtool"
        ) + theme_benchmarking + scale_x_continuous(expand = expansion(mult = 0.05)) + 
        custom_color_manual + 
        custom_shape_manual 
    }
    
        # Render the RMSE comparison plot in UI using the function
    output$rmse_comparison <- renderPlotly({
      data <- filtered_data_rmse_comparison()
      req(nrow(data) > 0)
      plot <- create_rmse_comparison_plot(data, input$rmse_comparison_tools_select, input$rmse_comparison_fraction_select, input$rmse_comparison_dmrtools_select)
      ggplotly(plot, tooltip = "text")
    })
    
    # Save RMSE comparison plot using the function
    download_rmse_comparison_plot <- function(ext) {
      downloadHandler(
        filename = function() paste("ranking_tools_fraction_", as.numeric(input$rmse_comparison_fraction_select), "_", Sys.Date(), ".", ext, sep = ""),
        content = function(file) {
          data <- filtered_data_rmse_comparison()
          req(nrow(data) > 0)
          ggsave(file, plot = create_rmse_comparison_plot(data, input$rmse_comparison_fraction_select),
                 width = 10, height = 6, dpi = 300, device = ext)
        }
      )
    }
    output$download_rmse_comparison_svg <- download_rmse_comparison_plot("svg")
    output$download_rmse_comparison_pdf <- download_rmse_comparison_plot("pdf")
    
    # Save dataframe rmse comparison plot as csv
    output$download_rmse_comparison_df <- downloadHandler(
      filename = function() paste("ranking_tools_fraction_", as.numeric(input$rmse_comparison_fraction_select), "_", Sys.Date(), ".csv", sep = ""),
      content = function(file) {
        data <- filtered_data_rmse_comparison() %>%
          group_by(tool, DMRtool) %>%
          summarise(RMSE = rmse(expected_fraction, nbl), .groups = "drop") %>%
          select(tool, DMRtool, RMSE)
        req(nrow(data) > 0)
        write.csv(data, file, row.names = FALSE)
      }
    )
    

    ############################################################################ 
    ## Heatmap
    # Dropdowns and checkboxes heatmap
    updateCheckboxGroupInput(session, "heatmap_tools_select", 
                             choices = sort(unique(bench$tool)), 
                             selected = sort(unique(bench$tool)))    
    updateSelectInput(session, "heatmap_dmrtool_select", 
                      choices = sort(unique(bench$DMRtool)), 
                      selected = sort(unique(bench$DMRtool))[1])    
    
    
    # Create a function to generate the heatmap
    create_heatmap_plot <- function(data, tools, dmrtool) {
      # Filter data based on user selection
      plot_data <- data %>%
        filter(DMRtool == dmrtool, 
               expected_fraction != 0, 
               tool %in% tools) %>%
        group_by(tool, expected_fraction) %>%
        summarize(
          RMSE = 1 - mean(rmse(expected_fraction, nbl), na.rm = TRUE),
          .groups = "drop"
        )
      
      # Rank tools by median RMSE
      median_diff <- plot_data %>%
        group_by(tool) %>%
        summarize(Mean = abs(mean(RMSE))) %>%
        arrange(desc(Mean))
      
      # Reorder tools globally
      plot_data <- plot_data %>%
        mutate(tool = factor(tool, levels = median_diff$tool))
      

      # Define a function to determine text color based on RMSE value
      get_text_color <- function(rmse_value) {
        # Handle NA values: default to black text for missing values
        if (is.na(rmse_value)) {
          return("black") 
        }
        
        # If RMSE is low, the tile is white and should have black text
        if (rmse_value <= 0.5) {
          return("black")
        } else {
          return("white")
        }
      }
      
      
      # Create a new column for the labels, where NA values are converted to the string 'NA'
      plot_data$label <- ifelse(is.na(plot_data$RMSE), "NA", round(plot_data$RMSE, 4))
      
      # Precompute text colors for each RMSE value
      plot_data$text_color <- sapply(plot_data$RMSE, get_text_color)
      
      
      # Create and return the heatmap plot
      ggplot(plot_data, aes(x = tool, y = factor(expected_fraction), fill = RMSE)) +
        geom_tile() +
        geom_text(aes(
          label = label, 
          color = text_color  # Apply dynamic text color based on tile fill
        ), size = 4) +
        scale_fill_gradient(low = "white", high = "gray10", limits = c(0,1) ) + # Greyscale color scale
        scale_color_identity() + 
        scale_x_discrete(labels = function(x) str_replace_all(x, "_", " "))+
        labs(
          x = "",
          y = "Tumoral Fraction"
        ) +
        theme(
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = "gray10"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.text.y = element_text(size = 12, color = "gray10"),
          axis.title.x = element_text(size = 14 , color = "gray10"),
          axis.title.y = element_text(size = 14, color = "gray10"),
          legend.title = element_text(size = 14, color = "gray10"),
          legend.text = element_text(size = 12, color = "gray10"),
          axis.ticks = element_line(color = "gray50")
        )
    }
    
    output$heatmap <- renderPlot({
      req(input$heatmap_tools_select, input$heatmap_dmrtool_select) # Ensure inputs are provided
      create_heatmap_plot(bench, input$heatmap_tools_select, input$heatmap_dmrtool_select)
    })
    
    # Save heatmap using the function
    download_heatmap_plot <- function(ext) {
      downloadHandler(
        filename = function() paste("heatmap_tools_", input$heatmap_dmrtool_select, "_", Sys.Date(), ".", ext, sep = ""),
        content = function(file) {
          req(input$heatmap_tools_select, input$heatmap_dmrtool_select)
          plot <- create_heatmap_plot(bench, input$heatmap_tools_select, input$heatmap_dmrtool_select)
          ggsave(file, plot = plot, width = 10, height = 6, dpi = 300, device = ext)

        }
      )
    }
    output$download_heatmap_svg <- download_heatmap_plot("svg")
    output$download_heatmap_pdf <- download_heatmap_plot("pdf")    
    
    # Save dataframe heatmap as csv
    output$download_heatmap_df <- downloadHandler(
      filename = function() paste("heatmap_tools_", input$heatmap_dmrtool_select, "_", Sys.Date(), ".csv", sep = ""),
      content = function(file) {
        data <- bench %>%
          filter(DMRtool == input$heatmap_dmrtool_select, expected_fraction != 0, tool %in% input$heatmap_tools_select) %>%
          group_by(tool, expected_fraction) %>%
          summarize(RMSE = 1 - mean(rmse(expected_fraction, nbl), na.rm = TRUE), .groups = "drop")
        write.csv(data, file, row.names = FALSE)
      }
    )

  }) # Close moduleServer
} # Close metricsTabServer    
    
    
    
    
    