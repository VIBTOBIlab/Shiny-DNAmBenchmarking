metricsTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Metrics",
    fluidPage(
      h2("Benchmarking Metrics"),
      p("We considered 3 different key metrics: the root-mean-squared error (RMSE), the area under the curve (AUC-ROC) and the Spearman's rank correlation coefficient (ρ). To create an overall benchmarking score against which to compare the deconvolution tools, we min-max scaled the metrics and computed the geometric mean of the three metrics to obtain the final benchmarking scores. Finally, we ranked the tools based on these scores."),
      p("Below, you can find the computed metrics and visualizations."),
      br(),
      
      # Selection: Boxplots of the predictions for each tumoral fraction
      h3("Boxplots of the predictions for each tumoral fraction"),
      fluidRow(
        # Drop down menu for selecting a tumoral fraction
        column(2,
               selectInput(
                 ns("boxplot_fraction_select"), 
                 label = "Select Tumoral Fraction:",
                 choices = NULL,
                 selected = NULL
               )
        ),

        # Checkbox for selecting tools to display
        column(2,
               checkboxGroupInput(
                 ns("boxplot_tools_select"),
                 label = "Select Deconvolution Tools:",
                 choices = NULL,  
                 inline = FALSE,
                 selected = NULL
               )
        ),

        # Checkbox for selecting DMRtools to display
        column(2,
               checkboxGroupInput(
                 ns("boxplot_dmrtools_select"),
                 label = "Select DMR Tools:",
                 choices = NULL,  
                 inline = FALSE,
                 selected = NULL
               )        
        )
      ), # Close fluidRow
      
      # Output: Boxplots of the predictions for each tumoral fraction
      fluidRow(
        column(8, 
               plotOutput(ns("boxplot_TF"), height = "600px"),
               downloadButton(ns("download_boxplot_TF"), "Save Plot")
        ),

        ), # Close fluidRow
      br(),
      
      # Selection: RMSE plot based on selected tool and DMRtools
      h3("Performance (nRMSE) per Tumoral Fractions"),
      p("We will use the normalized RMSE (NRMSE) in order to make the score size-free: otherwise, increasing the expected value will also determine an increase in RMSE, giving a (wrong) perception that tools perform worse when increasing the tumoral fraction."),
      fluidRow(
        column(4, 
               selectInput(ns("rmse_tool_select"), 
                           label = "Select Deconvolution Tool:",
                           choices = NULL,  
                           selected = NULL)
        ),
        
        column(4,
               checkboxGroupInput(ns("rmse_dmrtools_select"),
                                  label = "Select DMR Tools:",
                                  choices = NULL,  
                                  selected = NULL)
        )
      ), # Close FluidRow

      # Output: RMSE plot based on selected tool and DMRtools
      fluidRow(
        column(8, 
               plotOutput(ns("rmse_plot"), height = "600px"),
               downloadButton(ns("download_rmse_plot"), "Save Plot")
        )
      ),# Close FluidRow
      br(),
      
      # Selection: Tool ranks based on RMSE per tumoral fraction
      h3("Tool ranks based on RMSE per tumoral fraction"),
      fluidRow(
        # Drop down menu for selecting a tumoral fraction
        column(2,
               selectInput(
                 ns("rmse_comparison_fraction_select"), 
                 label = "Select Tumoral Fraction:",
                 choices = NULL,
                 selected = NULL
               )
        ),
        column(2,
               checkboxGroupInput(
                 ns("rmse_comparison_tools_select"),
                 label = "Select Deconvolution Tools:",
                 choices = NULL,  
                 inline = FALSE,
                 selected = NULL
               )
        ),
        column(2,
               checkboxGroupInput(
                 ns("rmse_comparison_dmrtools_select"),
                 label = "Select DMR Tools:",
                 choices = NULL, 
                 inline = FALSE,
                 selected = NULL)
        ),

      ),# Close FluidRow
        
      # Output: Tool ranks based on RMSE per tumoral fraction
      fluidRow(
        column(8, 
               plotOutput(ns("rmse_comparison"), height = "600px"),
               downloadButton(ns("download_rmse_comparison"), "Save Plot")
        )
      ) # Close fluidRow
      
      
    ) # Close fluidPage
  ) # Close tabPanel
} # close metricsTabUI

metricsTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## 1. Import the dataset and preprocess
    # Load the data (code remains the same as provided)
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
    
    
    
    # 3. Populate dropdowns and checkboxes
    updateSelectInput(session, "boxplot_fraction_select", 
                      choices = sort(unique(bench$expected_fraction[bench$expected_fraction != 0])), 
                      selected = sort(unique(bench$expected_fraction[bench$expected_fraction != 0]))[1])
    updateCheckboxGroupInput(session, "boxplot_tools_select", 
                             choices = sort(unique(bench$tool)), 
                             selected = sort(unique(bench$tool)))
    updateCheckboxGroupInput(session, "boxplot_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    
    updateSelectInput(session, "rmse_tool_select", 
                      choices = sort(unique(bench$tool)), 
                      selected = sort(unique(bench$tool))[1])
    updateCheckboxGroupInput(session, "rmse_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    
    updateSelectInput(session, "rmse_comparison_fraction_select", 
                      choices = sort(unique(bench$expected_fraction[bench$expected_fraction != 0])), 
                      selected = sort(unique(bench$expected_fraction[bench$expected_fraction != 0]))[1])
    updateCheckboxGroupInput(session, "rmse_comparison_tools_select", 
                             choices = sort(unique(bench$tool)), 
                             selected = sort(unique(bench$tool)))
    updateCheckboxGroupInput(session, "rmse_comparison_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    
    ## 4. Boxplot predictions for each tumoral fraction
    
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
      
      # Reorder the tools glabally
      data <- data %>%
        mutate(tool = factor(tool, levels = median_diff$tool))
      
      # Barplot
      ggplot(data, aes(x = tool, y = nbl, color = DMRtool)) +
        geom_boxplot() +
        geom_hline(yintercept = fraction, color = "red", linetype = "dashed") +
        labs(
          title = paste("Expected Fraction:", fraction),
          x = "",
          y = "Tumoral fraction"
        ) +
        theme(
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14)
        ) +
        scale_color_manual(values = c("limma" = "#F8766D", "wgbs_tools" = "#00BA38", "DMRfinder" = "#619CFF"))
    }
    
    # Render the boxplot in UI using the function
    output$boxplot_TF <- renderPlot({
      data <- filtered_data_boxplot()
      req(nrow(data) > 0)
      create_boxplot_TF(data, as.numeric(input$boxplot_fraction_select))
    })
    
    # Save boxplot using the function
    output$download_boxplot_TF <- downloadHandler(
      filename = function() {
        paste("boxplots_tools_fraction_", as.numeric(input$boxplot_fraction_select), "_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        data <- filtered_data_boxplot()
        req(nrow(data) > 0)
        plot <- create_boxplot_TF(data, as.numeric(input$boxplot_fraction_select))
        ggsave(file, plot = plot, width = 10, height = 6, dpi = 300)
      }
    )
    ## 6. RMSE plot
    # RMSE Data Filtering
    filtered_data_rmse <- reactive({
      req(input$rmse_tool_select, input$rmse_dmrtools_select)
      bench %>%
        filter(tool == input$rmse_tool_select, 
               expected_fraction != 0)  # Exclude expected_fraction == 0
    })    
    
    # Create a function to generate the RMSE plot
    create_plot_rmse <- function(data, tool, dmrtools) {

      plot_data <- data %>%
        filter(tool == tool, DMRtool %in% dmrtools) %>%
        group_by(DMRtool, expected_fraction) %>%
        summarise(RMSE = nrmse(expected_fraction, nbl), .groups = "drop")
      
      # Plot 
      ggplot(plot_data, aes(x = factor(expected_fraction), y = RMSE, color = DMRtool, shape = DMRtool)) +
        geom_point(size = 3, alpha = 0.8) +
        labs(
          title = paste("nRMSE for Tool:", tool),
          x = "Expected Fraction",
          y = "RMSE"
        ) +
        theme(
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14)
        ) +
        scale_color_manual(values = c("limma" = "#F8766D", "wgbs_tools" = "#00BA38", "DMRfinder" = "#619CFF"))
    }
    
    # Render the RMSE plot in UI using the function
    output$rmse_plot <- renderPlot({
      data <- filtered_data_rmse()
      req(nrow(data) > 0)
      create_plot_rmse(data, input$rmse_tool_select, input$rmse_dmrtools_select)
    })
    
    # Save RMSE plot using the function
    output$download_rmse_plot <- downloadHandler(
      filename = function() {
        paste(input$rmse_tool_select, "_vs_fractions_rmse_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        data <- filtered_data_rmse()
        req(nrow(data) > 0)
        plot <- create_plot_rmse(data, input$rmse_tool_select, input$rmse_dmrtools_select)
        ggsave(file, plot = plot, width = 10, height = 6, dpi = 300)
      }
    )
    
    ## 7. RMSE comparison Plot
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
        mutate(tool = factor(tool, levels = median_diff$tool))
      
      # Generate the plot
      ggplot(plot_data, aes(x = RMSE, y = fct_reorder(tool, RMSE), color = DMRtool, shape = DMRtool)) +
        geom_point(size = 3, alpha = 0.8) +
        labs(
          title = paste("RMSE vs Tool (Expected Fraction:", fraction, ")"),
          x = "RMSE",
          y = ""
        ) +
        theme(
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14)
        ) +
        scale_color_manual(values = c("limma" = "#F8766D", "wgbs_tools" = "#00BA38", "DMRfinder" = "#619CFF"))
    }
    # Render the RMSE comparison plot in UI using the function
    output$rmse_comparison <- renderPlot({
      data <- filtered_data_rmse_comparison()
      req(nrow(data) > 0)
      create_rmse_comparison_plot(data, input$rmse_comparison_tools_select, input$rmse_comparison_fraction_select, input$rmse_comparison_dmrtools_select)
    })
    
    # Save RMSE comparison plot using the function
    output$download_rmse_comparison <- downloadHandler(
      filename = function() {
        paste("ranking_tools_fraction_",as.numeric(input$rmse_comparison_fraction_select),"_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        data <- filtered_data_rmse_comparison()
        req(nrow(data) > 0)
        plot <- create_rmse_comparison_plot(data, input$rmse_comparison_tools_select, input$rmse_comparison_fraction_select, input$rmse_comparison_dmrtools_select)
        ggsave(file, plot = plot, width = 10, height = 6, dpi = 300)
      }
    )    
    
    
  }) # Close moduleServer
} # Close metricsTabServer    
    
    
    
    
    