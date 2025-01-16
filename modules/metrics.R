metricsTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Metrics",
    fluidPage(
      h2("Benchmarking Metrics"),
      p("We considered 3 different key metrics: the root-mean-squared error (RMSE), the Jensen--Shannon divergence (JSD) and the Spearman's rank correlation coefficient (ρ). To create an overall benchmarking score against which to compare the deconvolution tools, we min-max scaled the metrics and computed the geometric mean of the three metrics to obtain the final benchmarking scores. Finally, we ranked the tools based on these scores."),
      p("Below, you can find the computed metrics and visualizations."),

      # Row to place Select Tumoral Fraction and Select Tools side by side
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
      ),
      
      # Output: Boxplots of the predictions for each tumoral fraction
      h3("Boxplots of the predictions for each tumoral fraction"),
      fluidRow(
        column(8, 
               plotOutput(ns("boxplot_TF"), height = "600px"),
               downloadButton(ns("download_boxplot_TF"), "Save Plot")
        )
      ),

      # New UI section for the RMSE plot - selection of 1 tool and multiple DMR tools
      h3("Performance (RMSE) per Tumoral Fractions"),
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
      ),

      # Output: RMSE plot based on selected tool and DMRtools
      fluidRow(
        column(8, 
               plotOutput(ns("rmse_plot"), height = "600px"),
               downloadButton(ns("download_rmse_plot"), "Save Plot")
        )
      )
    )
  )
}

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
    bench$sample <- str_trim(bench$sample)
    bench$tool <- str_trim(bench$tool)
    bench$DMRtool <- str_trim(bench$DMRtool)
    bench$sample <- bench$sample %>% str_replace("_R1_001_val_1_bismark_bt2_pe", "")
    bench <- as.data.frame(unique(bench))
    
    # Populate dropdowns and checkboxes
    updateSelectInput(session, "boxplot_fraction_select", choices = sort(unique(bench$expected_fraction[bench$expected_fraction != 0])), selected = sort(unique(bench$expected_fraction[bench$expected_fraction != 0]))[1])
    updateCheckboxGroupInput(session, "boxplot_tools_select", choices = sort(unique(bench$tool)), selected = sort(unique(bench$tool)))
    updateCheckboxGroupInput(session, "boxplot_dmrtools_select", choices = sort(unique(bench$DMRtool)), selected = sort(unique(bench$DMRtool)))
    
    updateSelectInput(session, "rmse_tool_select", choices = sort(unique(bench$tool)), selected = sort(unique(bench$tool))[1])
    updateCheckboxGroupInput(session, "rmse_dmrtools_select", choices = sort(unique(bench$DMRtool)), selected = sort(unique(bench$DMRtool)))
    
    ## 2. Reactive expression for filtered data boxplot
    filtered_data_boxplot <- reactive({
      req(input$boxplot_fraction_select, input$boxplot_tools_select, input$boxplot_dmrtools_select)
      bench %>% 
        filter(expected_fraction == as.numeric(input$boxplot_fraction_select),
               tool %in% input$boxplot_tools_select,
               DMRtool %in% input$boxplot_dmrtools_select,
               expected_fraction != 0)  # Exclude expected_fraction == 0
      
    })
    
    ## 3. Boxplot predictions for each tumoral fraction
    # Function to create the boxplot
    create_boxplot_TF <- function(data, fraction) {
      median_diff <- bench %>%
        filter(expected_fraction == fraction) %>%
        group_by(tool, DMRtool) %>%
        summarise(Diff = abs(median(expected_fraction) - median(nbl))) %>%
        group_by(tool) %>%
        summarise(Mean = mean(Diff, na.rm = TRUE)) %>%
        arrange(Mean)
      
      data <- data %>%
        mutate(tool = factor(tool, levels = median_diff$tool))
      
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

    ## 4. RMSE Data Filtering
    filtered_data_rmse <- reactive({
      req(input$rmse_tool_select, input$rmse_dmrtools_select)
      bench %>%
        filter(tool == input$rmse_tool_select, 
               DMRtool %in% input$rmse_dmrtools_select,
               expected_fraction != 0)  # Exclude expected_fraction == 0
    })    
    
    ## 5. RMSE plot
    # Create a function to generate the RMSE plot
    create_plot_rmse <- function(data, tool, dmrtools) {
      nrmse <- function(actual, predicted) {
        round(sqrt(mean((actual - predicted)^2)) / mean(actual), 4)
      }
      
      plot_data <- data %>%
        filter(tool == tool, DMRtool %in% dmrtools) %>%
        group_by(DMRtool, expected_fraction) %>%
        summarise(RMSE = nrmse(expected_fraction, nbl), .groups = "drop")
      
      ggplot(plot_data, aes(x = factor(expected_fraction), y = RMSE, color = DMRtool, shape = DMRtool)) +
        geom_point(size = 3, alpha = 0.8) +
        labs(
          title = paste("RMSE for Tool:", tool),
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
      req(input$rmse_tool_select,input$rmse_dmrtools_select) 
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
  }) # Closing bracket for moduleServer
} # Closing bracket for metricsTabServer    
    
    
    
    
    