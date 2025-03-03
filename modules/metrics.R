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
                        ns("boxplot_depth_select"), 
                        label = "Depth",
                        choices = NULL,
                        selected = NULL
                      ),
                      selectInput(
                        ns("boxplot_approach_select"), 
                        label = "Approach",
                        choices = NULL,
                        selected = NULL
                      ),
                      selectInput(
                        ns("boxplot_fraction_select"), 
                        label = "Tumoral Fraction",
                        choices = NULL,
                        selected = NULL
                      ),
                      checkboxGroupInput(
                        ns("boxplot_tools_select"),
                        label = "Deconvolution Tools",
                        choices = NULL,  
                        selected = NULL
                      ),
                      checkboxGroupInput(
                        ns("boxplot_dmrtools_select"),
                        label = "DMR Tools",
                        choices = NULL,  
                        selected = NULL
                      )
                    ),
                    mainPanel( width = 9,
                      plotlyOutput(ns("boxplot_TF"), height = "600px"),
                      br(),
                      # downloadButton(ns("download_boxplot_TF_svg"), "Download as SVG"),
                      # downloadButton(ns("download_boxplot_TF_pdf"), "Download as PDF"),
                      downloadButton(ns("download_boxplot_TF_df"), "Download data"),
                      br(), br(), br()
                    )
                  ),
                  
                  tags$hr(), br(), br(),
                  
                  # nRMSE section
                  h3("Performance (nRMSE)"),
                  sidebarLayout(
                    sidebarPanel(width = 3,
                      selectInput(
                        ns("nrmse_depth_select"), 
                        label = "Depth",
                        choices = NULL,
                        selected = NULL
                        ),
                      selectInput(
                        ns("nrmse_approach_select"),
                        label = "Approach",
                        choices = NULL,
                        selected = NULL
                        ),                                 

                      selectInput(
                        ns("nrmse_tool_select"), 
                        label = "Deconvolution Tool",
                        choices = NULL,  
                        selected = NULL
                      ),
                      checkboxGroupInput(
                        ns("nrmse_dmrtools_select"),
                        label = "DMR Tools",
                        choices = NULL,  
                        selected = NULL
                      )
                    ),
                    mainPanel(width = 9,
                      plotlyOutput(ns("nrmse_plot"), height = "600px"),
                      br(),
                      # downloadButton(ns("download_nrmse_plot_svg"), "Download as SVG"),
                      # downloadButton(ns("download_nrmse_plot_pdf"), "Download as PDF"),
                      downloadButton(ns("download_nrmse_plot_df"), "Download data"),
                      br(), br(), br()
                    )
                  ),
                  
                 
                  tags$hr(), br(), br(),
                  

                  # AUC-ROC of tools at 4 low tumoral fractions
                 h3("AUC-ROC at different tumoral fractions"),
                 # First main panel for the initial AUC-ROC plot
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                selectInput(
                                  ns("aucroc_tool_select"),
                                  label = "Select Deconvolution Tool:",
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
                   mainPanel(width = 9,
                             plotlyOutput(ns("aucroc_plot"), height = "400px", width = "600px"),
                             br(),
                             # downloadButton(ns("download_aucroc_svg"), "Download as SVG"),
                             # downloadButton(ns("download_aucroc_pdf"), "Download as PDF"),
                             downloadButton(ns("download_aucroc_df"), "Download data"),
                             br(), br(), br()
                   )
                 ),
                 br(), br(),
                 # Second main panel for the complete AUC-ROC plot
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                checkboxGroupInput(
                                  ns("aucroc_complete_tools_select"),
                                  label = "Select Deconvolution Tools:",
                                  choices = NULL,
                                  selected = NULL
                                ),
                                selectInput(
                                  ns("aucroc_complete_dmrtool_select"),
                                  label = "Select DMR Tool:",
                                  choices = NULL,
                                  selected = NULL
                                )
                   ),
                   mainPanel(width = 9,
                             plotOutput(ns("aucroc_complete_plot"), height = "800px"),
                             br(),
                             downloadButton(ns("download_aucroc_complete_svg"), "Download as SVG"),
                             downloadButton(ns("download_aucroc_complete_pdf"), "Download as PDF"),
                             downloadButton(ns("download_aucroc_complete_df"), "Download data"),
                             br(), br(), br()
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
                    mainPanel(width = 9,
                      plotlyOutput(ns("rmse_comparison"), height = "600px"),
                      br(),
                      # downloadButton(ns("download_rmse_comparison_svg"), "Download as SVG"),
                      # downloadButton(ns("download_rmse_comparison_pdf"), "Download as PDF"),
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
                   mainPanel(width = 9,
                             plotlyOutput(ns("rank"), height = "600px"),
                             # downloadButton(ns("download_rank_svg"), "Download as SVG"),
                             # downloadButton(ns("download_rank_pdf"), "Download as PDF"),
                             downloadButton(ns("download_rank_df"), "Download data"),
                     
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
                   mainPanel(width = 9,
                     plotOutput(ns("heatmap"), height = "600px"),
                     br(),
                     downloadButton(ns("download_heatmap_svg"), "Download as SVG"),
                     downloadButton(ns("download_heatmap_pdf"), "Download as PDF"),
                     downloadButton(ns("download_heatmap_df"), "Download data"),
                     br(), br()
                   )
                 )

      ), # Close 'DMRtool' section 
      tabPanel(title = "Mean vs Median"),
      tabPanel(title = "LoD")
      
               
      ),# Close tabsetPanel
      
      # Go to top of the page
      lapply(1:100, function(x) br()),
      spsGoTop("default")
    )# Close fluidPage
  ) # close TabPanel 
  
} # Close metricsTabUI


metricsTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
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

    # Change names of references and tools
    # bench <- bench %>%
    #   mutate(
    #     # Replacing tool names 
    #     tool = case_when(
    #       tool == "EpiDISH_RPC" ~ "RPC",
    #       tool == "EpiDISH_CP_eq" ~ "Houseman's CP/QP w/equality",
    #       tool == "EpiDISH_CP_ineq" ~ "Houseman's CP/QP w/inequality",
    #       tool == "meth_atlas" ~ "MethAtlas",
    #       tool == "Methyl_Resolver" ~ "MethylResolver",
    #       tool == "PRMeth" ~ "NMF",
    #       TRUE ~ tool
    #     ),
    #     # Replacing reference values 
    #     reference = case_when(
    #       reference == "reference_11healthy_10nbl" ~ "all available cell lines",
    #       reference == "reference_11healthy_1nbl" ~ "matched cell line",
    #       reference == "reference_11healthy_9nbl" ~ "all cell lines except matched",
    #       reference == "reference_11healthy_4nbl" ~ "all adrenergic cell lines except matched",
    #       TRUE ~ reference  
    #     )
    #   )
    
    # Clean and format data 
    bench$nbl <- round(bench$nbl, 4) 
    bench$sample <- str_trim(bench$sample, side = c("both", "left", "right"))
    bench$tool <- str_trim(bench$tool, side = c("both", "left", "right"))
    bench <- as.data.frame(unique(bench))
    
    bench <- bench %>%
      mutate(across(c(reference, DMRtool, direction, top, collapse_approach, 
                      depth, min_cpgs, min_counts), as.factor))
    
    # print(head(bench))
    # print(str(bench))
    
    # 2. Functions
    # RMSE
    rmse <- function(actual, predicted) {
      round(sqrt(mean((actual - predicted)^2)),4)
    }
    # Normalized RMSE (NRMSE)
    # nrmse <- function(actual, predicted) {
    #   round(sqrt(mean((actual - predicted))^2)/mean(actual),4)
    # }
    # Spearman's rank correlation coefficient (SCC)
    scc <- function(actual, predicted) {
      cor(actual, predicted, method = "spearman")
    }
    
    roc.obj <- function(true_labels, predicted_scores) {
      true_labels[true_labels>0] <- 1
      roc_obj <- roc(true_labels, predicted_scores)
      return(roc_obj)
    }
    
    
    # # MCC
    # mcc <- function(y_true,y_pred) {
    #   TP <- as.numeric(sum(y_true > 0 & y_pred > 0 ))
    #   TN <- as.numeric(sum(y_true == 0 & y_pred == 0))
    #   FP <- as.numeric(sum(y_true == 0 & y_pred > 0))
    #   FN <- as.numeric(sum(y_true > 0 & y_pred == 0))
    #   
    #   numerator <- (TP * TN) - (FP * FN)
    #   denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
    #   if (denominator == 0) return(0)
    #   return(numerator / denominator)
    # }
    # aupr.obj <- function(true_labels, predicted_scores) {
    #   true_labels[true_labels>0] <- 1
    #   
    #   # Calculate precision-recall curve and AUPR
    #   pr <- pr.curve(scores.class0 = predicted_scores, weights.class0 = true_labels, curve = T)
    #   
    #   return(pr)
    # }

    ## 3. Visualizations Benchmarking Results
    ############################################################################
    ## Boxplot predictions for each tumoral fraction
    # Dropdowns and checkboxes boxplot 
    updateSelectInput(session, "boxplot_depth_select", 
                      choices = sort(unique(bench$depth)), 
                      selected = sort(unique(bench$depth))[1])
    updateSelectInput(session, "boxplot_approach_select", 
                      choices = sort(unique(bench$collapse_approach)), 
                      selected = sort(unique(bench$collapse_approach))[1])
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
      req(input$boxplot_depth_select, input$boxplot_approach_select, input$boxplot_fraction_select,
          input$boxplot_tools_select, input$boxplot_dmrtools_select)
      
      bench %>%
        filter(depth == input$boxplot_depth_select,                # Filter by depth
               collapse_approach == input$boxplot_approach_select,  # Filter by approach
               expected_fraction == as.numeric(input$boxplot_fraction_select),  # Filter by fraction
               tool %in% input$boxplot_tools_select,                   # Filter by tools
               DMRtool %in% input$boxplot_dmrtools_select,             # Filter by DMRtools
               expected_fraction != 0) # Exclude expected_fraction == 0
    })
    
    
    # Function to create the boxplot
    create_boxplot_TF <- function(data, depth, approach, fraction ) {
      # Rank the tools by median difference
      median_diff <- data %>%
        group_by(tool, DMRtool) %>%
        summarise(Diff = abs(median(expected_fraction) - median(nbl)), .groups = 'drop') %>%
        group_by(tool) %>%
        summarise(Mean = mean(Diff, na.rm = TRUE), .groups = 'drop') %>%
        arrange(Mean)
      # Tools with the smallest mean difference (more accurate) are placed first.
      # Tools with the largest mean difference (less accurate) are placed last.
      
      #print(median_diff)
      
      # Reorder the tools globally
      data <- data %>%
        mutate(tool = factor(tool, levels = median_diff$tool))

      # Create hover text for jitter points
      # data <- data %>%
      #   mutate(hover_text = paste0("Tool: ", tool, "<br>",
      #                              "nbl: ", nbl, "<br>",
      #                              "DMRtool: ", DMRtool))
      
      # Boxplot
      ggplot(data, aes(x = tool, y = nbl, fill = DMRtool, color = DMRtool)) +
        geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +
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
      plot <- create_boxplot_TF(data, input$boxplot_depth_select, input$boxplot_approach_select, as.numeric(input$boxplot_fraction_select))
      ggplotly(plot) %>% 
        layout(boxmode= "group") %>% 
        config(toImageButtonOptions = list(format = "svg",
                                           filename = paste("boxplot_",input$boxplot_depth_select,"_approach_",input$boxplot_approach_select,"_fraction_", input$boxplot_fraction_select, "_", Sys.Date())
        ))
      })
    
    # Save boxplot as svg and pdf
    # download_boxplot <- function(ext) {
    #   downloadHandler(
    #     filename = function() paste("boxplots_tools_fraction_", input$boxplot_fraction_select, "_", Sys.Date(), ".", ext, sep = ""),
    #     content = function(file) {
    #       data <- filtered_data_boxplot()
    #       req(nrow(data) > 0)
    #       ggsave(file, plot = create_boxplot_TF(data, as.numeric(input$boxplot_fraction_select)),
    #              width = 10, height = 6, dpi = 300, device = ext)
    #     }
    #   )
    # }
    # output$download_boxplot_TF_svg <- download_boxplot("svg")
    # output$download_boxplot_TF_pdf <- download_boxplot("pdf")  
    
    # Save dataframe boxplot as csv
    output$download_boxplot_TF_df <- downloadHandler(
      filename = function() {
        paste("boxplot_",input$boxplot_depth_select,"_approach_",input$boxplot_approach_select,"_fraction_", input$boxplot_fraction_select, "_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- filtered_data_boxplot()
        req(nrow(data) > 0)  
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    ############################################################################ 
    ## NRMSE plot
    # Dropdowns and checkboxes nRMSE plot 
    updateSelectInput(session, "nrmse_depth_select", 
                      choices = sort(unique(bench$depth)), 
                      selected = sort(unique(bench$depth))[1])
    updateSelectInput(session, "nrmse_approach_select", 
                      choices = sort(unique(bench$collapse_approach)), 
                      selected = sort(unique(bench$collapse_approach))[1])
    updateSelectInput(session, "nrmse_fraction_select", 
                      choices = sort(unique(bench$expected_fraction[bench$expected_fraction != 0])), 
                      selected = sort(unique(bench$expected_fraction[bench$expected_fraction != 0]))[1])

    updateSelectInput(session, "nrmse_tool_select", 
                      choices = sort(unique(bench$tool)), 
                      selected = sort(unique(bench$tool))[1])
    updateCheckboxGroupInput(session, "nrmse_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    
    # RMSE Data Filtering
    filtered_data_nrmse <- reactive({
      req(input$nrmse_depth_select, input$nrmse_approach_select,input$nrmse_tool_select, input$nrmse_dmrtools_select)
      data <- bench %>%
        filter(depth == input$nrmse_depth_select, 
               collapse_approach == input$nrmse_approach_select, 
               tool == input$nrmse_tool_select, 
               DMRtool %in% input$nrmse_dmrtools_select,
               expected_fraction != 0) # Exclude expected_fraction == 0
      # print(head(data))
      # print(str(data))
      return(data)
    })    
    
    compute_nrmse_data <- function(data) {
      nrmse_data <- data %>%
        #filter(DMRtool %in% dmrtools) %>%
        group_by(DMRtool, expected_fraction) %>%
        summarize(NRMSE = rmse(expected_fraction, nbl) / mean(expected_fraction), .groups = "drop") # Calculate mean RMSE
      print(head(nrmse_data))
      print(str(nrmse_data))
      return(nrmse_data)
    }
  
    # Create a function to generate the RMSE plot
    create_plot_nrmse <- function(data) {

      data <- data %>%
        mutate(tooltip_text = paste("DMRtool:", DMRtool, 
                                    "<br>Expected Fraction:", expected_fraction, 
                                    "<br>NRMSE:", round(NRMSE, 3)))
      
      # Plot 
      ggplot(data, aes(x = factor(expected_fraction), y = NRMSE, color = DMRtool, text = tooltip_text)) +
        geom_point(size = 3, alpha = 0.8) +
        labs(
          #title = paste("nRMSE for Tool:", tool),
          x = "Expected fraction",
          y = "NRMSE",
          color = "DMRtool",
          shape = "DMRtool"
        ) + theme_benchmarking +  
        scale_y_continuous(expand = expansion(mult = 0.05))  +
        custom_color_manual
      }

    #Render the RMSE plot in UI using the function
    output$nrmse_plot <- renderPlotly({
      data <- filtered_data_nrmse()
      req(nrow(data) > 0)
      nrmse_data <- compute_nrmse_data(data)
      plot <- create_plot_nrmse(nrmse_data)
      ggplotly(plot, tooltip = "text") %>% # Convert ggplot to interactive plotly
        config(toImageButtonOptions = list(format = c("svg"),
                                           filename = paste("NRMSE_",input$nrmse_tool_select, "_depth_",input$nrmse_depth_select,"_approach_",input$nrmse_approach_select,"_", Sys.Date()))
        )
    })
    
    # Save nrmse plot as svg and pdf
    # download_nrmse_plot <- function(ext) {
    #   downloadHandler(
    #     filename = function() paste(input$nrmse_tool_select, "_vs_fractions_nrmse_", Sys.Date(), ".", ext, sep = ""),
    #     content = function(file) {
    #       data <- filtered_data_nrmse()
    #       req(nrow(data) > 0)
    #       ggsave(file, plot = create_plot_nrmse(data, input$nrmse_tool_select, input$nrmse_dmrtools_select),
    #              width = 10, height = 6, dpi = 300, device = ext)
    #     }
    #   )
    # }
    # output$download_nrmse_plot_svg <- download_nrmse_plot("svg")
    # output$download_nrmse_plot_pdf <- download_nrmse_plot("pdf")
    
    # Save dataframe nrmse plot as csv
    output$download_nrmse_plot_df <- downloadHandler(
      filename = function() {
        paste("NRMSE_",input$nrmse_tool_select, "_depth_",input$nrmse_depth_select,"_approach_",input$nrmse_approach_select,"_", Sys.Date(),".csv", sep = "")
      },
      content = function(file) {
        data <- filtered_data_nrmse()
        req(nrow(data) > 0)  
        nrmse_data <- compute_nrmse_data(data)
        write.csv(nrmse_data, file, row.names = FALSE)
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
    
    print(head(filtered_data_rmse_comparison))
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
        mutate(tooltip_text = paste("DMRtool:", DMRtool, 
                                    "<br>Tool:", tool,
                                    "<br>nRMSE:", round(RMSE, 3)))
      
      # Generate the plot
      ggplot(plot_data, aes(x = RMSE, y = fct_reorder(tool, RMSE), color = DMRtool, text = tooltip_text)) +
        geom_point(size = 3, alpha = 0.8) +
        scale_y_discrete(labels = function(y) str_replace_all(y, "_", " ")) +
        labs(
          #title = paste("RMSE vs Tool (Expected Fraction:", fraction, ")"),
          x = "nRMSE",
          y = "",
          color = "DMRtool",
          shape = "DMRtool"
        ) + theme_benchmarking + scale_x_continuous(expand = expansion(mult = 0.05)) + 
        custom_color_manual
      }
    
        # Render the RMSE comparison plot in UI using the function
    output$rmse_comparison <- renderPlotly({
      data <- filtered_data_rmse_comparison()
      req(nrow(data) > 0)
      plot <- create_rmse_comparison_plot(data, input$rmse_comparison_tools_select, input$rmse_comparison_fraction_select, input$rmse_comparison_dmrtools_select)
      ggplotly(plot, tooltip = "text") %>% # Convert ggplot to interactive plotly
        config(toImageButtonOptions = list(format = "svg",
                                           filename = paste("ranking_tools_fraction_", as.numeric(input$rmse_comparison_fraction_select), "_", Sys.Date())
        ))
    })
    
    # Save RMSE comparison plot using the function
    # download_rmse_comparison_plot <- function(ext) {
    #   downloadHandler(
    #     filename = function() paste("ranking_tools_fraction_", as.numeric(input$rmse_comparison_fraction_select), "_", Sys.Date(), ".", ext, sep = ""),
    #     content = function(file) {
    #       data <- filtered_data_rmse_comparison()
    #       req(nrow(data) > 0)
    #       plot <- create_rmse_comparison_plot(data, input$rmse_comparison_tools_select, input$rmse_comparison_fraction_select, input$rmse_comparison_dmrtools_select)
    # 
    #       ggsave(file, plot = plot,
    #              width = 10, height = 6, dpi = 300, device = ext)
    #     }
    #   )
    # }
    # output$download_rmse_comparison_svg <- download_rmse_comparison_plot("svg")
    # output$download_rmse_comparison_pdf <- download_rmse_comparison_plot("pdf")
    
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
        scale_fill_gradient(low = "white", high = "#2f425e", limits = c(0,1) ) + # Greyscale color scale
        scale_color_identity() + 
        #scale_x_discrete(labels = function(x) str_replace_all(x, "_", " "))+
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
    
    # Render heatmap
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

    
    ############################################################################ 
    ## AUCROC plot
    # Dropdowns and checkboxes AUCROC
    updateSelectInput(session, "aucroc_tool_select", 
                      choices = sort(unique(bench$tool)), 
                      selected = sort(unique(bench$tool))[1])
    
    updateSelectInput(session, "aucroc_dmrtool_select", 
                      choices = sort(unique(bench$DMRtool)), 
                      selected = sort(unique(bench$DMRtool))[1])

    # Create a function to generate the heatmap
    create_aucroc_data <- function(data, tool, dmrtool) {
      aucroc_data <- data.frame()
      fractions <- c(0.0001, 0.001, 0.01, 0.05)
      for (fraction in unique(fractions)) {
        filt_df <- data %>%
          filter(expected_fraction %in% c(0, fraction) & DMRtool == dmrtool & tool == !!tool)
        if (nrow(filt_df) > 0) {
          roc_curve <- roc.obj(filt_df$expected_fraction, filt_df$nbl)
          tmp <- data.frame(
                    fpr = 1 - rev(roc_curve$specificities),
                    tpr = rev(roc_curve$sensitivities),
                    thresholds = rev(roc_curve$thresholds),
                    auc = rev(roc_curve$auc),
                    fraction = fraction,
                    tool = tool
                  )
          aucroc_data <- rbind(aucroc_data, tmp)
        }
      }
      return(aucroc_data)
    }
    
    # Function to generate AUC-ROC plot
    create_aucroc_plot <- function(aucroc_data) {
      # Tooltip text for lines (FPR, TPR, fraction)
      aucroc_data <- aucroc_data %>%
        mutate(tooltip_line = paste("FPR:", round(fpr, 3), "<br>TPR:", round(tpr, 3), "<br>Fraction:", fraction))
      
      # Tooltip text for points (AUC value)
      aucroc_data <- aucroc_data %>%
        mutate(tooltip_point = paste("AUC:", round(auc, 3)))
      
      ggplot(aucroc_data, aes(x = fpr, y = tpr, color = as.factor(fraction), group = fraction)) + 
        # ROC Curve Lines
        geom_line(aes(text = tooltip_line), size = 1) +
        
        # AUC Points (only at x=0)
        geom_point(aes(x = 0, y = auc, text = tooltip_point), shape = 1, stroke = 1.5, size = 2, show.legend = FALSE) +
        
        # Labels and theme
        labs(
          # x = "False Positive Rate (FPR)",
          # y = "True Positive Rate (TPR)",
          x = "FPR",
          y = "TPR",
          color = "Tumoral fraction"
        ) + theme_benchmarking
    }

    # Render output AUCROC plot
    output$aucroc_plot <- renderPlotly({
      req(input$aucroc_tool_select, input$aucroc_dmrtool_select)
      aucroc_data <- create_aucroc_data(bench, input$aucroc_tool_select, input$aucroc_dmrtool_select)
      req(nrow(aucroc_data) > 0)  
      plot <- create_aucroc_plot(aucroc_data)
      ggplotly(plot, tooltip = "text") %>%
        config(toImageButtonOptions = list(format = "svg",
                                           filename = paste("aucroc_tumor_fractions_", input$aucroc_tool_select, "_", input$aucroc_dmrtool_select, "_", Sys.Date())
                                           ))
    })
    
    # Save AUCROC using the function
    # download_aucroc_plot <- function(ext) {
    #   downloadHandler(
    #     filename = function() paste("aucroc_tumor_fractions_", input$aucroc_tool_select, "_", input$aucroc_dmrtool_select, "_", Sys.Date(), ".", ext, sep=""),
    #     content = function(file) {
    #       req(input$aucroc_tool_select, input$aucroc_dmrtool_select)
    #       aucroc_data <- create_aucroc_data(bench, input$aucroc_tool_select, input$aucroc_dmrtool_select)
    #       req(nrow(aucroc_data) > 0)  
    #       plot <- create_aucroc_plot(aucroc_data)
    #       ggsave(file, plot = plot, width = 6, height = 6, dpi = 300, device = ext)
    #     }
    #   )
    # }
    # output$download_aucroc_svg <- download_aucroc_plot("svg")
    # output$download_aucroc_pdf <- download_aucroc_plot("pdf")
    
    # Save dataframe AUCROC plot as csv
    output$download_aucroc_df <- downloadHandler(
      filename = function() {
        paste("aucroc_tumor_fractions_", input$aucroc_tool_select, "_", input$aucroc_dmrtool_select, "_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        req(input$aucroc_tool_select, input$aucroc_dmrtool_select)
        aucroc_data <- create_aucroc_data(bench, input$aucroc_tool_select, input$aucroc_dmrtool_select)
        req(nrow(aucroc_data) > 0)  
        write.csv(aucroc_data, file, row.names = FALSE)
      }
    )
    
    
    ############################################################################ 
    # AUCROC complete plot
    # Dropdowns and checkboxes AUCROC complete plot
    
    updateCheckboxGroupInput(session, "aucroc_complete_tools_select", 
                             choices = sort(unique(bench$tool)), 
                             selected = sort(unique(bench$tool)))
    updateSelectInput(session, "aucroc_complete_dmrtool_select", 
                      choices = sort(unique(bench$DMRtool)), 
                      selected = sort(unique(bench$DMRtool))[1])
    
    # Create a function to generate the heatmap for multiple tools
    create_aucroc_complete_data <- function(data, tools, dmrtool) {
      aucroc_complete_data <- data.frame()
      fractions <- c(0.0001, 0.001, 0.01, 0.05)
      for (tool in tools) {
        for (fraction in unique(fractions)) {
          filt_df <- data %>%
            filter(expected_fraction %in% c(0, fraction) & DMRtool == dmrtool & tool == !!tool)
          if (nrow(filt_df) > 0) {
            roc_curve <- roc.obj(filt_df$expected_fraction, filt_df$nbl)
            tmp <- data.frame(
              fpr = 1 - rev(roc_curve$specificities),
              tpr = rev(roc_curve$sensitivities),
              thresholds = rev(roc_curve$thresholds),
              auc = rev(roc_curve$auc),
              fraction = fraction,
              tool = tool
            )
            aucroc_complete_data <- rbind(aucroc_complete_data, tmp)
          }
        }
      }
      return(aucroc_complete_data)
    }
    
    # Function to generate AUC-ROC plot with facet_wrap
    create_aucroc_complete_plot <- function(aucroc_complete_data) {
      ggplot(aucroc_complete_data, aes(x = fpr, y = tpr, color = as.factor(fraction), group = fraction)) + 
        # ROC Curve Lines
        geom_line(size = 1) +
        
        # AUC Points (only at x=0)
        geom_point(aes(x = 0, y = auc), shape = 1, stroke = 1.5, size = 2, show.legend = FALSE) +
        
        # Labels and theme
        labs(
          x = "FPR",
          y = "TPR",
          color = "Tumoral fraction"
        ) + 
        theme_benchmarking +
        facet_wrap(~ tool, ncol = 4)+ # Adjust ncol to control the number of columns
        theme(
          text = element_text(size = 14),
          strip.text = element_text(size = 12),
          legend.position = "bottom",
          panel.spacing = unit(1,"lines")
        )
    }
    
    # Render output AUCROC plot
    output$aucroc_complete_plot <- renderPlot({
      req(input$aucroc_complete_tools_select, input$aucroc_complete_dmrtool_select)
      aucroc_complete_data <- create_aucroc_complete_data(bench, input$aucroc_complete_tools_select, input$aucroc_complete_dmrtool_select)
      req(nrow(aucroc_complete_data) > 0)  
      create_aucroc_complete_plot(aucroc_complete_data)
    })
    
    # Save AUCROC using the function
    download_aucroc_complete_plot <- function(ext) {
      downloadHandler(
        filename = function() paste("aucroc_tumor_fractions_",input$aucroc_dmrtool_select, "_", Sys.Date(), ".", ext, sep=""),
        content = function(file) {
          req(input$aucroc_complete_tools_select, input$aucroc_complete_dmrtool_select)
          aucroc_complete_data <- create_aucroc_complete_data(bench, input$aucroc_complete_tools_select, input$aucroc_complete_dmrtool_select)
          req(nrow(aucroc_complete_data) > 0)  
          plot <- create_aucroc_complete_plot(aucroc_complete_data)
          ggsave(file, plot = plot, width = 8, height = 6, dpi = 300, device = ext)
        }
      )
    }
    output$download_aucroc_complete_svg <- download_aucroc_complete_plot("svg")
    output$download_aucroc_complete_pdf <- download_aucroc_complete_plot("pdf")
    
    # Save dataframe AUCROC plot as csv
    output$download_aucroc_complete_df <- downloadHandler(
      filename = function() {
        paste("aucroc_tumor_fractions_", input$aucroc_complete_dmrtool_select, "_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        req(input$aucroc_complete_tools_select, input$aucroc_complete_dmrtool_select)
        aucroc_complete_data <- create_aucroc_complete_data(bench, input$aucroc_complete_tools_select, input$aucroc_complete_dmrtool_select)
        req(nrow(aucroc_complete_data) > 0)  
        write.csv(aucroc_complete_data, file, row.names = FALSE)
      }
    )
    ############################################################################ 
    ## Rank tools 
    # Dropdowns and checkboxes Rank tools
    updateCheckboxGroupInput(session, "rank_tools_select", 
                             choices = sort(unique(bench$tool)), 
                             selected = sort(unique(bench$tool)))
    updateCheckboxGroupInput(session, "rank_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    updateSelectInput(session, "rank_metric_select", 
                      choices =  c("normAUC", "normRMSE", "normSCC", "Score"), 
                      selected = c("normAUC", "normRMSE", "normSCC", "Score")[1])
    
    
    # Rank tool Data Filtering
    filtered_data_ranking <- function(bench, tools, dmrtools) {
      # Filter the input data based on selected tools and DMR tools
      bench <- bench %>% 
        filter(tool %in% tools, DMRtool %in% dmrtools)
      
      print(str(bench))
      print(head(bench))
      
      # Compute AUC-ROC
      aucroc_data <- data.frame()
      fractions_auc <- unique(bench[bench$expected_fraction != 0, "expected_fraction"])
      
      for (fraction in fractions_auc) {
        df <- bench %>% filter(expected_fraction %in% c(0, fraction))
        for (deconv in unique(df$tool)) {
          filt_df <- df %>% filter(tool == deconv)
          for (dmrtool in unique(filt_df$DMRtool)) {
            fin_df <- filt_df %>% filter(DMRtool == dmrtool)
            
            roc_curve <- roc.obj(fin_df$expected_fraction, fin_df$nbl)
            tmp <- data.frame(
              tool = deconv,
              DMRtool = dmrtool,
              meanAUC = mean(rev(roc_curve$auc)),
              fraction = fraction
            )
            aucroc_data <- rbind(aucroc_data, tmp)
          }
        }
      }
      
      classif_performance_auc <- aucroc_data %>%
        group_by(tool, DMRtool) %>%
        summarize(meanAUC = mean(meanAUC))
      
      # Compute RMSE for nonzero fractions
      nonzero_fraction <- bench %>%
        filter(expected_fraction != 0) %>%
        group_by(tool, DMRtool) %>%
        summarize(RMSE = rmse(expected_fraction, nbl))
      
      # Compute SCC for all fractions
      all_fractions <- bench %>%
        group_by(tool, DMRtool) %>%
        summarize(SCC = scc(expected_fraction, nbl))
      
      # Merge the computed metrics
      merged_metrics <- merge(all_fractions, nonzero_fraction, by = c("tool", "DMRtool"))
      merged_metrics <- merge(merged_metrics, classif_performance_auc, by = c("tool", "DMRtool"))
      
      # Normalize the metrics
      normalized_list <- list()
      for (selection in unique(merged_metrics$DMRtool)) {
        tmp <- na.omit(merged_metrics[merged_metrics$DMRtool == selection, ])
        tmp$normSCC <- (tmp$SCC - min(tmp$SCC)) / (max(tmp$SCC) - min(tmp$SCC))
        tmp$normRMSE <- 1 - (tmp$RMSE - min(tmp$RMSE)) / (max(tmp$RMSE) - min(tmp$RMSE))
        tmp$normAUC <- (tmp$meanAUC - min(tmp$meanAUC)) / (max(tmp$meanAUC) - min(tmp$meanAUC))
        normalized_list[[selection]] <- tmp[, c("tool", "DMRtool", "normSCC", "normRMSE", "normAUC")]
      }
      
      normalized_df <- do.call(rbind, normalized_list)
      
      # Create a combined metrics 'Score' (using the formula you provided)
      nzeros <- nrow(bench[bench$expected_fraction == 0,])
      nnonzeros <- nrow(bench[bench$expected_fraction != 0,])
      tot <- nzeros + nnonzeros
      
      normalized_df$Score <- 
        normalized_df$normAUC + 
        (nnonzeros / tot) * (normalized_df$normRMSE) + 
        normalized_df$normSCC
      
      return(normalized_df)
    }
    
    
    # Create a function to generate rank plots
    create_plot_ranking <- function(data, metric) {
      data <- data %>%
        mutate(tooltip_text = paste(metric,":", round(.data[[metric]], 3), 
                                    "<br>Tool:", tool, 
                                    "<br>DMRtool:", DMRtool))
      
      # Calculate mean score if not already available (or use from existing data)
      mean_score <- data %>%
        group_by(tool) %>%
        summarise(Mean = mean(.data[[metric]], na.rm = TRUE)) %>%
        arrange(desc(Mean))
      
      # Reorder the tools globally without adding 'meanScore' column to the dataframe
      data <- data %>%
        mutate(tool = factor(tool, levels = mean_score$tool))
      
      ggplot(data, aes(y = as.factor(tool), x = .data[[metric]], color = DMRtool, text = tooltip_text)) +
        geom_point(size = 3, alpha = 0.8) +
        labs(
          #title = paste("Tools Ranked by", metric),
          x = metric,
          y = ""
        ) +
        theme_benchmarking +  
       #scale_y_discrete(expand = expansion(mult = 0.05)) +
        #scale_y_discrete(labels = function(x) str_replace_all(x, "_", " "))+
        custom_color_manual 
    }
    

    # Render ggplotly rank plots
    output$rank <- renderPlotly({
      data <- filtered_data_ranking(bench, input$rank_tools_select,input$rank_dmrtools_select )
      req(nrow(data) > 0)
      
      plot <- create_plot_ranking(data, input$rank_metric_select)
      ggplotly(plot, tooltip = "text") %>% # Convert ggplot to interactive plotly
        config(toImageButtonOptions = list(format = "svg",
                                           filename = paste("tools_vs_",input$rank_metric_select,"_", Sys.Date())
                                           ))
    })
    
    # Download rank plots via svg or pdf 
    # download_rank_plot <- function(ext) {
    #   downloadHandler(
    #     filename = function() {
    #       paste("tools_vs_",input$rank_metric_select,"_", Sys.Date(), ".", ext, sep = "")
    #     },
    #     content = function(file) {
    #       data <- filtered_data_ranking(bench, input$rank_tools_select, input$rank_dmrtools_select)
    #       req(nrow(data) > 0)
    #       ggsave(file, plot = create_plot_ranking(data, input$rank_metric_select),
    #              width = 10, height = 6, dpi = 300, device = ext)
    #     }
    #   )
    # }
    # 
    # output$download_rank_svg <- download_rank_plot("svg")
    # output$download_rank_pdf <- download_rank_plot("pdf")
    
    # Download data of rank plots
    output$download_rank_df <- downloadHandler(
      filename = function() {
        paste("tools_vs_",input$rank_metric_select, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- filtered_data_ranking(bench, input$rank_tools_select, input$rank_dmrtools_select)
        req(nrow(data) > 0)
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
    
    
    
  }) # Close moduleServer
} # Close metricsTabServer    
    
    
    
    
    