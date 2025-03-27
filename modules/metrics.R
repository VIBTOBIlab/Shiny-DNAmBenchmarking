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
                      checkboxInput(ns("boxplot_tools_select_all"),label =tags$em("Select All/None"), value = TRUE),
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
                      downloadButton(ns("download_boxplot_TF_df"), "Download data"),
                      textOutput("warning_message_boxplot_TF"),
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
                      downloadButton(ns("download_nrmse_plot_df"), "Download data"),
                      br(), br(), br()
                    )
                  ),
                  
                 
                  tags$hr(), br(), br(),
                  

                  # AUC-ROC of tools at 4 low tumoral fractions
                 h3("AUC-ROC at different tumoral fractions"),
                 # First main panel for the complete AUC-ROC plot
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                selectInput(
                                  ns("aucroc_complete_depth_select"), 
                                  label = "Depth",
                                  choices = NULL,
                                  selected = NULL
                                ),
                                selectInput(
                                  ns("aucroc_complete_approach_select"),
                                  label = "Approach",
                                  choices = NULL,
                                  selected = NULL
                                ),   
                                checkboxGroupInput(
                                  ns("aucroc_complete_tools_select"),
                                  label = "Deconvolution Tool",
                                  choices = NULL,
                                  selected = NULL
                                ),
                                checkboxInput(ns("aucroc_complete_tools_select_all"),label =tags$em("Select All/None"), value = TRUE),
                                selectInput(
                                  ns("aucroc_complete_dmrtool_select"),
                                  label = "DMR Tool",
                                  choices = NULL,
                                  selected = NULL
                                )
                   ),
                   mainPanel(width = 9,
                             plotOutput(ns("aucroc_complete_plot"), height = "800px"),
                             br(),
                             downloadButton(ns("download_aucroc_complete_df"), "Download data"),
                             downloadButton(ns("download_aucroc_complete_svg"), "Download as SVG"),
                             downloadButton(ns("download_aucroc_complete_pdf"), "Download as PDF"),
                             br(), br(), br()
                   )
                 ),
                 # Second main panel for on specific AUC-ROC interactive plot
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                selectInput(
                                  ns("aucroc_depth_select"), 
                                  label = "Depth",
                                  choices = NULL,
                                  selected = NULL
                                ),
                                selectInput(
                                  ns("aucroc_approach_select"),
                                  label = "Approach",
                                  choices = NULL,
                                  selected = NULL
                                ),   
                                
                                selectInput(
                                  ns("aucroc_tool_select"),
                                  label = "Deconvolution Tool",
                                  choices = NULL,
                                  selected = NULL
                                ),
                                selectInput(
                                  ns("aucroc_dmrtool_select"),
                                  label = "DMR Tool",
                                  choices = NULL,
                                  selected = NULL
                                )
                   ),
                   mainPanel(width = 9,
                             plotlyOutput(ns("aucroc_plot"), height = "400px", width = "600px"),
                             br(),
                             downloadButton(ns("download_aucroc_df"), "Download data"),
                             br(), br(), br()
                   )
                 ),
                 br(), br()

                 ), # close 'General' tab 
        
        tabPanel(title = "Tools",
                  # RMSE Comparison section
                  h3("Tools RMSE"),
                  sidebarLayout(
                    sidebarPanel(width = 3,
                      selectInput(
                        ns("rmse_comparison_depth_select"),
                        label = "Depth",
                        choices = NULL,
                        selected = NULL
                        ),
                      selectInput(
                        ns("rmse_comparison_approach_select"),
                        label = "Approach",
                        choices = NULL,
                        selected = NULL
                        ),
                      selectInput(
                        ns("rmse_comparison_fraction_select"), 
                        label = "Tumoral Fraction",
                        choices = NULL,
                        selected = NULL
                      ),
                      checkboxGroupInput(
                        ns("rmse_comparison_tools_select"),
                        label = "Deconvolution Tools",
                        choices = NULL,  
                        selected = NULL
                      ),
                      checkboxInput(ns("rmse_comparison_tools_select_all"),label =tags$em("Select All/None"), value = TRUE),
                      checkboxGroupInput(
                        ns("rmse_comparison_dmrtools_select"),
                        label = "DMR Tools",
                        choices = NULL, 
                        selected = NULL
                      )
                    ),
                    mainPanel(width = 9,
                      plotlyOutput(ns("rmse_comparison"), height = "600px"),
                      br(),
                      downloadButton(ns("download_rmse_comparison_df"), "Download data"),
                      br(), br(),
                    )
                  ),
                 tags$hr(), br(), br(),
                 h3("Final ranking of the tools"),
                 sidebarLayout(
                   sidebarPanel(width = 3,
                      selectInput(
                        ns("rank_depth_select"),
                        label = "Depth",
                        choices = NULL,
                        selected = NULL
                        ),
                      selectInput(
                        ns("rank_approach_select"),
                        label = "Approach",
                        choices = NULL,
                        selected = NULL
                        ),
                      selectInput(
                        ns("rank_metric_select"), 
                        label = "Metric",
                        choices = c("meanAUC", "RMSE", "SCC", "Score"),
                        selected = "meanAUC"
                      ),
                     checkboxGroupInput(
                       ns("rank_tools_select"),
                       label = "Deconvolution Tools",
                       choices = NULL,  
                       selected = NULL
                     ),
                     checkboxInput(ns("rank_tools_select_all"),label =tags$em("Select All/None"), value = TRUE),
                     checkboxGroupInput(
                       ns("rank_dmrtools_select"),
                       label = "DMR Tools",
                       choices = NULL, 
                       selected = NULL
                     )
                   ),
                   mainPanel(width = 9,
                             plotlyOutput(ns("rank"), height = "600px"),
                             downloadButton(ns("download_rank_df"), "Download data"),
                             br(), br(), br()
                   ) #,
                 ) #,
                 # sidebarLayout(
                 #   sidebarPanel(width = 3),
                 #   mainPanel(width = 9,
                 #             plotOutput(ns("rank_static"), height = "900px"),
                 #             downloadButton(ns("download_rank_static_svg"), "Download as SVG"),
                 #             downloadButton(ns("download_rank_static_pdf"), "Download as PDF"),
                 #             downloadButton(ns("download_rank_static_df"), "Download data"),
                 #             br(), br()
                 #   )
                 # )

        ), # Close 'Tools' section
        tabPanel(title = 'DMRtool',
                 # Heatmap section
                 h3("Heatmap of tumoral fraction vs tools"),
                 sidebarLayout(
                   sidebarPanel(width = 3,
                    selectInput(
                      ns("heatmap_depth_select"),
                      label = "Depth",
                      choices = NULL,
                      selected = NULL
                      ),
                    selectInput(
                      ns("heatmap_approach_select"),
                      label = "Approach",
                      choices = NULL,
                      selected = NULL
                      ),
                     checkboxGroupInput(
                       ns("heatmap_tools_select"),
                       label = "Deconvolution Tools",
                       choices = NULL,  
                       selected = NULL
                     ),
                    checkboxInput(ns("heatmap_tools_select_all"),label =tags$em("Select All/None"), value = TRUE),
                    selectInput(
                       ns("heatmap_dmrtool_select"),
                       label = "DMR Tool",
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
      tabPanel(title = "LoD",
               h3("Limit of detection"),
               sidebarLayout(
                 sidebarPanel(width = 3,
                              selectInput(
                                ns("lod_depth_select"),
                                label = "Depth",
                                choices = NULL,
                                selected = NULL
                              ),
                              selectInput(
                                ns("lod_approach_select"),
                                label = "Approach",
                                choices = NULL,
                                selected = NULL
                              ),
                              selectInput(
                                ns("lod_tool_select"),
                                label = "Deconvolution Tool",
                                choices = NULL,  
                                selected = NULL
                              ),
                              selectInput(
                                ns("lod_dmrtool_select"),
                                label = "DMR Tool",
                                choices = NULL, 
                                selected = NULL
                              ),
                              selectInput(
                                ns("lod_plabel_select"),
                                label = "P-value Label",
                                choices = c("p", "p.adj", "p.adj.signif"),
                                selected = "padj.signif"
                              )
                 ),
                 mainPanel(width = 9,
                           plotOutput(ns("lod"), height = "600px"),
                           downloadButton(ns("download_lod_svg"), "Download as SVG"),
                           downloadButton(ns("download_lod_pdf"), "Download as PDF"),
                           downloadButton(ns("download_lod_df"), "Download data"),
                           br(), br()
                 )
               )
      ) # Close 'LoD' section
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
    
    # updateCheckboxGroupInput(session, "boxplot_tools_select", 
    #                          choices = sort(unique(bench$tool)), 
    #                          selected = sort(unique(bench$tool)))
    updateCheckboxGroupInput(session, "boxplot_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    
    observe({
      current_choices <- sort(unique(bench$tool))  # Get all available tools
      
      # Update the checkbox group based on select all/none toggle
      updateCheckboxGroupInput(
        session, "boxplot_tools_select",
        choices = current_choices,
        selected = if (input$boxplot_tools_select_all) current_choices else character(0) # Select all if TRUE, else deselect all
      )
    })
    
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
                                    "<br>NRMSE:", round(NRMSE, 4)))
      
      # Plot 
      ggplot(data, aes(x = factor(expected_fraction), y = NRMSE, color = DMRtool, text = tooltip_text)) +
        geom_point(size = 3, alpha = 0.8,  position = position_jitter(width = 0, height = 0)) +
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
    updateSelectInput(session, "rmse_comparison_depth_select", 
                      choices = sort(unique(bench$depth)), 
                      selected = sort(unique(bench$depth))[1])
    updateSelectInput(session, "rmse_comparison_approach_select", 
                      choices = sort(unique(bench$collapse_approach)), 
                      selected = sort(unique(bench$collapse_approach))[1])
    updateSelectInput(session, "rmse_comparison_fraction_select", 
                      choices = sort(unique(bench$expected_fraction[bench$expected_fraction != 0])), 
                      selected = sort(unique(bench$expected_fraction[bench$expected_fraction != 0]))[1])
    
    # updateCheckboxGroupInput(session, "rmse_comparison_tools_select", 
    #                          choices = sort(unique(bench$tool)), 
    #                          selected = sort(unique(bench$tool)))
    updateCheckboxGroupInput(session, "rmse_comparison_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    
    observe({
      current_choices <- sort(unique(bench$tool))  # Get all available tools
      
      # Update the checkbox group based on select all/none toggle
      updateCheckboxGroupInput(
        session, "rmse_comparison_tools_select",
        choices = current_choices,
        selected = if (input$rmse_comparison_tools_select_all) current_choices else character(0) # Select all if TRUE, else deselect all
      )
    })
    
    # RMSE tool comparison Data Filtering
    filtered_data_rmse_comparison <- reactive({
      req(input$rmse_comparison_depth_select,input$rmse_comparison_approach_select, input$rmse_comparison_fraction_select, input$rmse_comparison_tools_select, input$rmse_comparison_dmrtools_select)
      bench %>%
        filter(depth == input$rmse_comparison_depth_select,
               collapse_approach == input$rmse_comparison_approach_select, 
               expected_fraction == input$rmse_comparison_fraction_select,
               expected_fraction != 0, # Exclude expected_fraction == 0
               DMRtool %in% input$rmse_comparison_dmrtools_select,
               tool %in% input$rmse_comparison_tools_select
        )
    })

    # Create a function to generate the RMSE comparison plot
    create_rmse_comparison_plot <- function(data) {
      
      # Calculate RMSE
      plot_data <- data %>%
        #filter(tool != 'Methyl_Resolver') %>%
        group_by(tool, DMRtool) %>%
        summarise(RMSE = rmse(expected_fraction, nbl), .groups = "drop")
      
      # Rank the tools by mean RMSE
      ranked_tools <- plot_data %>%
        group_by(tool) %>%
        summarise(MeanRMSE = mean(RMSE, na.rm = TRUE)) %>%
        arrange(desc(MeanRMSE)) %>%
        pull(tool)  # Extract ordered tool names
      
      # Reorder tools based on calculated ranking
      plot_data <- plot_data %>%
        mutate(tool = factor(tool, levels = ranked_tools)) 
      
      # The tools with lower RMSE (better performance) will appear at the top of the y-axis.
      # The tools with higher RMSE (worse performance) will appear at the bottom of the y-axis.
      
      # Reorder the tools globally
      plot_data <- plot_data %>%
        # mutate(tool = factor(tool, levels = median_diff$tool)) %>%
        mutate(tooltip_text = paste("DMRtool:", DMRtool, 
                                    "<br>Tool:", tool,
                                    "<br>RMSE:", round(RMSE, 4)))
      
      # Generate the plot
      ggplot(plot_data, aes(x = RMSE, y = tool, color = DMRtool, text = tooltip_text)) +
        geom_point(size = 3, alpha = 0.8, position = position_jitter(width = 0, height = 0) ) +
        scale_y_discrete(labels = function(y) str_replace_all(y, "_", " ")) +
        labs(
          #title = paste("RMSE vs Tool (Expected Fraction:", fraction, ")"),
          x = "RMSE",
          y = "",
          color = "DMRtool",
          shape = "DMRtool"
        ) + theme_benchmarking + 
        theme(#legend.text = element_text(size = 13),
          #legend.title = element_text(size = 14),
          #axis.title.x = element_text(size = 15),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major.y = element_line(color = "lightgrey", linewidth = 0.4),
          panel.grid.minor.y = element_blank()
        ) +
        scale_x_continuous(expand = expansion(mult = 0.05)) + 
        custom_color_manual
      }
    
        # Render the RMSE comparison plot in UI using the function
    output$rmse_comparison <- renderPlotly({
      data <- filtered_data_rmse_comparison()
      req(nrow(data) > 0)
      plot <- create_rmse_comparison_plot(data)
      ggplotly(plot, tooltip = "text") %>% # Convert ggplot to interactive plotly
        config(toImageButtonOptions = list(format = "svg",
                                           filename = paste("ranking_tools_fraction_", as.numeric(input$rmse_comparison_fraction_select),"_depth_",input$rmse_comparison_depth_select,"_", Sys.Date())
        ))
    })
    
    # Save dataframe rmse comparison plot as csv
    output$download_rmse_comparison_df <- downloadHandler(
      filename = function() paste("ranking_tools_fraction_", as.numeric(input$rmse_comparison_fraction_select), "_depth_", input$rmse_comparison_depth_select,"_", Sys.Date(), ".csv", sep = ""),
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
    # AUCROC complete plot
    # Dropdowns and checkboxes AUCROC complete plot
    updateSelectInput(session, "aucroc_complete_depth_select", 
                      choices = sort(unique(bench$depth)), 
                      selected = sort(unique(bench$depth))[1])
    updateSelectInput(session, "aucroc_complete_approach_select", 
                      choices = sort(unique(bench$collapse_approach)), 
                      selected = sort(unique(bench$collapse_approach))[1])
    
    # updateCheckboxGroupInput(session, "aucroc_complete_tools_select", 
    #                          choices = sort(unique(bench$tool)), 
    #                          selected = sort(unique(bench$tool)))
    updateSelectInput(session, "aucroc_complete_dmrtool_select", 
                      choices = sort(unique(bench$DMRtool)), 
                      selected = sort(unique(bench$DMRtool))[1])
    
    observe({
      current_choices <- sort(unique(bench$tool))  # Get all available tools
      
      # Update the checkbox group based on select all/none toggle
      updateCheckboxGroupInput(
        session, "aucroc_complete_tools_select",
        choices = current_choices,
        selected = if (input$aucroc_complete_tools_select_all) current_choices else character(0) # Select all if TRUE, else deselect all
      )
    })
    
    # Create a reactive function for AUCROC complete data
    create_aucroc_complete_data <- reactive({
      req(input$aucroc_complete_depth_select, 
          input$aucroc_complete_approach_select, 
          input$aucroc_complete_tools_select, 
          input$aucroc_complete_dmrtool_select)
      
      # Initialize empty dataframe
      aucroc_complete_data <- data.frame()
      fractions <- c(0.0001, 0.001, 0.01, 0.05)
      miss <- c()  # Initialize missing tool tracker
      
      # Loop through tools and fractions to generate ROC data
      for (tool in input$aucroc_complete_tools_select) {
        for (fraction in unique(fractions)) {
          filt_df <- bench %>%
            filter(depth == input$aucroc_complete_depth_select,
                   collapse_approach == input$aucroc_complete_approach_select,
                   expected_fraction %in% c(0, fraction), 
                   DMRtool == input$aucroc_complete_dmrtool_select,
                   tool == !!tool
                   )
          # Ensure both 0 and fraction are present before running ROC analysis
          if (length(unique(filt_df$expected_fraction)) != 2) {
            miss <- c(miss, tool)  # Keep track of skipped cases
            next  # Skip to the next iteration
          }
          
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
    })
    
    # Function to generate AUC-ROC plot with facet_wrap
    create_aucroc_complete_plot <- function(aucroc_complete_data) {
      # print(aucroc_complete_data)
      aucroc_complete_data$tool <- gsub("_", " ", aucroc_complete_data$tool)
      
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
      req(input$aucroc_complete_depth_select, 
          input$aucroc_complete_approach_select, 
          input$aucroc_complete_tools_select, 
          input$aucroc_complete_dmrtool_select)
      aucroc_complete_data <- create_aucroc_complete_data()
      req(nrow(aucroc_complete_data) > 0)  
      create_aucroc_complete_plot(aucroc_complete_data)
    })
    
    # Save AUCROC using the function
    download_aucroc_complete_plot <- function(ext) {
      downloadHandler(
        filename = function() paste("auc_depth_",input$aucroc_complete_depth_select, "_",input$aucroc_complete_approach_select, "_", input$aucroc_complete_dmrtool_select,"_" ,Sys.Date(), ".", ext, sep=""),
        content = function(file) {
          req(input$aucroc_complete_tools_select, input$aucroc_complete_dmrtool_select)
          aucroc_complete_data <- create_aucroc_complete_data()
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
        paste("auc_depth_",input$aucroc_complete_depth_select, "_",input$aucroc_complete_approach_select, "_", input$aucroc_complete_dmrtool_select,"_" ,Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        req(input$aucroc_complete_tools_select, input$aucroc_complete_dmrtool_select)
        aucroc_complete_data <- create_aucroc_complete_data()
        req(nrow(aucroc_complete_data) > 0)  
        write.csv(aucroc_complete_data, file, row.names = FALSE)
      }
    )
    
    ############################################################################ 
    ## Interactive AUCROC plot
    # Dropdowns and checkboxes AUCROC
    updateSelectInput(session, "aucroc_depth_select",
                      choices = sort(unique(bench$depth)),
                      selected = sort(unique(bench$depth))[1]) 
    updateSelectInput(session, "aucroc_approach_select", 
                      choices = sort(unique(bench$collapse_approach)), 
                      selected = sort(unique(bench$collapse_approach))[1])
    
    updateSelectInput(session, "aucroc_tool_select", 
                      choices = sort(unique(bench$tool)), 
                      selected = sort(unique(bench$tool))[1])
    
    updateSelectInput(session, "aucroc_dmrtool_select", 
                      choices = sort(unique(bench$DMRtool)), 
                      selected = sort(unique(bench$DMRtool))[1])
    
    # Create a reactive function for AUCROC data
    create_aucroc_data <- reactive({
      req(input$aucroc_depth_select, 
          input$aucroc_approach_select, 
          input$aucroc_tool_select, 
          input$aucroc_dmrtool_select)
      
      aucroc_data <- data.frame()
      fractions <- c(0.0001, 0.001, 0.01, 0.05)

      for (fraction in unique(fractions)) {
        filt_df <- bench %>%
          filter(depth == input$aucroc_depth_select,
                 collapse_approach == input$aucroc_approach_select,
                 expected_fraction %in% c(0, fraction),
                 DMRtool == input$aucroc_dmrtool_select,
                 tool == input$aucroc_tool_select)

        # Ensure both 0 and fraction are present before running ROC analysis
        if (length(unique(filt_df$expected_fraction)) != 2) {
          next  # Skip to the next iteration
        }
        
        if (nrow(filt_df) > 0) {
          roc_curve <- roc.obj(filt_df$expected_fraction, filt_df$nbl)
          tmp <- data.frame(
            fpr = 1 - rev(roc_curve$specificities),
            tpr = rev(roc_curve$sensitivities),
            thresholds = rev(roc_curve$thresholds),
            auc = rev(roc_curve$auc),
            fraction = fraction,
            tool = input$aucroc_tool_select
          )
          aucroc_data <- rbind(aucroc_data, tmp)
        }
      }
      return(aucroc_data)
    })
    
    # Function to generate AUC-ROC plot
    create_aucroc_plot <- function(aucroc_data) {
      print(head(aucroc_data))
      print(str(aucroc_data))
      # Tooltip text for lines (FPR, TPR, fraction)
      aucroc_data <- aucroc_data %>%
        mutate(tooltip_line = paste("FPR:", round(fpr, 3), "<br>TPR:", round(tpr, 3), "<br>Fraction:", fraction))
      
      # Tooltip text for points (AUC value)
      aucroc_data <- aucroc_data %>%
        mutate(tooltip_point = paste("AUC:", round(auc, 4)))
      
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
      aucroc_data <- create_aucroc_data()
      req(nrow(aucroc_data) > 0)  
      plot <- create_aucroc_plot(aucroc_data)
      ggplotly(plot, tooltip = "text") %>%
        config(toImageButtonOptions = list(format = "svg",
                                           filename = paste("auc_depth_",input$aucroc_depth_select, "_", input$aucroc_approach_select, "_", input$aucroc_tool_select, "_", input$aucroc_dmrtool_select, "_", Sys.Date())
        ))
    })
    
    # Save dataframe AUCROC plot as csv
    output$download_aucroc_df <- downloadHandler(
      filename = function() {
        paste("auc_depth_",input$aucroc_depth_select, "_", input$aucroc_approach_select, "_", input$aucroc_tool_select, "_", input$aucroc_dmrtool_select, "_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        aucroc_data <- create_aucroc_data()
        req(nrow(aucroc_data) > 0)  
        write.csv(aucroc_data, file, row.names = FALSE)
      }
    )
    
    ############################################################################ 
    ## Final ranking of the tools 
    # Dropdowns and checkboxes Rank tools
    updateSelectInput(session, "rank_depth_select",
                      choices = sort(unique(bench$depth)),
                      selected = sort(unique(bench$depth))[1]) 
    updateSelectInput(session, "rank_approach_select", 
                      choices = sort(unique(bench$collapse_approach)), 
                      selected = sort(unique(bench$collapse_approach))[1])
    
    # updateCheckboxGroupInput(session, "rank_tools_select", 
    #                          choices = sort(unique(bench$tool)), 
    #                          selected = sort(unique(bench$tool)))
    updateCheckboxGroupInput(session, "rank_dmrtools_select", 
                             choices = sort(unique(bench$DMRtool)), 
                             selected = sort(unique(bench$DMRtool)))
    updateSelectInput(session, "rank_metric_select", 
                      choices =  c("meanAUC", "RMSE", "SCC", "Score"), 
                      selected = c("meanAUC", "RMSE", "SCC", "Score")[1])
    
    observe({
      current_choices <- sort(unique(bench$tool))  # Get all available tools
      
      # Update the checkbox group based on select all/none toggle
      updateCheckboxGroupInput(
        session, "rank_tools_select",
        choices = current_choices,
        selected = if (input$rank_tools_select_all) current_choices else character(0) # Select all if TRUE, else deselect all
      )
    })
    
    # Merge AUC-ROC, RMSE, SCC
    merge_metrics_rank <- reactive({
      req(input$rank_depth_select, # Ensure inputs exist before running
          input$rank_approach_select,
          input$rank_dmrtools_select,
          input$rank_tools_select)  
      
      # Initialize an empty dataframe
      aucroc_data <- data.frame()
      fractions_auc <- unique(bench[bench$expected_fraction != 0, "expected_fraction"])
      miss <- c() # Initialize missing tool tracker
      
      filt_df <- bench %>%
        filter(depth == input$rank_depth_select,
               collapse_approach == input$rank_approach_select,
               DMRtool %in% input$rank_dmrtools_select, 
               tool %in% input$rank_tools_select)
      
      # Loop only through selected user inputs
      for (dmrtool in input$rank_dmrtools_select) {
        for (tool in input$rank_tools_select) {
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
                  DMRtool = dmrtool,
                  collapse_approach = input$rank_approach_select,
                  depth = input$rank_depth_select
                )
                aucroc_data <- rbind(aucroc_data, tmp)
              }
            }
      }

      # Compute mean AUC Grouped Performance
      classif_performance_auc <- aucroc_data %>%
        group_by(fraction, tool, DMRtool, collapse_approach, depth) %>%
        summarize(AUC = mean(auc), .groups = 'drop') %>% 
        group_by(tool, DMRtool, collapse_approach, depth) %>%
        summarize(meanAUC = mean(AUC), .groups = 'drop')

      # Compute RMSE for fractions > 0
      nonzero_fraction <- filt_df %>%
        filter(expected_fraction != 0) %>%
        group_by(tool, DMRtool, collapse_approach, depth) %>%
        summarize(RMSE = rmse(expected_fraction, nbl), .groups = 'drop')

      # Compute Spearman's rank correlation coefficient (SCC) on all fractions
      all_fractions <- filt_df %>%
        group_by(tool, DMRtool, depth, collapse_approach) %>%
        summarize(SCC = scc(expected_fraction, nbl), .groups = 'drop')
      
      # Merge all computed metrics
      merged_metrics <- merge(all_fractions, nonzero_fraction, by = c("tool", "DMRtool", "collapse_approach", "depth"))
      merged_metrics <- merge(merged_metrics, classif_performance_auc, by = c("tool", "DMRtool", "collapse_approach", "depth"))
      return(merged_metrics)
    })
    
    normalize_metrics <- reactive({ 
      # Retrieve merged metrics from reactive function
      merged_metrics <- merge_metrics_rank()
      
      # Normalize all the metrics based on user-selected inputs
      normalized_list <- list()
      miss <- list()
      
      for (dmrtool in input$rank_dmrtools_select) {
        tmp <- merged_metrics %>%
          filter(DMRtool == dmrtool,
                 collapse_approach == input$rank_approach_select,
                 depth == input$rank_depth_select)
        
        # Skip if df is empty
        if (dim(tmp)[1] == 0) {
          miss <- c(miss, dmrtool)
          next
        }
        
        tmp <- tmp %>%
          mutate(
            SCC = ifelse(is.na(SCC), 0, SCC),  # Replace NAs in SCC with 0
            RMSE = ifelse(is.na(RMSE), 1, RMSE),   # Replace NAs in RMSE with 1
            AUC = ifelse(is.na(meanAUC), 0, meanAUC)
          )
        
        tmp$normSCC <- (tmp$SCC - min(tmp$SCC)) / (max(tmp$SCC) - min(tmp$SCC))
        tmp$normRMSE <- 1 - (tmp$RMSE - min(tmp$RMSE)) / (max(tmp$RMSE) - min(tmp$RMSE))
        tmp$normAUC <- (tmp$meanAUC - min(tmp$meanAUC)) / (max(tmp$meanAUC) - min(tmp$meanAUC))
        

        # Append the normalized subset to the list
        key <- paste(dmrtool, input$rank_depth_select, input$rank_approach_select, sep = "_")
        normalized_list[[key]] <- tmp[, c("tool", "DMRtool", "collapse_approach", "depth", 
                                          "meanAUC", "RMSE", "SCC", "normAUC", "normSCC", "normRMSE" )]
      }

      # Combine all normalized subsets into a single data frame
      normalized_df <- do.call(rbind, normalized_list)
      
      # Create a combined metric score
      filt_df <- bench %>%
        filter(depth == input$rank_depth_select,
               collapse_approach == input$rank_approach_select,
               DMRtool %in% input$rank_dmrtools_select, 
               tool %in% input$rank_tools_select)
      
      nzeros <- nrow(filt_df[filt_df$expected_fraction == 0,])
      nnonzeros <- nrow(filt_df[filt_df$expected_fraction != 0,])
      tot <- nzeros + nnonzeros
      
      normalized_df$Score <- 
        normalized_df$normAUC +
        (nnonzeros / tot) * (normalized_df$normRMSE) + 
        normalized_df$normSCC

      return(normalized_df)
      })
    
    # Create a function to generate rank plots
    create_plot_ranking <- function(metric) {
      merged_metrics <- merge_metrics_rank()
      normalized_df <- normalize_metrics()
      
      data <- normalized_df %>%
        mutate(tooltip_text = paste(metric,":", round(.data[[metric]], 3), 
                                    "<br>Tool:", tool, 
                                    "<br>DMRtool:", DMRtool))
      
      # Calculate mean score if not already available (or use from existing data)
      mean_score <- data %>%
        group_by(tool) %>%
        summarise(Mean=sum(.data[[metric]])/length(input$rank_dmrtools_select)) %>%
        arrange(if (metric == "RMSE") desc(Mean) else Mean)  # Reverse order for RMSE

      # Reorder the tools globally without adding 'meanScore' column to the dataframe
      data <- data %>%
        mutate(tool = factor(tool, levels = mean_score$tool))
      
      # Tools with best performance are placed on top.
      # Tools with worst performance are placed at the bottom. 
      
      ggplot(data, aes(y = as.factor(tool), x = .data[[metric]], color = DMRtool, text = tooltip_text)) +
        geom_point(size = 3, alpha = 0.8) +
        labs(
          #title = paste("Tools Ranked by", metric),
          x = metric,
          y = ""
        ) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +  
        theme_benchmarking +  
        theme(#legend.text = element_text(size = 13),
              #legend.title = element_text(size = 14),
              #axis.title.x = element_text(size = 15),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              panel.grid.major.y = element_line(color = "lightgrey", linewidth = 0.4),
              panel.grid.minor.y = element_blank()
        ) +
        scale_y_discrete(labels = function(x) str_replace_all(x, "_", " ")) +
        custom_color_manual
    }
    
    
    # Render ggplotly rank plots
    output$rank <- renderPlotly({
      plot <- create_plot_ranking(input$rank_metric_select)
      ggplotly(plot, tooltip = "text") %>% # Convert ggplot to interactive plotly
        config(toImageButtonOptions = list(format = "svg",
                                           filename = paste("rank_",input$rank_metric_select,"_depth_",input$rank_depth_select, "_",input$rank_approach_select,"_",Sys.Date())
      ))
    })
    
    # Download data of rank plots
    output$download_rank_df <- downloadHandler(
      filename = function() {
        paste("rank_",input$rank_metric_select,"_depth_",input$rank_depth_select, "_",input$rank_approach_select,"_",Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        merged_metrics <- merge_metrics_rank()
        normalized_df <- normalize_metrics()
        df <- normalized_df %>% select(-normAUC, -normSCC, -normRMSE)
        write.csv(df, file, row.names = FALSE)
      }
    )


    ############################################################################ 
    # ## Final ranking of the tools static
    # 
    # # Merge AUC-ROC, RMSE, SCC
    # merge_metrics_rank_static <- reactive({
    #   
    #   filt_df <- bench %>% 
    #     filter(!is.na(tool), !is.na(DMRtool))
    #   
    #   # Initialize an empty dataframe
    #   aucroc_data <- data.frame()
    #   fractions_auc <- unique(bench[bench$expected_fraction != 0, "expected_fraction"])
    #   miss <- c() # Initialize missing tool tracker
    #   
    #   # Loop only through selected user inputs
    #   for (dmrtool in unique(bench$DMRtool)) {
    #     for (tool in unique(bench$tool)) {
    #       for (fraction in fractions_auc) {
    #         
    #         filt_df2 <- filt_df %>%
    #           filter(expected_fraction %in% c(0, fraction),
    #                  DMRtool == dmrtool,
    #                  tool == !!tool)
    #         
    #         if (length(unique(filt_df2$expected_fraction)) != 2) {
    #           miss <- c(miss, tool)
    #           next
    #         }
    #         
    #         roc_curve <- roc.obj(filt_df2$expected_fraction, filt_df2$nbl)
    #         tmp <- data.frame(
    #           fpr = 1-rev(roc_curve$specificities),  # False Positive Rate
    #           tpr = rev(roc_curve$sensitivities),  # True Positive Rate
    #           thresholds = rev(roc_curve$thresholds),
    #           auc = rev(roc_curve$auc),
    #           fraction = fraction,
    #           tool = tool,
    #           DMRtool = dmrtool
    #         )
    #         aucroc_data <- rbind(aucroc_data, tmp)
    #       }
    #     }
    #   }
    # 
    #   # Compute mean AUC Grouped Performance
    #   classif_performance_auc <- aucroc_data %>%
    #     group_by(fraction, tool, DMRtool) %>%
    #     summarize(AUC = mean(auc), .groups = 'drop') %>% 
    #     group_by(tool, DMRtool) %>%
    #     summarize(meanAUC = mean(AUC), .groups = 'drop')
    #   
    #   # Compute RMSE for fractions > 0
    #   nonzero_fraction <- filt_df %>%
    #     filter(expected_fraction != 0) %>%
    #     group_by(tool, DMRtool, depth, collapse_approach) %>%
    #     summarize(RMSE = replace_na(rmse(expected_fraction, nbl),1), .groups = 'drop') %>%
    #     group_by(tool,DMRtool) %>%
    #     summarize(meanRMSE = mean(RMSE), .groups = 'drop')
    #     
    #   # Compute Spearman's rank correlation coefficient (SCC) on all fractions
    #   all_fractions <- filt_df %>%
    #     group_by(tool, DMRtool, depth, collapse_approach) %>%
    #     summarize(SCC = replace_na(scc(expected_fraction, nbl),0), .groups = 'drop') %>%         
    #     group_by(tool,DMRtool) %>%         
    #     summarize(meanSCC = mean(SCC))
    #   
    #   # Merge all computed metrics
    #   merged_metrics <- merge(all_fractions, nonzero_fraction, by = c("tool", "DMRtool"))
    #   merged_metrics <- merge(merged_metrics, classif_performance_auc, by = c("tool", "DMRtool"))
    #   return(merged_metrics)
    # })
    # 
    # normalize_metrics_static <- reactive({ 
    #   # Retrieve merged metrics from reactive function
    #   merged_metrics <- merge_metrics_rank_static()
    #   
    #   # Normalize all the metrics based on user-selected inputs
    #   normalized_list <- list()
    #   miss <- list()
    #   
    #   for (dmrtool in unique(bench$DMRtool)) {
    #     tmp <- merged_metrics %>%
    #       filter(DMRtool == dmrtool)
    #     
    #     # Skip if df is empty
    #     if (dim(tmp)[1] == 0) {
    #       miss <- c(miss, dmrtool)
    #       next
    #     }
    #     
    #     tmp <- tmp %>%
    #       mutate(
    #         SCC = ifelse(is.na(meanSCC), 0, meanSCC),  # Replace NAs in SCC with 0
    #         RMSE = ifelse(is.na(meanRMSE), 1, meanRMSE),   # Replace NAs in RMSE with 1
    #         AUC = ifelse(is.na(meanAUC), 0, meanAUC)
    #       )
    #     
    #     tmp$normSCC <- (tmp$SCC - min(tmp$SCC)) / (max(tmp$SCC) - min(tmp$SCC))
    #     tmp$normRMSE <- 1 - (tmp$RMSE - min(tmp$RMSE)) / (max(tmp$RMSE) - min(tmp$RMSE))
    #     tmp$normAUC <- (tmp$meanAUC - min(tmp$meanAUC)) / (max(tmp$meanAUC) - min(tmp$meanAUC))
    #     
    #     # Append the normalized subset to the list
    #     normalized_list[[dmrtool]] <- tmp[, c("tool", "DMRtool", 
    #                                       "meanAUC", "meanRMSE", "meanSCC", "normAUC", "normSCC", "normRMSE" )]
    #     }
    #   
    #   # Combine all normalized subsets into a single data frame
    #   normalized_df <- do.call(rbind, normalized_list)
    # 
    #   normalized_df <- normalized_df %>%
    #     rename(RMSE = meanRMSE, SCC = meanSCC)
    #   
    #   # Create a combined metric score
    #   nzeros <- nrow(bench[bench$expected_fraction == 0,])
    #   nnonzeros <- nrow(bench[bench$expected_fraction != 0,])
    #   tot <- nzeros + nnonzeros
    #   
    #   normalized_df$Score <- 
    #     normalized_df$normAUC +
    #     (nnonzeros / tot) * (normalized_df$normRMSE) + 
    #     normalized_df$normSCC
    #   
    #   return(normalized_df)
    # 
    # })
    # 
    # create_metric_plot <- function(metric_name, data) {
    #   # Determine sort direction
    #   desc_order <- metric_name != "RMSE"
    # 
    #   metric_data <- data %>%
    #     select(tool, DMRtool, !!sym(metric_name)) %>%
    #     rename(Value = !!sym(metric_name))
    # 
    #   # Sort tools manually
    #   tool_levels <- metric_data %>%
    #     group_by(tool) %>%
    #     summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    #     arrange(if (desc_order) Value else desc(Value)) %>%
    #     pull(tool)
    # 
    #   # Apply tool order explicitly
    #   metric_data$tool <- factor(metric_data$tool, levels = tool_levels)
    # 
    #   ggplot(metric_data, aes(x = Value, y = tool, color = DMRtool)) +
    #     geom_point(size = 3, alpha = 0.85) +
    #     labs(
    #       #title = metric_name,
    #       x = metric_name,
    #       y = ""
    #     ) +
    #     theme_benchmarking +
    #     #theme(panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.5)) +
    #     scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +  
    #     scale_y_discrete(labels = function(x) str_replace_all(x, "_", " ")) +
    #     custom_color_manual+
    #     theme(legend.text = element_text(size = 13),
    #           legend.title = element_text(size = 14),
    #           axis.title.x = element_text(size = 15),
    #           axis.ticks.y = element_blank(),
    #           axis.line.y = element_blank(),
    #           panel.grid.major.y = element_line(color = "lightgrey", linewidth = 0.4),
    #           panel.grid.minor.y = element_blank()
    #     )
    #   }
    # 
    # create_static_ranking_plot <- function() {
    #   merged_metrics <- merge_metrics_rank_static()
    #   normalized_df <- normalize_metrics_static()
    # 
    #   p1 <- create_metric_plot("meanAUC", normalized_df)
    #   p2 <- create_metric_plot("RMSE", normalized_df)
    #   p3 <- create_metric_plot("SCC", normalized_df)
    #   p4 <- create_metric_plot("Score", normalized_df)
    # 
    #   (p1 + p2) / 
    #     patchwork::plot_spacer() / 
    #     (p3 + p4) +
    #     plot_layout(
    #       guides = "collect",
    #       heights = c(1, 0.05, 1)  # Row 1, spacer, Row 2
    #     ) & 
    #     theme(legend.position = "top")
    #   
    # }
    # 
    # output$rank_static <- renderPlot({
    #   create_static_ranking_plot()
    #   
    # })
    # 
    # # Save rank static using the function
    # download_rank_static_plot <- function(ext) {
    #   downloadHandler(
    #     filename = function() paste("rank_static_",Sys.Date(),".", ext, sep = ""),
    #     content = function(file) {
    # 
    #       plot <- create_static_ranking_plot()
    #       ggsave(file, plot = plot, width = 14, height = 10, dpi = 300, device = ext)
    #       
    #     }
    #   )
    # }
    # output$download_rank_static_svg <- download_rank_static_plot("svg")
    # output$download_rank_static_pdf <- download_rank_static_plot("pdf")    
    # 
    # # Download data of rank plots
    # output$download_rank_static_df <- downloadHandler(
    #   filename = function() {
    #     paste("rank_static_",Sys.Date(), ".csv", sep = "")
    #   },
    #   content = function(file) {
    #     merged_metrics <- merge_metrics_rank_static()
    #     normalized_df <- normalize_metrics_static()
    #     # df <- normalized_df %>% select(-normAUC, -normSCC, -normRMSE)
    #     write.csv(normalized_df, file, row.names = FALSE)
    #   }
    # )
    
    ############################################################################ 
    ## Heatmap
    # Dropdowns and checkboxes heatmap
    updateSelectInput(session, "heatmap_depth_select",
                      choices = sort(unique(bench$depth)),
                      selected = sort(unique(bench$depth))[1]) 
    updateSelectInput(session, "heatmap_approach_select", 
                      choices = sort(unique(bench$collapse_approach)), 
                      selected = sort(unique(bench$collapse_approach))[1])
    
    # updateCheckboxGroupInput(session, "heatmap_tools_select", 
    #                          choices = sort(unique(bench$tool)), 
    #                          selected = sort(unique(bench$tool)))    
    updateSelectInput(session, "heatmap_dmrtool_select", 
                      choices = sort(unique(bench$DMRtool)), 
                      selected = sort(unique(bench$DMRtool))[1])    
    
    observe({
      current_choices <- sort(unique(bench$tool))  # Get all available tools
      
      # Update the checkbox group based on select all/none toggle
      updateCheckboxGroupInput(
        session, "heatmap_tools_select",
        choices = current_choices,
        selected = if (input$heatmap_tools_select_all) current_choices else character(0) # Select all if TRUE, else deselect all
      )
    })
    
    # Filter data for heatmap
    create_heatmap_data <- reactive({
      req(input$heatmap_depth_select, 
          input$heatmap_approach_select, 
          input$heatmap_tools_select, 
          input$heatmap_dmrtool_select)
      
      data <- bench %>%
        filter(DMRtool == input$heatmap_dmrtool_select,
               expected_fraction != 0, 
               tool %in% input$heatmap_tools_select,
               depth == input$heatmap_depth_select,
               collapse_approach == input$heatmap_approach_select) %>%
        group_by(tool,expected_fraction) %>%  # Group by tool and tumoral fraction
        summarize(RMSE = rmse(expected_fraction, nbl), .groups = "drop")
    
      return(data)
    })
    
    # Create a function to generate the heatmap
    create_heatmap_plot <- function(plot_data) {
      # Rank tools by median RMSE
      median_diff <- plot_data %>%
        group_by(tool) %>%
        filter(expected_fraction==0.0001) %>%
        arrange(RMSE)
      
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
      plot_data$label <- ifelse(is.na(plot_data$RMSE), "NA", round(plot_data$RMSE, 5))
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
          axis.ticks = element_blank()
        )
    }
    
    # Render heatmap
    output$heatmap <- renderPlot({
      plot_data <- create_heatmap_data()
      create_heatmap_plot(plot_data)
    })
    # lower RMSE appear first (left side).
    
    
    # Save heatmap using the function
    download_heatmap_plot <- function(ext) {
      downloadHandler(
        filename = function() paste("heatmap_tools_", input$heatmap_dmrtool_select, "_depth_", input$heatmap_depth_select, "_", input$heatmap_approach_select, "_",Sys.Date(), ".", ext, sep = ""),
        content = function(file) {
          plot_data <- create_heatmap_data()
          plot <- create_heatmap_plot(plot_data)
          ggsave(file, plot = plot, width = 10, height = 6, dpi = 300, device = ext)
          
        }
      )
    }
    output$download_heatmap_svg <- download_heatmap_plot("svg")
    output$download_heatmap_pdf <- download_heatmap_plot("pdf")    
    
    # Save dataframe heatmap as csv
    output$download_heatmap_df <- downloadHandler(
      filename = function() paste("heatmap_tools_", input$heatmap_dmrtool_select, "_depth_", input$heatmap_depth_select, "_", input$heatmap_approach_select, "_", Sys.Date(), ".csv", sep = ""),
      content = function(file) {
        plot_data <- create_heatmap_data()
        write.csv(plot_data, file, row.names = FALSE)
      }
    )
    
    
    ############################################################################ 
    ## LoD
    # Dropdowns and checkboxes LoD    
    updateSelectInput(session, "lod_depth_select",
                      choices = sort(unique(bench$depth)),
                      selected = sort(unique(bench$depth))[1]) 
    updateSelectInput(session, "lod_approach_select", 
                      choices = sort(unique(bench$collapse_approach)), 
                      selected = sort(unique(bench$collapse_approach))[1])
    
    updateSelectInput(session, "lod_tool_select",
                      choices = sort(unique(bench$tool)),
                      selected = sort(unique(bench$tool))[1])    
    updateSelectInput(session, "lod_dmrtool_select", 
                      choices = sort(unique(bench$DMRtool)), 
                      selected = sort(unique(bench$DMRtool))[1])  
    
    updateSelectInput(session, "lod_plabel_select", 
                      choices = c("p", "p.adj", "p.adj.signif"),
                      selected = "p.adj.signif")
    
    # Filter data for LoD
    create_lod_data <- reactive({
      req(input$lod_depth_select, 
          input$lod_approach_select, 
          input$lod_tool_select, 
          input$lod_dmrtool_select)

      data <- bench %>%
        filter(DMRtool == input$lod_dmrtool_select,
               collapse_approach == input$lod_approach_select,
               depth == input$lod_depth_select,
               tool == input$lod_tool_select)
      return(data)
    })
    
    # Perform t-test/wilcox test 
    stat_results <- function(data) {
      stats <- data %>%
        wilcox_test(nbl ~ expected_fraction, 
                    ref.group = "0", 
                    alternative = "less") %>%
        adjust_pvalue(method = "BH") %>%
        mutate(p = formatC(p, format = "e", digits = 1),
               p.adj = formatC(p.adj, format = "e", digits = 1))
      
      # Number of comparisons
      num_comparisons <- nrow(stats)
      
      # Get max and min y-values
      max_nbl <- max(data$nbl, na.rm = TRUE)  
      min_nbl <- min(data$nbl, na.rm = TRUE)  
      
      # Assign y.position dynamically
      stats <- stats %>%
        arrange(as.numeric(group2)) %>%
        mutate(
          y.position = seq(from = max_nbl * 1.1,  # Start slightly above max
                           to = max_nbl * 1.8,   # Space out comparisons
                           length.out = num_comparisons)
        )
      
      return(stats)
    }
    
    # Create a function to generate the lod
    create_lod_plot <- function(data) {
      unique_fractions <- unique(data$expected_fraction)
      unique_fractions <- subset(unique_fractions, unique_fractions != 0)
      
      stats <- stat_results(data)
      print(as.data.frame(stats))
      
      # Plot the data 
      plot <- ggplot(data, aes(x = as.factor(expected_fraction), y = nbl)) +
        geom_boxplot() +
        labs(
          title = "",
          x = "Expected Fraction",
          y = "Estimated Tumoral Fraction (%)"
        ) +
        theme_benchmarking +
        theme(#axis.text.x = element_text(angle = 45, hjust = 1),
              axis.ticks.x = element_blank()) +
        stat_pvalue_manual(stats, label = input$lod_plabel_select) 
      
      return(plot)
      
    }

    # Output plot 
    output$lod <- renderPlot({
      data <- create_lod_data()
      #print(tapply(data$nbl, data$expected_fraction, var, na.rm = TRUE)) 
      create_lod_plot(data)
    })
    

    # Save lod plot as svg and pdf
    download_lod <- function(ext) {
      downloadHandler(
        filename = function() paste("LoD_", input$lod_tool_select, "_depth_", input$lod_depth_select, "_", input$lod_approach_select,"_", input$lod_dmrtool_select, "_" , Sys.Date(), ".", ext, sep = ""),
        content = function(file) {
          data <- create_lod_data()
          req(nrow(data) > 0)
          plot <- create_lod_plot(data)
          
          ggsave(file, plot = plot, width = 10, height = 6, dpi = 300, device = ext)
        }
      )
    }
    output$download_lod_svg <- download_lod("svg")
    output$download_lod_pdf <- download_lod("pdf")
    
    
    # Save dataframe lod as csv
    output$download_lod_df <- downloadHandler(
      filename = function() paste("LoD_", input$lod_tool_select, "_depth_", input$lod_depth_select, "_", input$lod_approach_select,"_", input$lod_dmrtool_select, "_" , Sys.Date(), ".csv", sep = ""),
      content = function(file) {
        data <- create_lod_data()
        stats <- stat_results(data)  # Generate stats before writing
        write.csv(as.data.frame(stats), file, row.names = FALSE)     
        }
    )
    
       
  }) # Close moduleServer
} # Close metricsTabServer    
    
    
    
    
    