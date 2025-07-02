rrbsTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "RRBS",

    # Table of Contents
    tags$div(class = "toc-container",
             h3("Table of Contents"),
             tags$ul(class = "toc-list",
                     tags$li(tags$a(href = "#boxplots_rrbs", "Boxplots of the predictions for each tumoral fraction")),
                     tags$li(tags$a(href = "#nrmse_rrbs", "Performance (nRMSE)")),
                     tags$li(tags$a(href = "#aucroc_rrbs", "AUC-ROC at different tumoral fractions")),
                     tags$li(tags$a(href = "#tools-rmse_rrbs", "Tools RMSE")),
                     tags$li(tags$a(href = "#final_rrbs", "Final ranking of the tools")),
                     tags$li(tags$a(href = "#heatmap_rrbs", "Heatmap of expected tumoral fraction vs deconvolution tools")),
                     tags$li(tags$a(href = "#lod_rrbs", "Limit of detection"))
             )
    ),
    tags$hr(), br(),
  
    
    ############################################################################
    # Boxplots section
    tags$div(id = "boxplots_rrbs",
             h4("Boxplots of the predictions for each tumoral fraction")
    ),

    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("boxplot_seqdepth_select"), 
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
                     ns("boxplot_exptf_select"),
                     label = "Tumoral Fraction",
                     choices = NULL,
                     selected = NULL
                     ),
                   checkboxGroupInput(
                     ns("boxplot_deconvtools_select"),
                     label = "Deconvolution Tools",
                     choices = NULL,
                     selected = NULL
                     ),
                   checkboxInput(ns("boxplot_deconvtools_select_all"),label = tags$em("Select All/None"), value = TRUE),
                   checkboxGroupInput(
                     ns("boxplot_dmrtools_select"),
                     label = "DMR Tools",
                     choices = NULL,
                     selected = NULL
                     )
                   ),
      
      mainPanel(width = 9,
                plotlyOutput(ns("boxplot_TF"), height = "600px"),
                br(),
                downloadButton(ns("download_boxplot_TF_df"), "Download data"),
                downloadButton(ns("download_rrbs_df"), "Download RRBS"),
                
                br(), br(), br()
                )
      ),
    tags$hr(), br(), br(),
    
    ############################################################################
    # nRMSE section
    tags$div(id = "nrmse_rrbs", 
             h4("Performance (nRMSE)")
    ),
    
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("nrmse_seqdepth_select"), 
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
                     ns("nrmse_deconvtool_select"), 
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

    ############################################################################
    # AUC-ROC of tools at 4 low tumoral fractions
    tags$div(id = "aucroc_rrbs", 
             h4("AUC-ROC at different tumoral fractions")
             ),
    
    # First main panel for the complete AUC-ROC plot
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("aucroc_complete_seqdepth_select"), 
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
                     ns("aucroc_complete_deconvtools_select"),
                     label = "Deconvolution Tool",
                     choices = NULL,
                     selected = NULL
                   ),
                   checkboxInput(ns("aucroc_complete_deconvtools_select_all"),label =tags$em("Select All/None"), value = TRUE),
                   selectInput(
                     ns("aucroc_complete_dmrtool_select"),
                     label = "DMR Tool",
                     choices = NULL,
                     selected = NULL
                   )
                   # radioButtons("aucroc_complete_dmrtool_select", 
                   #              label = "DMR Tool", 
                   #              choices = NULL,
                   #              selected = NULL)
                   
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
                     ns("aucroc_seqdepth_select"), 
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
                     ns("aucroc_deconvtool_select"),
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
    tags$hr(), br(), br(),
    
    ############################################################################
    # RMSE Comparison section
    tags$div(id = "tools-rmse_rrbs", 
             h4("Tools RMSE")
             ),
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("rmse_comparison_seqdepth_select"),
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
                     ns("rmse_comparison_expectedtf_select"), 
                     label = "Tumoral Fraction",
                     choices = NULL,
                     selected = NULL
                   ),
                   checkboxGroupInput(
                     ns("rmse_comparison_deconvtools_select"),
                     label = "Deconvolution Tools",
                     choices = NULL,  
                     selected = NULL
                   ),
                   checkboxInput(ns("rmse_comparison_deconvtools_select_all"),label =tags$em("Select All/None"), value = TRUE),
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
                br(), br(), br()
      )
    ),
    tags$hr(), br(), br(),
    
  
    ############################################################################
    # final ranking section
    tags$div(id = "final_rrbs", 
             h4("Final ranking of the tools")
    ), 
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("rank_seqdepth_select"),
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
                     ns("rank_deconvtools_select"),
                     label = "Deconvolution Tools",
                     choices = NULL,  
                     selected = NULL
                   ),
                   checkboxInput(ns("rank_deconvtools_select_all"),label =tags$em("Select All/None"), value = TRUE),
                   checkboxGroupInput(
                     ns("rank_dmrtools_select"),
                     label = "DMR Tools",
                     choices = NULL, 
                     selected = NULL
                   )
      ),
      mainPanel(width = 9,
                plotlyOutput(ns("rank"), height = "600px"),
                br(),
                downloadButton(ns("download_rank_df"), "Download data"),
                br(), br(), br()
      ) 
    ) ,
    
    tags$hr(), br(), br(),
    
    # sidebarLayout(
    #   sidebarPanel(width = 3),
    #   mainPanel(width = 9,
    #             plotOutput(ns("rank_static"), height = "900px"),
    #             downloadButton(ns("download_rank_static_df"), "Download data"),
    #             downloadButton(ns("download_rank_static_svg"), "Download as SVG"),
    #             downloadButton(ns("download_rank_static_pdf"), "Download as PDF"),
    #             br(), br()
    #   )
    # ),
    
    ############################################################################
    # Heatmap section
    tags$div(id = "heatmap_rrbs", 
             h4("Heatmap of expected tumoral fraction vs deconvolution tools")
             ),
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("heatmap_seqdepth_select"),
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
                     ns("heatmap_deconvtools_select"),
                     label = "Deconvolution Tools",
                     choices = NULL,  
                     selected = NULL
                   ),
                   checkboxInput(ns("heatmap_deconvtools_select_all"),label =tags$em("Select All/None"), value = TRUE),
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
                downloadButton(ns("download_heatmap_df"), "Download data"),
                downloadButton(ns("download_heatmap_svg"), "Download as SVG"),
                downloadButton(ns("download_heatmap_pdf"), "Download as PDF"),
                br(), br()
      )
    ),
    tags$hr(), br(), br(),
    
    
    ############################################################################
    # LoD section
    tags$div(id = "lod_rrbs", 
             h4("Limit of detection")
             ),
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("lod_seqdepth_select"),
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
                     ns("lod_deconvtool_select"),
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
                br(),
                downloadButton(ns("download_lod_df"), "Download data"),
                downloadButton(ns("download_lod_svg"), "Download as SVG"),
                downloadButton(ns("download_lod_pdf"), "Download as PDF"),
                br(), br(), br()
      )
    ), 
    
    # footer 
    footer_citation()  
    
    ##end
  )
}





# Optionally define server logic for this module (if needed)
rrbsTabServer <- function(id) {
moduleServer(id, function(input, output, session) {

  ## 1. Filter dataset
  bench <- subset(tot_bench, tot_bench$seq_method == "rrbs")
  
  # Sort the levels of depth
  bench$seq_depth <- factor(bench$seq_depth,
                            levels = c("5M", "10M", "15M", "20M"))
  

  output$download_rrbs_df <- downloadHandler(
    filename = function() {
      paste0("rrbs", "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(bench, file, row.names = FALSE)
    }
  )
  
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
  
  ############################################################################
  ## Boxplot predictions for each tumoral fraction
  # Dropdowns and checkboxes boxplot 
  updateSelectInput(session, "boxplot_seqdepth_select", 
                    choices = sort(unique(bench$seq_depth)), 
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] )
  updateSelectInput(session, "boxplot_approach_select", 
                    choices = sort(unique(bench$collapse_approach)), 
                    selected = sort(unique(bench$collapse_approach))[1])
  updateSelectInput(session, "boxplot_exptf_select", 
                    choices = sort(unique(bench$expected_tf[bench$expected_tf != 0])), 
                    selected = if (0.1 %in% bench$expected_tf) 0.1 else sort(unique(bench$expected_tf[bench$expected_tf != 0]))[1] )
  
  # updateCheckboxGroupInput(session, "boxplot_deconvtools_select", 
  #                          choices = sort(unique(bench$deconv_tool)), 
  #                          selected = sort(unique(bench$deconv_tool)))
  updateCheckboxGroupInput(session, "boxplot_dmrtools_select", 
                           choices = sort(unique(bench$dmr_tool)), 
                           selected = sort(unique(bench$dmr_tool)))
  
  observe({
    current_choices <- sort(unique(bench$deconv_tool))  # Get all available tools
    
    # Update the checkbox group based on select all/none toggle
    updateCheckboxGroupInput(
      session, "boxplot_deconvtools_select",
      choices = current_choices,
      selected = if (input$boxplot_deconvtools_select_all) current_choices else character(0) # Select all if TRUE, else deselect all
    )
  })
  
  # Reactive expression for filtered data boxplot
  filtered_data_boxplot <- reactive({
    req(input$boxplot_seqdepth_select, input$boxplot_approach_select, input$boxplot_exptf_select,
        input$boxplot_deconvtools_select, input$boxplot_dmrtools_select)
    
    bench %>%
      filter(seq_depth == input$boxplot_seqdepth_select,                # Filter by depth
             collapse_approach == input$boxplot_approach_select,  # Filter by approach
             expected_tf == as.numeric(input$boxplot_exptf_select),  # Filter by fraction
             deconv_tool %in% input$boxplot_deconvtools_select,                   # Filter by deconv_tools
             dmr_tool %in% input$boxplot_dmrtools_select,             # Filter by dmr_tools
             expected_tf != 0) # Exclude expected_tf == 0
  })
  
  # Function to create the boxplot
  create_boxplot_TF <- function(data, seq_depth, approach, expected_tf ) {
    # Rank the tools by median difference
    median_diff <- data %>%
      group_by(deconv_tool, dmr_tool) %>%
      summarise(Diff = abs(median(expected_tf) - median(predicted_tf)), .groups = 'drop') %>%
      group_by(deconv_tool) %>%
      summarise(Mean = mean(Diff, na.rm = TRUE), .groups = 'drop') %>%
      arrange(Mean)
    # Tools with the smallest mean difference (more accurate) are placed first.
    # Tools with the largest mean difference (less accurate) are placed last.
    
    # Reorder the tools globally
    data <- data %>%
      mutate(deconv_tool = factor(deconv_tool, levels = median_diff$deconv_tool))
    
    # Create hover text for jitter points
    # data <- data %>%
    #   mutate(hover_text = paste0("deconv_tool: ", deconv_tool, "<br>",
    #                              "predicted_tf: ", predicted_tf, "<br>",
    #                              "dmr_tool: ", dmr_tool))
    
    # Boxplot
    ggplot(data, aes(x = deconv_tool, y = predicted_tf, fill = dmr_tool, color = dmr_tool)) +
      geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +
      geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), size = 0.8, alpha = 0.5) +
      geom_hline(yintercept = expected_tf, color = "red", linetype = "dashed") +
      scale_x_discrete(labels = function(x) str_replace_all(x, "_", " ")) +
      labs(
        x = "",
        y = "Predicted Tumoral Fraction"
      ) + theme_benchmarking +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      custom_color_manual + custom_fill_manual
  }
  
  # Render the boxplot in UI using the function
  output$boxplot_TF <- renderPlotly({
    data <- filtered_data_boxplot()
    req(nrow(data) > 0)
    plot <- create_boxplot_TF(data, input$boxplot_seqdepth_select, input$boxplot_approach_select, as.numeric(input$boxplot_exptf_select))
    ggplotly(plot) %>% 
      layout(boxmode= "group") %>% 
      config(toImageButtonOptions = list(format = "pdf",
                                         filename = paste0("boxplot_",input$boxplot_seqdepth_select,"_approach_",input$boxplot_approach_select,"_fraction_", input$boxplot_exptf_select, "_", Sys.Date())
      ))
  })
  
  # Save dataframe boxplot as csv
  output$download_boxplot_TF_df <- downloadHandler(
    filename = function() {
      paste0("boxplot_",input$boxplot_seqdepth_select,"_approach_",input$boxplot_approach_select,"_fraction_", input$boxplot_exptf_select, "_", Sys.Date(), ".csv", sep = "")
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
  updateSelectInput(session, "nrmse_seqdepth_select", 
                    choices = sort(unique(bench$seq_depth)), 
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] )
  updateSelectInput(session, "nrmse_approach_select", 
                    choices = sort(unique(bench$collapse_approach)), 
                    selected = sort(unique(bench$collapse_approach))[1])

  updateSelectInput(session, "nrmse_deconvtool_select", 
                    choices = sort(unique(bench$deconv_tool)), 
                    selected = sort(unique(bench$deconv_tool))[1])
  updateCheckboxGroupInput(session, "nrmse_dmrtools_select", 
                           choices = sort(unique(bench$dmr_tool)), 
                           selected = sort(unique(bench$dmr_tool)))
  
  # RMSE Data Filtering
  filtered_data_nrmse <- reactive({
    req(input$nrmse_seqdepth_select, input$nrmse_approach_select,input$nrmse_deconvtool_select, input$nrmse_dmrtools_select)
    data <- bench %>%
      filter(seq_depth == input$nrmse_seqdepth_select, 
             collapse_approach == input$nrmse_approach_select, 
             deconv_tool == input$nrmse_deconvtool_select, 
             dmr_tool %in% input$nrmse_dmrtools_select,
             expected_tf != 0) # Exclude expected_tf == 0
    return(data)
  })    
  head(filtered_data_nrmse)
  str(filtered_data_nrmse)
  
  compute_nrmse_data <- function(data) {
    nrmse_data <- data %>%
      #filter(DMRtool %in% dmrtools) %>%
      group_by(dmr_tool, expected_tf) %>%
      summarize(NRMSE = rmse(expected_tf, predicted_tf) / mean(expected_tf), .groups = "drop") # Calculate mean RMSE
    return(nrmse_data)
  }
  
  # Create a function to generate the RMSE plot
  create_plot_nrmse <- function(data) {
    
    data <- data %>%
      mutate(tooltip_text = paste0("dmr_tool: ", dmr_tool, 
                                  "<br>Expected Fraction: ", expected_tf, 
                                  "<br>NRMSE: ", round(NRMSE, 4)))
    
    # Plot 
    ggplot(data, aes(x = factor(expected_tf), y = NRMSE, color = dmr_tool, text = tooltip_text)) +
      geom_point(size = 3, alpha = 0.8,  position = position_jitter(width = 0, height = 0)) +
      labs(
        #title = paste0("nRMSE for Tool: ", tool),
        x = "Expected fraction",
        y = "NRMSE",
        color = "dmr_tool",
        shape = "dmr_tool"
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
                                         filename = paste0("NRMSE_",input$nrmse_deconvtool_select, "_depth_",input$nrmse_seqdepth_select,"_approach_",input$nrmse_approach_select,"_", Sys.Date()))
      )
  })
  
  # Save dataframe nrmse plot as csv
  output$download_nrmse_plot_df <- downloadHandler(
    filename = function() {
      paste0("NRMSE_",input$nrmse_deconvtool_select, "_depth_",input$nrmse_seqdepth_select,"_approach_",input$nrmse_approach_select,"_", Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      data <- filtered_data_nrmse()
      req(nrow(data) > 0)  
      nrmse_data <- compute_nrmse_data(data)
      write.csv(nrmse_data, file, row.names = FALSE)
    }
  )
  
  
  ############################################################################     
  # AUCROC complete plot
  # Dropdowns and checkboxes AUCROC complete plot
  updateSelectInput(session, "aucroc_complete_seqdepth_select", 
                    choices = sort(unique(bench$seq_depth)), 
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] )
  updateSelectInput(session, "aucroc_complete_approach_select", 
                    choices = sort(unique(bench$collapse_approach)), 
                    selected = sort(unique(bench$collapse_approach))[1])
  
  # updateCheckboxGroupInput(session, "aucroc_complete_deconvtools_select", 
  #                          choices = sort(unique(bench$deconv_tool)), 
  #                          selected = sort(unique(bench$deconv_tool)))
  updateSelectInput(session, "aucroc_complete_dmrtool_select",
                    choices = sort(unique(bench$dmr_tool)),
                    selected = sort(unique(bench$dmr_tool))[1])
  
  # updateRadioButtons(session, "aucroc_complete_dmrtool_select",
  #                    choices = sort(unique(bench$dmr_tool)),
  #                    selected = sort(unique(bench$dmr_tool))[1])
  
  observe({
    current_choices <- sort(unique(bench$deconv_tool))  # Get all available tools
    
    # Update the checkbox group based on select all/none toggle
    updateCheckboxGroupInput(
      session, "aucroc_complete_deconvtools_select",
      choices = current_choices,
      selected = if (input$aucroc_complete_deconvtools_select_all) current_choices else character(0) # Select all if TRUE, else deselect all
    )
  })
  
  
  create_aucroc_complete_data <- reactive({
    req(input$aucroc_complete_seqdepth_select,
        input$aucroc_complete_approach_select,
        input$aucroc_complete_deconvtools_select,
        input$aucroc_complete_dmrtool_select)
    
    # Pre-filter the dataset once
    filtered_bench <- bench %>%
      filter(seq_depth == input$aucroc_complete_seqdepth_select,
             collapse_approach == input$aucroc_complete_approach_select,
             dmr_tool == input$aucroc_complete_dmrtool_select)
    
    # Define all combinations of tools × fractions
    fractions <- c(0.0001, 0.001, 0.01, 0.05)
    combinations <- expand.grid(
      deconv_tool = input$aucroc_complete_deconvtools_select,
      fraction = fractions,
      stringsAsFactors = FALSE
    )
    
    # Function to compute ROC curve for each tool-fraction pair
    get_auc_data <- function(deconv_tool, fraction) {
      filt_df <- filtered_bench %>%
        filter(expected_tf %in% c(0, fraction),
               deconv_tool == !!deconv_tool)
      
      if (length(unique(filt_df$expected_tf)) != 2 || nrow(filt_df) == 0) return(NULL)
      
      roc_curve <- roc(filt_df$expected_tf, filt_df$predicted_tf)
      
      data.frame(
        fpr = 1 - rev(roc_curve$specificities),
        tpr = rev(roc_curve$sensitivities),
        thresholds = rev(roc_curve$thresholds),
        auc = rev(roc_curve$auc),
        fraction = fraction,
        deconv_tool = deconv_tool
        )
    }
    
    # Run the AUC calculation in parallel
    future_pmap_dfr(
      list(combinations$deconv_tool, combinations$fraction),
      get_auc_data,
      .options = furrr_options(seed = TRUE)
    )
  })
  
  # Function to generate AUC-ROC plot with facet_wrap
  create_aucroc_complete_plot <- function(aucroc_complete_data) {
    # print(aucroc_complete_data)
    aucroc_complete_data$deconv_tool <- gsub("_", " ", aucroc_complete_data$deconv_tool)
    
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
      facet_wrap(~ deconv_tool, ncol = 4)+ # Adjust ncol to control the number of columns
      theme(
        text = element_text(size = 14),
        strip.text = element_text(size = 12),
        legend.position = "bottom",
        panel.spacing = unit(1,"lines")
      )
  }
  
  # Render output AUCROC plot
  output$aucroc_complete_plot <- renderPlot({
    aucroc_complete_data <- create_aucroc_complete_data()
    req(nrow(aucroc_complete_data) > 0)  
    create_aucroc_complete_plot(aucroc_complete_data)
  })
  
  # Save AUCROC using the function
  download_aucroc_complete_plot <- function(ext) {
    downloadHandler(
      filename = function() paste0("auc_depth_",input$aucroc_complete_seqdepth_select, "_",input$aucroc_complete_approach_select, "_", input$aucroc_complete_dmrtool_select,"_" ,Sys.Date(), ".", ext, sep=""),
      content = function(file) {
        req(input$aucroc_complete_deconvtools_select, input$aucroc_complete_dmrtool_select)
        aucroc_complete_data <- create_aucroc_complete_data()
        req(nrow(aucroc_complete_data) > 0)  
        plot <- create_aucroc_complete_plot(aucroc_complete_data)
        ggsave(file, plot = plot, width = 9, height = 10, dpi = 300, device = ext)
      }
    )
  }
  output$download_aucroc_complete_svg <- download_aucroc_complete_plot("svg")
  output$download_aucroc_complete_pdf <- download_aucroc_complete_plot("pdf")
  
  # Save dataframe AUCROC plot as csv
  output$download_aucroc_complete_df <- downloadHandler(
    filename = function() {
      paste0("auc_depth_",input$aucroc_complete_seqdepth_select, "_",input$aucroc_complete_approach_select, "_", input$aucroc_complete_dmrtool_select,"_" ,Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(input$aucroc_complete_deconvtools_select, input$aucroc_complete_dmrtool_select)
      aucroc_complete_data <- create_aucroc_complete_data()
      req(nrow(aucroc_complete_data) > 0)  
      write.csv(aucroc_complete_data, file, row.names = FALSE)
    }
  )
  
  
  ############################################################################ 
  ## Interactive AUCROC plot
  # Dropdowns and checkboxes AUCROC
  updateSelectInput(session, "aucroc_seqdepth_select",
                    choices = sort(unique(bench$seq_depth)),
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] ) 
  updateSelectInput(session, "aucroc_approach_select", 
                    choices = sort(unique(bench$collapse_approach)), 
                    selected = sort(unique(bench$collapse_approach))[1])
  
  updateSelectInput(session, "aucroc_deconvtool_select", 
                    choices = sort(unique(bench$deconv_tool)), 
                    selected = sort(unique(bench$deconv_tool))[1])
  
  updateSelectInput(session, "aucroc_dmrtool_select", 
                    choices = sort(unique(bench$dmr_tool)), 
                    selected = sort(unique(bench$dmr_tool))[1])
  
  # Create a reactive function for AUCROC data
  create_aucroc_data <- reactive({
    req(input$aucroc_seqdepth_select, 
        input$aucroc_approach_select, 
        input$aucroc_deconvtool_select, 
        input$aucroc_dmrtool_select)
    
    aucroc_data <- data.frame()
    fractions <- c(0.0001, 0.001, 0.01, 0.05)
    
    for (fraction in unique(fractions)) {
      filt_df <- bench %>%
        filter(seq_depth == input$aucroc_seqdepth_select,
               collapse_approach == input$aucroc_approach_select,
               expected_tf %in% c(0, fraction),
               dmr_tool == input$aucroc_dmrtool_select,
               deconv_tool == input$aucroc_deconvtool_select)
      
      # Ensure both 0 and fraction are present before running ROC analysis
      if (length(unique(filt_df$expected_tf)) != 2) {
        next  # Skip to the next iteration
      }
      
      if (nrow(filt_df) > 0) {
        roc_curve <- roc.obj(filt_df$expected_tf, filt_df$predicted_tf)
        tmp <- data.frame(
          fpr = 1 - rev(roc_curve$specificities),
          tpr = rev(roc_curve$sensitivities),
          thresholds = rev(roc_curve$thresholds),
          auc = rev(roc_curve$auc),
          fraction = fraction,
          deconv_tool = input$aucroc_deconvtool_select
        )
        aucroc_data <- rbind(aucroc_data, tmp)
      }
    }
    return(aucroc_data)
  })
  
  # Function to generate AUC-ROC plot
  create_aucroc_plot <- function(aucroc_data) {
    # print(head(aucroc_data))
    # print(str(aucroc_data))
    # Tooltip text for lines (FPR, TPR, fraction)
    aucroc_data <- aucroc_data %>%
      mutate(tooltip_line = paste0("FPR: ", round(fpr, 3), "<br>TPR: ", round(tpr, 3), "<br>Fraction: ", fraction))
    
    # Tooltip text for points (AUC value)
    aucroc_data <- aucroc_data %>%
      mutate(tooltip_point = paste0("AUC: ", round(auc, 4)))
    
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
                                         filename = paste0("auc_depth_",input$aucroc_seqdepth_select, "_", input$aucroc_approach_select, "_", input$aucroc_deconvtool_select, "_", input$aucroc_dmrtool_select, "_", Sys.Date())
      ))
  })
  
  # Save dataframe AUCROC plot as csv
  output$download_aucroc_df <- downloadHandler(
    filename = function() {
      paste0("auc_depth_",input$aucroc_seqdepth_select, "_", input$aucroc_approach_select, "_", input$aucroc_deconvtool_select, "_", input$aucroc_dmrtool_select, "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      aucroc_data <- create_aucroc_data()
      req(nrow(aucroc_data) > 0)  
      write.csv(aucroc_data, file, row.names = FALSE)
    }
  )
  
  
  ############################################################################ 
  ## RMSE comparison Plot
  # Dropdowns and checkboxes RMSE comparison plot 
  updateSelectInput(session, "rmse_comparison_seqdepth_select", 
                    choices = sort(unique(bench$seq_depth)), 
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] )
  updateSelectInput(session, "rmse_comparison_approach_select", 
                    choices = sort(unique(bench$collapse_approach)), 
                    selected = sort(unique(bench$collapse_approach))[1])
  updateSelectInput(session, "rmse_comparison_expectedtf_select", 
                    choices = sort(unique(bench$expected_tf[bench$expected_tf != 0])), 
                    selected = if (0.1 %in% bench$expected_tf) 0.1 else sort(unique(bench$expected_tf[bench$expected_tf != 0]))[1] )
  
  # updateCheckboxGroupInput(session, "rmse_comparison_deconvtools_select", 
  #                          choices = sort(unique(bench$deconv_tool)), 
  #                          selected = sort(unique(bench$deconv_tool)))
  updateCheckboxGroupInput(session, "rmse_comparison_dmrtools_select", 
                           choices = sort(unique(bench$dmr_tool)), 
                           selected = sort(unique(bench$dmr_tool)))
  
  observe({
    current_choices <- sort(unique(bench$deconv_tool))  # Get all available tools
    
    # Update the checkbox group based on select all/none toggle
    updateCheckboxGroupInput(
      session, "rmse_comparison_deconvtools_select",
      choices = current_choices,
      selected = if (input$rmse_comparison_deconvtools_select_all) current_choices else character(0) # Select all if TRUE, else deselect all
    )
  })
  
  # RMSE tool comparison Data Filtering
  filtered_data_rmse_comparison <- reactive({
    req(input$rmse_comparison_seqdepth_select,input$rmse_comparison_approach_select, input$rmse_comparison_expectedtf_select, input$rmse_comparison_deconvtools_select, input$rmse_comparison_dmrtools_select)
    bench %>%
      filter(seq_depth == input$rmse_comparison_seqdepth_select,
             collapse_approach == input$rmse_comparison_approach_select, 
             expected_tf == input$rmse_comparison_expectedtf_select,
             expected_tf != 0, # Exclude expected_tf == 0
             dmr_tool %in% input$rmse_comparison_dmrtools_select,
             deconv_tool %in% input$rmse_comparison_deconvtools_select
      )
  })
  
  # Create a function to generate the RMSE comparison plot
  create_rmse_comparison_plot <- function(data) {
    
    # Calculate RMSE
    plot_data <- data %>%
      #filter(deconv_tool != 'Methyl_Resolver') %>%
      group_by(deconv_tool, dmr_tool) %>%
      summarise(RMSE = rmse(expected_tf, predicted_tf), .groups = "drop")
    
    # Rank the tools by mean RMSE
    ranked_tools <- plot_data %>%
      group_by(deconv_tool) %>%
      summarise(MeanRMSE = mean(RMSE, na.rm = TRUE)) %>%
      arrange(desc(MeanRMSE)) %>%
      pull(deconv_tool)  # Extract ordered deconv_tool names
    
    # Reorder tools based on calculated ranking
    plot_data <- plot_data %>%
      mutate(deconv_tool = factor(deconv_tool, levels = ranked_tools)) 
    
    # The tools with lower RMSE (better performance) will appear at the top of the y-axis.
    # The tools with higher RMSE (worse performance) will appear at the bottom of the y-axis.
    
    # Reorder the tools globally
    plot_data <- plot_data %>%
      # mutate(deconv_tool = factor(deconv_tool, levels = median_diff$deconv_tool)) %>%
      mutate(tooltip_text = paste0("dmr_tool: ", dmr_tool, 
                                  "<br>deconv_tool: ", deconv_tool,
                                  "<br>RMSE: ", round(RMSE, 4)))
    
    # Generate the plot
    ggplot(plot_data, aes(x = RMSE, y = deconv_tool, color = dmr_tool, text = tooltip_text)) +
      geom_point(size = 3, alpha = 0.8, position = position_jitter(width = 0, height = 0) ) +
      scale_y_discrete(labels = function(y) str_replace_all(y, "_", " ")) +
      labs(
        #title = paste0("RMSE vs Tool (Expected Fraction: ", fraction, ")"),
        x = "RMSE",
        y = "",
        color = "dmr_tool",
        shape = "dmr_tool"
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
      config(toImageButtonOptions = list(format = c("svg","pdf"),
                                         filename = paste0("ranking_deconvtools_fraction_", as.numeric(input$rmse_comparison_expectedtf_select),"_depth_",input$rmse_comparison_seqdepth_select,"_", Sys.Date())
      ))
  })
  
  # Save dataframe rmse comparison plot as csv
  output$download_rmse_comparison_df <- downloadHandler(
    filename = function() paste0("ranking_deconvtools_fraction_", as.numeric(input$rmse_comparison_expectedtf_select), "_depth_", input$rmse_comparison_seqdepth_select,"_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      data <- filtered_data_rmse_comparison() %>%
        group_by(deconv_tool, dmr_tool) %>%
        summarise(RMSE = rmse(expected_tf, predicted_tf), .groups = "drop") %>%
        select(deconv_tool, dmr_tool, RMSE)
      req(nrow(data) > 0)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  ############################################################################ 
  ## Final ranking of the tools 
  # Dropdowns and checkboxes Rank tools
  updateSelectInput(session, "rank_seqdepth_select",
                    choices = sort(unique(bench$seq_depth)),
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] ) 
  updateSelectInput(session, "rank_approach_select", 
                    choices = sort(unique(bench$collapse_approach)), 
                    selected = sort(unique(bench$collapse_approach))[1])
  
  # updateCheckboxGroupInput(session, "rank_deconvtools_select", 
  #                          choices = sort(unique(bench$deconv_tool)), 
  #                          selected = sort(unique(bench$deconv_tool)))
  updateCheckboxGroupInput(session, "rank_dmrtools_select", 
                           choices = sort(unique(bench$dmr_tool)), 
                           selected = sort(unique(bench$dmr_tool)))
  updateSelectInput(session, "rank_metric_select", 
                    choices =  c("meanAUC", "RMSE", "SCC", "Score"), 
                    selected = c("meanAUC", "RMSE", "SCC", "Score")[1])
  
  observe({
    current_choices <- sort(unique(bench$deconv_tool))  # Get all available tools
    
    # Update the checkbox group based on select all/none toggle
    updateCheckboxGroupInput(
      session, "rank_deconvtools_select",
      choices = current_choices,
      selected = if (input$rank_deconvtools_select_all) current_choices else character(0) # Select all if TRUE, else deselect all
    )
  })
  
  
  # Merge AUC-ROC, RMSE, SCC
  merge_metrics_rank <- reactive({
    req(input$rank_seqdepth_select, # Ensure inputs exist before running
        input$rank_approach_select,
        input$rank_dmrtools_select,
        input$rank_deconvtools_select)
    
    # Initialize an empty dataframe
    aucroc_data <- data.frame()
    fractions_auc <- unique(bench[bench$expected_tf != 0, "expected_tf"])
    miss <- c() # Initialize missing tool tracker
    
    filt_df <- bench %>%
      filter(seq_depth == input$rank_seqdepth_select,
             collapse_approach == input$rank_approach_select,
             dmr_tool %in% input$rank_dmrtools_select,
             deconv_tool %in% input$rank_deconvtools_select)
    
    # Loop only through selected user inputs
    for (dmr_tool in input$rank_dmrtools_select) {
      for (deconv_tool in input$rank_deconvtools_select) {
        for (fraction in fractions_auc) {
          
          filt_df2 <- filt_df %>%
            filter(expected_tf %in% c(0, fraction),
                   dmr_tool == !!dmr_tool,
                   deconv_tool == !!deconv_tool)
          
          if (length(unique(filt_df2$expected_tf)) != 2) {
            miss <- c(miss, deconv_tool)
            next
          }
          
          roc_curve <- roc(filt_df2$expected_tf, filt_df2$predicted_tf)
          aucroc_data <- rbind(aucroc_data, data.frame(                
            auc = roc_curve$auc,                                       
            fraction = fraction,                                       
            deconv_tool = deconv_tool,                                 
            dmr_tool = dmr_tool,                                       
            collapse_approach = input$rank_approach_select,            
            seq_depth = input$rank_seqdepth_select                     
          ))                                                           
        }
      }
    }

    # Compute mean AUC Grouped Performance
    classif_performance_auc <- aucroc_data %>%                        
      group_by(deconv_tool, dmr_tool, collapse_approach, seq_depth) %>%  
      summarize(meanAUC = mean(auc), .groups = 'drop')                

    
    
    # Compute RMSE for fractions > 0
    nonzero_fraction <- filt_df %>%
      filter(expected_tf != 0) %>%
      group_by(deconv_tool, dmr_tool, collapse_approach, seq_depth) %>%
      summarize(RMSE = rmse(expected_tf, predicted_tf), .groups = 'drop')
    
    # Compute Spearman's rank correlation coefficient (SCC) on all fractions
    all_fractions <- filt_df %>%
      group_by(deconv_tool, dmr_tool, seq_depth, collapse_approach) %>%
      summarize(SCC = scc(expected_tf, predicted_tf), .groups = 'drop')
    
    # Merge all computed metrics
    merged_metrics <- merge(all_fractions, nonzero_fraction, by = c("deconv_tool", "dmr_tool", "collapse_approach", "seq_depth"))
    merged_metrics <- merge(merged_metrics, classif_performance_auc, by = c("deconv_tool", "dmr_tool", "collapse_approach", "seq_depth"))
    return(merged_metrics)
  })
  
  normalize_metrics <- reactive({ 
    # Retrieve merged metrics from reactive function
    merged_metrics <- merge_metrics_rank()
    
    # Normalize all the metrics based on user-selected inputs
    normalized_list <- list()
    miss <- list()
    
    for (dmr_tool in input$rank_dmrtools_select) {
      tmp <- merged_metrics %>%
        filter(dmr_tool == dmr_tool,
               collapse_approach == input$rank_approach_select,
               seq_depth == input$rank_seqdepth_select)
      
      # Skip if df is empty
      if (dim(tmp)[1] == 0) {
        miss <- c(miss, dmr_tool)
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
      key <- paste0(dmr_tool, input$rank_seqdepth_select, input$rank_approach_select, sep = "_")
      normalized_list[[key]] <- tmp[, c("deconv_tool", "dmr_tool", "collapse_approach", "seq_depth", 
                                        "meanAUC", "RMSE", "SCC", "normAUC", "normSCC", "normRMSE" )]
    }
    
    # Combine all normalized subsets into a single data frame
    normalized_df <- do.call(rbind, normalized_list)
    
    # Create a combined metric score
    filt_df <- bench %>%
      filter(seq_depth == input$rank_seqdepth_select,
             collapse_approach == input$rank_approach_select,
             dmr_tool %in% input$rank_dmrtools_select, 
             deconv_tool %in% input$rank_deconvtools_select)
    
    nzeros <- nrow(filt_df[filt_df$expected_tf == 0,])
    nnonzeros <- nrow(filt_df[filt_df$expected_tf != 0,])
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
      mutate(tooltip_text = paste0(metric,": ", round(.data[[metric]], 3), 
                                  "<br>deconv_tool: ", deconv_tool, 
                                  "<br>dmr_tool: ", dmr_tool))
    
    # Calculate mean score if not already available (or use from existing data)
    mean_score <- data %>%
      group_by(deconv_tool) %>%
      summarise(Mean=sum(.data[[metric]])/length(input$rank_dmrtools_select)) %>%
      arrange(if (metric == "RMSE") desc(Mean) else Mean)  # Reverse order for RMSE
    
    # Reorder the tools globally without adding 'meanScore' column to the dataframe
    data <- data %>%
      mutate(deconv_tool = factor(deconv_tool, levels = mean_score$deconv_tool))
    
    # Tools with best performance are placed on top.
    # Tools with worst performance are placed at the bottom. 
    
    ggplot(data, aes(y = as.factor(deconv_tool), x = .data[[metric]], color = dmr_tool, text = tooltip_text)) +
      geom_point(size = 3, alpha = 0.8) +
      labs(
        #title = paste0("Tools Ranked by ", metric),
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
                                         filename = paste0("rank_",input$rank_metric_select,"_depth_",input$rank_seqdepth_select, "_",input$rank_approach_select,"_",Sys.Date())
      ))
  })
  
  # Download data of rank plots
  output$download_rank_df <- downloadHandler(
    filename = function() {
      paste0("rank_",input$rank_metric_select,"_depth_",input$rank_seqdepth_select, "_",input$rank_approach_select,"_",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      merged_metrics <- merge_metrics_rank()
      normalized_df <- normalize_metrics()
      df <- normalized_df %>% select(-normAUC, -normSCC, -normRMSE)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  
  ############################################################################ 
  # # ## Final ranking of the tools static
  # #
  # # Merge AUC-ROC, RMSE, SCC
  # merge_metrics_rank_static <- reactive({
  # 
  #   filt_df <- bench %>%
  #     filter(!is.na(deconv_tool), !is.na(dmr_tool))
  # 
  #   # Initialize an empty dataframe
  #   aucroc_data <- data.frame()
  #   fractions_auc <- unique(bench[bench$expected_tf != 0, "expected_tf"])
  #   miss <- c() # Initialize missing tool tracker
  # 
  #   # Loop only through selected user inputs
  #   for (dmr_tool in unique(bench$dmr_tool)) {
  #     for (deconv_tool in unique(bench$deconv_tool)) {
  #       for (fraction in fractions_auc) {
  # 
  #         filt_df2 <- filt_df %>%
  #           filter(expected_tf %in% c(0, fraction),
  #                  dmr_tool == !!dmr_tool,
  #                  deconv_tool == !!deconv_tool)
  # 
  #         if (length(unique(filt_df2$expected_tf)) != 2) {
  #           miss <- c(miss, deconv_tool)
  #           next
  #         }
  # 
  #         roc_curve <- roc.obj(filt_df2$expected_tf, filt_df2$predicted_tf)
  #         tmp <- data.frame(
  #           fpr = 1-rev(roc_curve$specificities),  # False Positive Rate
  #           tpr = rev(roc_curve$sensitivities),  # True Positive Rate
  #           thresholds = rev(roc_curve$thresholds),
  #           auc = rev(roc_curve$auc),
  #           fraction = fraction,
  #           deconv_tool = deconv_tool,
  #           dmr_tool = dmr_tool
  #         )
  #         aucroc_data <- rbind(aucroc_data, tmp)
  #       }
  #     }
  #   }
  # 
  #   # Compute mean AUC Grouped Performance
  #   classif_performance_auc <- aucroc_data %>%
  #     group_by(fraction, deconv_tool, dmr_tool) %>%
  #     summarize(AUC = mean(auc), .groups = 'drop') %>%
  #     group_by(deconv_tool, dmr_tool) %>%
  #     summarize(meanAUC = mean(AUC), .groups = 'drop')
  # 
  #   # Compute RMSE for fractions > 0
  #   nonzero_fraction <- filt_df %>%
  #     filter(expected_tf != 0) %>%
  #     group_by(deconv_tool, dmr_tool, seq_depth, collapse_approach) %>%
  #     summarize(RMSE = replace_na(rmse(expected_tf, predicted_tf),1), .groups = 'drop') %>%
  #     group_by(deconv_tool,dmr_tool) %>%
  #     summarize(meanRMSE = mean(RMSE), .groups = 'drop')
  # 
  #   # Compute Spearman's rank correlation coefficient (SCC) on all fractions
  #   all_fractions <- filt_df %>%
  #     group_by(deconv_tool, dmr_tool, seq_depth, collapse_approach) %>%
  #     summarize(SCC = replace_na(scc(expected_tf, predicted_tf),0), .groups = 'drop') %>%
  #     group_by(deconv_tool,dmr_tool) %>%
  #     summarize(meanSCC = mean(SCC))
  # 
  #   # Merge all computed metrics
  #   merged_metrics <- merge(all_fractions, nonzero_fraction, by = c("deconv_tool", "dmr_tool"))
  #   merged_metrics <- merge(merged_metrics, classif_performance_auc, by = c("deconv_tool", "dmr_tool"))
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
  #   for (dmr_tool in unique(bench$dmr_tool)) {
  #     tmp <- merged_metrics %>%
  #       filter(dmr_tool == dmr_tool)
  # 
  #     # Skip if df is empty
  #     if (dim(tmp)[1] == 0) {
  #       miss <- c(miss, dmr_tool)
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
  #     normalized_list[[dmr_tool]] <- tmp[, c("deconv_tool", "dmr_tool",
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
  #   nzeros <- nrow(bench[bench$expected_tf == 0,])
  #   nnonzeros <- nrow(bench[bench$expected_tf != 0,])
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
  #     select(deconv_tool, dmr_tool, !!sym(metric_name)) %>%
  #     rename(Value = !!sym(metric_name))
  # 
  #   # Sort tools manually
  #   tool_levels <- metric_data %>%
  #     group_by(deconv_tool) %>%
  #     summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  #     arrange(if (desc_order) Value else desc(Value)) %>%
  #     pull(deconv_tool)
  # 
  #   # Apply tool order explicitly
  #   metric_data$deconv_tool <- factor(metric_data$deconv_tool, levels = tool_levels)
  # 
  #   ggplot(metric_data, aes(x = Value, y = deconv_tool, color = dmr_tool)) +
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
  #     filename = function() paste0("rank_static_",Sys.Date(),".", ext, sep = ""),
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
  #     paste0("rank_static_",Sys.Date(), ".csv", sep = "")
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
  updateSelectInput(session, "heatmap_seqdepth_select",
                    choices = sort(unique(bench$seq_depth)),
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] ) 
  updateSelectInput(session, "heatmap_approach_select", 
                    choices = sort(unique(bench$collapse_approach)), 
                    selected = sort(unique(bench$collapse_approach))[1])
  
  # updateCheckboxGroupInput(session, "heatmap_decovtools_select", 
  #                          choices = sort(unique(bench$deconv_tool)), 
  #                          selected = sort(unique(bench$deconv_tool)))    
  updateSelectInput(session, "heatmap_dmrtool_select", 
                    choices = sort(unique(bench$dmr_tool)), 
                    selected = sort(unique(bench$dmr_tool))[1])    
  
  observe({
    current_choices <- sort(unique(bench$deconv_tool))  # Get all available tools
    
    # Update the checkbox group based on select all/none toggle
    updateCheckboxGroupInput(
      session, "heatmap_deconvtools_select",
      choices = current_choices,
      selected = if (input$heatmap_deconvtools_select_all) current_choices else character(0) # Select all if TRUE, else deselect all
    )
  })
  
  # Filter data for heatmap
  create_heatmap_data <- reactive({
    req(input$heatmap_seqdepth_select, 
        input$heatmap_approach_select, 
        input$heatmap_deconvtools_select, 
        input$heatmap_dmrtool_select)
    
    data <- bench %>%
      filter(dmr_tool == input$heatmap_dmrtool_select,
             expected_tf != 0, 
             deconv_tool %in% input$heatmap_deconvtools_select,
             seq_depth == input$heatmap_seqdepth_select,
             collapse_approach == input$heatmap_approach_select) %>%
      group_by(deconv_tool,expected_tf) %>%  # Group by tool and tumoral fraction
      summarize(RMSE = rmse(expected_tf, predicted_tf), .groups = "drop")
    
    return(data)
  })
  
  # Create a function to generate the heatmap
  create_heatmap_plot <- function(plot_data) {
    # Rank tools by median RMSE
    median_diff <- plot_data %>%
      group_by(deconv_tool) %>%
      filter(expected_tf==0.0001) %>%
      arrange(RMSE)
    
    # Reorder tools globally
    plot_data <- plot_data %>%
      mutate(deconv_tool = factor(deconv_tool, levels = median_diff$deconv_tool))
    
    
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
    ggplot(plot_data, aes(x = deconv_tool, y = factor(expected_tf), fill = RMSE)) +
      geom_tile() +
      geom_text(aes(
        label = label, 
        color = text_color  # Apply dynamic text color based on tile fill
      ), size = 4) + 
      scale_fill_gradient(low = "white", high = "#9c080d" , limits = c(0,1) ) + # Greyscale color scale ##2f425e"
      scale_color_identity() + 
      scale_x_discrete(labels = function(x) str_replace_all(x, "_", " "))+
      labs(
        x = "",
        y = "Expected Tumoral Fraction"
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
      )+ 
      annotate("segment", x = 0.5, xend = 0.5, y = 0.5, yend = length(unique(plot_data$expected_tf)) + 0.5, color = "black", size = 1)     
  }
  
  # Render heatmap
  output$heatmap <- renderPlot({
    plot_data <- create_heatmap_data()
    create_heatmap_plot(plot_data)
  })
  # tools on the left side of the x-axis are considered better (i.e., lower RMSE), but only at expected_tf == 0.0001.
  
  # Save heatmap using the function
  download_heatmap_plot <- function(ext) {
    downloadHandler(
      filename = function() paste0("heatmap_tools_", input$heatmap_dmrtool_select, "_depth_", input$heatmap_seqdepth_select, "_", input$heatmap_approach_select, "_",Sys.Date(), ".", ext, sep = ""),
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
    filename = function() paste0("heatmap_tools_", input$heatmap_dmrtool_select, "_depth_", input$heatmap_seqdepth_select, "_", input$heatmap_approach_select, "_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      plot_data <- create_heatmap_data()
      write.csv(plot_data, file, row.names = FALSE)
    }
  )
  
  
  
  ############################################################################ 
  ## LoD
  # Dropdowns and checkboxes LoD    
  updateSelectInput(session, "lod_seqdepth_select",
                    choices = sort(unique(bench$seq_depth)),
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] ) 
  updateSelectInput(session, "lod_approach_select", 
                    choices = sort(unique(bench$collapse_approach)), 
                    selected = sort(unique(bench$collapse_approach))[1])
  
  updateSelectInput(session, "lod_deconvtool_select",
                    choices = sort(unique(bench$deconv_tool)),
                    selected = sort(unique(bench$deconv_tool))[1])    
  updateSelectInput(session, "lod_dmrtool_select", 
                    choices = sort(unique(bench$dmr_tool)), 
                    selected = sort(unique(bench$dmr_tool))[1])  
  
  updateSelectInput(session, "lod_plabel_select", 
                    choices = c("p", "p.adj", "p.adj.stars"),
                    selected = "p.adj.stars")
  
  # Define comparisons
  my_comparisons <- list(
    c("0", "1e-04"), 
    c("0", "0.001"), 
    c("0", "0.003"), 
    c("0", "0.007"),
    c("0", "0.01"),
    c("0", "0.025"),
    c("0", "0.05"),
    c("0", "0.1"),
    c("0", "0.25"),
    c("0", "0.5")
  )
  
  # Custom Wilcoxon testing function
  perform_custom_wilcox_tests <- function(data, comparisons, correction.method = "BH") {
    # data: dataframe or matrix with numeric features in columns
    # comparisons: list of length-2 vectors specifying groups to compare
    # correction.method: method for p.adjust (e.g., "BH", "bonferroni", etc.)
    stats <- data.frame()
    for (comp in comparisons) {
      data.filt <- data[data$expected_tf %in% comp, ]
      test <- wilcox.test(predicted_tf ~ expected_tf, data = data.filt, alternative = "less")
      stats <- rbind(stats, data.frame(
        group1 = comp[1],
        group2 = comp[2],
        p = test$p.value
      ))
    }
    stats$p.adj <- p.adjust(stats$p, method = correction.method)
    max_val <- max(data$predicted_tf, na.rm = TRUE)
    stats <- stats %>%
      arrange(as.numeric(group2)) %>%
      mutate(
        y.position = seq(from = max_val *1, to = max_val *1.5, length.out = nrow(stats)),
        p.adj.stars = case_when(
          p.adj <= 0.0001 ~ "****",
          p.adj <= 0.001 ~ "***",
          p.adj <= 0.01 ~ "**",
          p.adj <= 0.05 ~ "*",
          TRUE ~ "ns"
        ),
        p = formatC(p, format = "e", digits = 1),
        p.adj = formatC(p.adj, format = "e", digits = 1)
      )
    return(stats)
  }
  

  # Filter data for LoD
  create_lod_data <- reactive({
    req(input$lod_seqdepth_select, 
        input$lod_approach_select, 
        input$lod_deconvtool_select, 
        input$lod_dmrtool_select)
    
    data <- bench %>%
      filter(dmr_tool == input$lod_dmrtool_select,
             collapse_approach == input$lod_approach_select,
             seq_depth == input$lod_seqdepth_select,
             deconv_tool == input$lod_deconvtool_select)
    return(data)
  })
  
  
  # Create a function to generate the lod plot
  create_lod_plot <- function(data) {
    stats <- perform_custom_wilcox_tests(data, my_comparisons)
    ggplot(data, aes(x = as.factor(expected_tf), y = predicted_tf)) +
      geom_boxplot() +
      labs(
        x = "Expected Tumoral Fraction",
        y = "Estimated Tumoral Fraction (%)"
      ) +
      theme_benchmarking +
      theme(axis.ticks.x = element_blank()) +
      stat_pvalue_manual(stats, label = input$lod_plabel_select)
  }
  
  # Output plot 
  output$lod <- renderPlot({
    data <- create_lod_data()
    create_lod_plot(data)
  })
  
  
  # Save lod plot as svg and pdf
  download_lod <- function(ext) {
    downloadHandler(
      filename = function() paste0("LoD_", input$lod_deconvtool_select, "_depth_", input$lod_seqdepth_select, "_", input$lod_approach_select,"_", input$lod_dmrtool_select, "_" , Sys.Date(), ".", ext, sep = ""),
      content = function(file) {
        data <- create_lod_data()
        req(nrow(data) > 0)
        plot <- create_lod_plot(data)
        
        ggsave(file, plot = plot, width = 10, height = 8, dpi = 300, device = ext)
      }
    )
  }
  output$download_lod_svg <- download_lod("svg")
  output$download_lod_pdf <- download_lod("pdf")
  
  
  # Save dataframe lod as csv
  output$download_lod_df <- downloadHandler(
    filename = function() paste0("LoD_", input$lod_deconvtool_select, "_depth_", input$lod_seqdepth_select, "_", input$lod_approach_select,"_", input$lod_dmrtool_select, "_" , Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      data <- create_lod_data()
      stats <- perform_custom_wilcox_tests(data, my_comparisons)
      write.csv(stats, file, row.names = FALSE)
    }
  )

  ## end 
  
  })
}