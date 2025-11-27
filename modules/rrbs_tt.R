# ==============================================================================
# rrbs_tt.R – UI and Server logic for RRBS - TT benchmarking tab
# This module is part of the DecoNFlow Shiny app
# ==============================================================================

# -----------------------------------------------------------------------
# Packages used in this script (loaded in global.R)
# -----------------------------------------------------------------------
# shiny            : Shiny app framework (UI + server, inputs/outputs, reactivity)
# shinycssloaders  : withSpinner() to show loaders around plots
# plotly           : Interactive plots via plotlyOutput(), renderPlotly(), ggplotly()
# ggplot2          : Static plotting backend for all ggplot-based figures
# DT               : Interactive tables (DT::datatable, DT::renderDataTable)
# dplyr            : Data manipulation (filter, mutate, group_by, summarise, arrange, etc.)
# purrr            : Functional helpers, e.g. map2_dfr() for combining AUC-ROC results
# stringr          : String helpers, e.g. str_replace_all() for nicer labels
# scales           : Labelling and scaling helpers, e.g. label_scientific() for legends
# pROC             : ROC/AUC calculations used in AUC-ROC plots
# ggpubr           : Plot annotations, e.g. stat_pvalue_manual() for LoD p-values
# stats (base)     : Statistical tests and p.adjust(), e.g. wilcox.test()
# utils (base)     : I/O helpers, e.g. write.csv() in download handlers
# grid (base)      : Low-level graphics utilities, e.g. unit() for legend sizing


rrbsttTabUI <- function(id, label = "RRBS-TT") {
  ns <- NS(id)
  tabPanel(
    title = label,
    h3("Benchmarking plots: RRBS-TT", style = "font-weight: bold;"),

    # -----------------------------------------------------------------------
    # Table of Contents
    # -----------------------------------------------------------------------
   
    tags$div(class = "toc-container",
             h4("Table of Contents"),
             tags$ul(class = "toc-list",
                     tags$li(tags$a(href = "#boxplots_rrbstt", "Boxplot of predictions for each tumoral fraction")),
                     tags$li(tags$a(href = "#nrmse_rrbstt", "Performance (NRMSE)")),
                     tags$li(tags$a(href = "#heatmap_rrbstt", "Heatmap of expected tumoral fraction vs deconvolution tools")),
                     tags$li(tags$a(href = "#aucroc_rrbstt", "AUC-ROC for different tumoral fractions")),
                     tags$li(tags$a(href = "#tools-rmse_rrbstt", "Tools RMSE")),
                     tags$li(tags$a(href = "#lod_rrbstt", "Limit of detection (LoD)")),
                     tags$li(tags$a(href = "#final_rrbstt", "Final ranking of deconvolution tools"))
                     )
             ),
    
    tags$div(
      style = "margin: 15px 0;",
      downloadButton(ns("download_rrbstt_df"), "Download RRBS-TT", 
                     style = "width: auto; white-space: nowrap; padding: 10px 20px;")
    ),
    tags$hr(), br(),
    
    # -----------------------------------------------------------------------
    # Boxplots section
    # -----------------------------------------------------------------------
    
    tags$div(id = "boxplots_rrbstt",
             h4("Boxplots of predictions for each tumoral fraction")),
    p("The plot shows predicted tumoral fractions as boxplots for each deconvolution tool (grouped by DMR tool), at a selected expected TF (marked by a red dashed line), allowing comparison of prediction accuracy and variability across methods."
      ),

    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("boxplot_tumortype_select"), 
                     label = "Tumor Type",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("boxplot_seqdepth_select"), 
                     label = "Depth",
                     choices = NULL,
                     selected = NULL
                     ),
                   selectInput(
                     ns("boxplot_exptf_select"),
                     label = "Expected Tumoral Fraction",
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
                withSpinner(plotlyOutput(ns("boxplot_TF"), height = "600px")),
                br(),
                downloadButton(ns("download_boxplot_TF_df"), "Download data"),
                br(), br(), br()
                )
      ),
    tags$hr(), br(), br(),
    
    # -----------------------------------------------------------------------
    # NRMSE section
    # -----------------------------------------------------------------------
    
    tags$div(id = "nrmse_rrbstt", 
             h4("Performance (NRMSE)")),
    p("The plot shows NRMSE values for a selected deconvolution tool across expected tumoral fractions (X-axis) and DMR tools (color), allowing comparison of prediction error normalized by expected value, with lower points indicating better performance."
    ),

    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("nrmse_tumortype_select"), 
                     label = "Tumor Type",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("nrmse_seqdepth_select"), 
                     label = "Depth",
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
                withSpinner(plotlyOutput(ns("nrmse_plot"), height = "600px")),
                br(),
                downloadButton(ns("download_nrmse_plot_df"), "Download data"),
                br(), br(), br()
      )
    ),
    tags$hr(), br(), br(),

    # -----------------------------------------------------------------------
    # Heatmap section
    # -----------------------------------------------------------------------
    
    tags$div(id = "heatmap_rrbstt", 
             h4("Heatmap of expected tumoral fraction vs deconvolution tools")),
    p("The plot shows log2-scaled NRMSE values (tile color) for each deconvolution tool across expected tumoral fractions, with tools ranked left-to-right by their NRMSE at expected TF = 0.0001, where lower values indicate better performance."
      ),
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("heatmap_tumortype_select"), 
                     label = "Tumor Type",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("heatmap_seqdepth_select"),
                     label = "Depth",
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
                   radioButtons(
                     ns("heatmap_dmrtool_select"),
                     label = "DMR Tool",
                     choices = c("DMRfinder", "limma", "wgbstools"),
                     selected = "DMRfinder"
                   )
      ),
      mainPanel(width = 9,
                withSpinner(plotOutput(ns("heatmap"), height = "600px")),
                br(),
                downloadButton(ns("download_heatmap_df"), "Download data"),
                downloadButton(ns("download_heatmap_svg"), "Download as SVG"),
                downloadButton(ns("download_heatmap_pdf"), "Download as PDF"),
                br(), br()
      )
    ),
    tags$hr(), br(), br(),
    
    # -----------------------------------------------------------------------
    # AUC-ROC of tools at 4 low tumoral fractions
    # -----------------------------------------------------------------------
    
    tags$div(id = "aucroc_rrbstt", 
             h4("AUC-ROC for different tumoral fractions")),
    p("The plot shows ROC curves and AUC values for each selected deconvolution tool (faceted), across multiple low tumoral fractions (0.0001 to 0.05), where each line color represents a fraction and higher curves indicate better classification performance."
    ),
    
    # First main panel for the complete AUC-ROC plot
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("aucroc_complete_tumortype_select"), 
                     label = "Tumor Type",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("aucroc_complete_seqdepth_select"), 
                     label = "Depth",
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
                   radioButtons(
                     ns("aucroc_complete_dmrtool_select"),
                     label = "DMR Tool",
                     choices = c("DMRfinder", "limma", "wgbstools"),
                     selected = "DMRfinder"
                   )
                   
      ),
      mainPanel(width = 9,
                withSpinner(plotOutput(ns("aucroc_complete_plot"), height = "800px")),
                br(),
                downloadButton(ns("download_aucroc_complete_df"), "Download data"),
                downloadButton(ns("download_aucroc_complete_svg"), "Download as SVG"),
                downloadButton(ns("download_aucroc_complete_pdf"), "Download as PDF"),
                br(), br(), br()
      )
    ),
    br(),
    
    # Second main panel for on specific AUC-ROC interactive plot
    p("This interactive plot shows ROC curves and AUC values for a selected deconvolution tool across multiple low tumoral fractions (0.0001 to 0.5). Each curve represents a different fraction, and the AUC value is indicated at FPR = 0 for each. Hover over lines and points to view detailed sensitivity, specificity, and AUC metrics. Higher AUC values and curves closer to the top-left indicate better classification performance."
    ), 
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("aucroc_tumortype_select"), 
                     label = "Tumor Type",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("aucroc_seqdepth_select"), 
                     label = "Depth",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("aucroc_deconvtool_select"),
                     label = "Deconvolution Tool",
                     choices = NULL,
                     selected = NULL
                   ),
                   radioButtons(
                     ns("aucroc_dmrtool_select"),
                     label = "DMR Tool",
                     choices = c("DMRfinder", "limma", "wgbstools"),
                     selected = "DMRfinder"
                   ),
                   checkboxGroupInput(
                     ns("aucroc_exptfs_select"),
                     label = "Expected Tumoral Fractions",
                     choices = NULL,
                     selected = NULL
                   ),
                   checkboxInput(ns("aucroc_exptfs_select_all"),label =tags$em("Select All/None"), value = TRUE)
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(
            width = 6,   
            withSpinner(plotlyOutput(ns("aucroc_plot"), height = "500px", width = "100%")),
            br(),
            downloadButton(ns("download_aucroc_df"), "Download data"),
            br(), br()
          ),
          column(
            width = 3,
            div(
              style = "overflow-x:auto; max-height:500px;",
              DT::dataTableOutput(ns("aucroc_table"))
            )
          )
        )
      )
      ),
    tags$hr(), br(), br(),
    
    # -----------------------------------------------------------------------
    # RMSE Comparison section
    # -----------------------------------------------------------------------
    
    tags$div(id = "tools-rmse_rrbstt", 
             h4("Tools RMSE")),
    p("The plot shows RMSE values for each deconvolution tool (Y-axis) across selected DMR tools (colored points) at a specific expected tumoral fraction, with tools sorted top-to-bottom by their mean RMSE, so that lower (better) RMSE tools appear at the top."
      ), 
    
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("rmse_comparison_tumortype_select"), 
                     label = "Tumor Type",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("rmse_comparison_seqdepth_select"),
                     label = "Depth",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("rmse_comparison_expectedtf_select"), 
                     label = "Expected Tumoral Fraction",
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
                withSpinner(plotlyOutput(ns("rmse_comparison"), height = "600px")),
                br(),
                downloadButton(ns("download_rmse_comparison_df"), "Download data"),
                br(), br(), br()
      )
    ),
    tags$hr(), br(), br(),
    
    # -----------------------------------------------------------------------
    # LoD section
    # -----------------------------------------------------------------------
    
    tags$div(id = "lod_rrbstt", 
             h4("Limit of detection (LoD)")),
    p("The plot shows predicted tumoral fractions across increasing expected fractions for a selected tool and DMR method, with boxplots and Wilcoxon test significance markers comparing each level to 0 to assess the lowest fraction at which signal becomes statistically distinguishable from noise."
    ),
    
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("lod_tumortype_select"), 
                     label = "Tumor Type",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("lod_seqdepth_select"),
                     label = "Depth",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("lod_deconvtool_select"),
                     label = "Deconvolution Tool",
                     choices = NULL,  
                     selected = NULL
                   ),
                   radioButtons(
                     ns("lod_dmrtool_select"),
                     label = "DMR Tool",
                     choices = c("DMRfinder", "limma", "wgbstools"),
                     selected = "DMRfinder"
                   ),
                   selectInput(
                     ns("lod_plabel_select"),
                     label = "P-value Label",
                     choices = c("p", "p.adj", "p.adj.signif"),
                     selected = "p.adj.signif"
                   )
      ),
      mainPanel(width = 9,
                withSpinner(plotOutput(ns("lod"), height = "600px")),
                br(),
                downloadButton(ns("download_lod_df"), "Download data"),
                downloadButton(ns("download_lod_svg"), "Download as SVG"),
                downloadButton(ns("download_lod_pdf"), "Download as PDF"),
                br(), br(), br()
      )
    ),
    tags$hr(), br(), br(),
    
    # -----------------------------------------------------------------------
    # final ranking section
    # -----------------------------------------------------------------------
    
    tags$div(id = "final_rrbstt", 
             h4("Final ranking of deconvolution tools")), 
    withMathJax(
      p("To get a unique aggregated metric per combination, individual metrics (RMSE, AUC, SCC, and LoD) are min–max scaled across deconvolution tools at each combination analysed and aggregated into a final score, and then min–max scaled again across tools: ",
        tags$span("\\(\\small \\text{Score} = \\text{RMSE} + \\text{AUC} + \\text{SCC} + \\text{LoD}\\)")
      )
    ),
    sidebarLayout(
      sidebarPanel(width = 3,
                   selectInput(
                     ns("rank_tumortype_select"), 
                     label = "Tumor Type",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("rank_seqdepth_select"),
                     label = "Depth",
                     choices = NULL,
                     selected = NULL
                   ),
                   selectInput(
                     ns("rank_metric_select"), 
                     label = "Normalized metric",
                     choices = c("AUC", "RMSE", "SCC", "LoD", "Score"),
                     selected = "Score"
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
                withSpinner(plotlyOutput(ns("rank"), height = "600px")),
                br(),
                downloadButton(ns("download_rank_df"), "Download data"),
                br(), br(), br()
      ) 
    )
    
  ) # end tabPanel 
} # end rrbsttTabUI


# rrbstab server
rrbsttTabServer <- function(id) {
moduleServer(id, function(input, output, session) {

  #### 1. Filter dataset and basic setup #####
  bench <- subset(tot_bench,
                  seq_method == "rrbs") 
  
  # Sort the levels of depth (e.g. 5M, 10M, 20M, ...)
  bench$seq_depth <- factor(
    bench$seq_depth,
    levels = unique(bench$seq_depth)[order(as.numeric(sub("M", "", unique(bench$seq_depth))))]
  )

  # Download full RRBS-TT benchmark data
  output$download_rrbstt_df <- downloadHandler(
    filename = function() {
      paste0("rrbs_tt_", "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(bench, file, row.names = FALSE)
    }
  )
  
  #### 2. Visualizations #####
  
  # -----------------------------------------------------------------------
  ## Boxplot: predictions for each tumoral fraction
  # -----------------------------------------------------------------------
  
  # Initial inputs
  updateSelectInput(session, "boxplot_tumortype_select", 
                    choices = sort(unique(bench$tumor_type)), 
                    selected = if ("LC" %in% bench$tumor_type) "LC" else sort(unique(bench$tumor_type))[1] )
  updateSelectInput(session, "boxplot_seqdepth_select", 
                    choices = sort(unique(bench$seq_depth)), 
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] )
  updateSelectInput(session, "boxplot_exptf_select", 
                    choices = sort(unique(bench$expected_tf[bench$expected_tf != 0])), 
                    selected = if (0.1 %in% bench$expected_tf) 0.1 else sort(unique(bench$expected_tf[bench$expected_tf != 0]))[1] )
  
  updateCheckboxGroupInput(session, "boxplot_dmrtools_select", 
                           choices = sort(unique(bench$dmr_tool)), 
                           selected = sort(unique(bench$dmr_tool)))
  
  # Select all / none for deconv tools
  observe({
    current_choices <- sort(unique(bench$deconv_tool))
    updateCheckboxGroupInput(
      session, "boxplot_deconvtools_select",
      choices = current_choices,
      selected = if (input$boxplot_deconvtools_select_all) current_choices else character(0) 
    )
  })
  
  # Filter data for boxplot
  filtered_data_boxplot <- reactive({
    req(input$boxplot_tumortype_select, 
        input$boxplot_seqdepth_select, 
        input$boxplot_exptf_select,
        input$boxplot_deconvtools_select, 
        input$boxplot_dmrtools_select)
    
    bench %>%
      dplyr::filter(tumor_type == input$boxplot_tumortype_select,          
             seq_depth == input$boxplot_seqdepth_select,             
             expected_tf == as.numeric(input$boxplot_exptf_select),  
             deconv_tool %in% input$boxplot_deconvtools_select,     
             dmr_tool %in% input$boxplot_dmrtools_select,           
             expected_tf != 0)                                     
  })
  
  # Boxplot function
  create_boxplot_TF <- function(data, seq_depth, expected_tf ) {
    # Rank the tools by median difference
    median_diff <- data %>%
      dplyr::group_by(deconv_tool, dmr_tool) %>%
      dplyr::summarise(Diff = abs(median(expected_tf) - median(predicted_tf)), .groups = 'drop') %>%
      dplyr::group_by(deconv_tool) %>%
      dplyr::summarise(Mean = mean(Diff, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::arrange(Mean)
    # Tools with the smallest mean difference (more accurate: predictions closest to expected) are placed first.
    # Tools with the largest mean difference (less accurate) are placed last.
    
    # Reorder the tools globally
    data <- data %>%
      dplyr::mutate(deconv_tool = factor(deconv_tool, levels = median_diff$deconv_tool))
    
    # Boxplot
    ggplot2::ggplot(data, aes(x = deconv_tool, y = predicted_tf, fill = dmr_tool, color = dmr_tool)) +
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
  
  # Render boxplot
  output$boxplot_TF <- plotly::renderPlotly({
    data <- filtered_data_boxplot()
    req(nrow(data) > 0)
    plot <- create_boxplot_TF(data, input$boxplot_seqdepth_select, as.numeric(input$boxplot_exptf_select))
    
    plotly::ggplotly(plot) %>% 
      plotly::layout(
        legend = list(
          orientation = "h",   # horizontal
          y = 1.1,            # move above the plot
          x = 0.5,             # center horizontally
          xanchor = "center",
          yanchor = "bottom"
        ), boxmode = "group")  %>% 
      plotly::config(toImageButtonOptions = list(format = "png",
                                                 filename = paste0("boxplot_",input$boxplot_tumortype_select, "_", input$boxplot_seqdepth_select,"_fraction_", input$boxplot_exptf_select, "_", Sys.Date())
      ))
  })
  
  # Download boxplot data
  output$download_boxplot_TF_df <- downloadHandler(
    filename = function() {
      paste0("boxplot_",input$boxplot_tumortype_select,"_", input$boxplot_seqdepth_select,"_fraction_", input$boxplot_exptf_select, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- filtered_data_boxplot()
      req(nrow(data) > 0)  
      utils::write.csv(data, file, row.names = FALSE)
    }
  )
  
  # -----------------------------------------------------------------------
  ## NRMSE plot
  # -----------------------------------------------------------------------
  
  # Initial inputs
  updateSelectInput(session, "nrmse_tumortype_select", 
                    choices = sort(unique(bench$tumor_type)), 
                    selected = if ("LC" %in% bench$tumor_type) "LC" else sort(unique(bench$tumor_type))[1] )
  updateSelectInput(session, "nrmse_seqdepth_select", 
                    choices = sort(unique(bench$seq_depth)), 
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] )
  updateSelectInput(session, "nrmse_deconvtool_select", 
                    choices = sort(unique(bench$deconv_tool)), 
                    selected = sort(unique(bench$deconv_tool))[1])
  updateCheckboxGroupInput(session, "nrmse_dmrtools_select", 
                           choices = sort(unique(bench$dmr_tool)), 
                           selected = sort(unique(bench$dmr_tool)))
  
  # Filter data for NRMSE
  filtered_data_nrmse <- reactive({
    req(input$nrmse_tumortype_select, input$nrmse_seqdepth_select, input$nrmse_deconvtool_select, input$nrmse_dmrtools_select)
    data <- bench %>%
      dplyr::filter(tumor_type == input$nrmse_tumortype_select,
                    seq_depth == input$nrmse_seqdepth_select, 
                    deconv_tool == input$nrmse_deconvtool_select, 
                    dmr_tool %in% input$nrmse_dmrtools_select,
                    expected_tf != 0) 
    return(data)
  })   
  
  # Compute NRMSE
  compute_nrmse_data <- function(data) {
    nrmse_data <- data %>%
      # Calculate mean RMSE
      dplyr::group_by(dmr_tool, expected_tf, tumor_type) %>%
      dplyr::summarize(NRMSE = rmse(expected_tf, predicted_tf) / mean(expected_tf), 
                       .groups = "drop") 
    return(nrmse_data)
  }
  
  # NRMSE plot function
  create_plot_nrmse <- function(data) {
    
    data <- data %>%
      dplyr::mutate(tooltip_text = paste0("dmr_tool: ", dmr_tool, 
                                          "<br>Expected Fraction: ", expected_tf, 
                                          "<br>NRMSE: ", round(NRMSE, 4),
                                          "<br>tumor_type: ", tumor_type))
    
    ggplot2::ggplot(data, aes(x = factor(expected_tf), y = NRMSE, color = dmr_tool, text = tooltip_text)) +
      geom_point(size = 3, alpha = 0.8,  position = position_jitter(width = 0, height = 0)) +
      labs(
        x = "Expected fraction",
        y = "NRMSE",
        color = "dmr_tool",
        shape = "dmr_tool"
      ) + theme_benchmarking +  
      scale_y_continuous(expand = expansion(mult = 0.05))  +
      custom_color_manual
  }
  
  # Render NRMSE plot
  output$nrmse_plot <- plotly::renderPlotly({
    data <- filtered_data_nrmse()
    req(nrow(data) > 0)
    nrmse_data <- compute_nrmse_data(data)
    plot <- create_plot_nrmse(nrmse_data)
    plotly::ggplotly(plot, tooltip = "text") %>% # Convert ggplot to interactive plotly
      plotly::layout(
        legend = list(
          orientation = "h",    # horizontal
          y = 1.1,              # move above the plot
          x = 0.5,              # center horizontally
          xanchor = "center",
          yanchor = "bottom"    # anchor from bottom edge of legend box
        )) %>%
      plotly::config(toImageButtonOptions = list(format = c("svg"),
                                                 filename = paste0("NRMSE_",input$nrmse_deconvtool_select, "_", input$nrmse_tumortype_select, "_depth_",input$nrmse_seqdepth_select,"_", Sys.Date()))
      )
  })
  
  # Download NRMSE data
  output$download_nrmse_plot_df <- downloadHandler(
    filename = function() {
      paste0("NRMSE_",input$nrmse_deconvtool_select, "_", input$nrmse_tumortype_select, "_depth_",input$nrmse_seqdepth_select,"_", Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      data <- filtered_data_nrmse()
      req(nrow(data) > 0)  
      nrmse_data <- compute_nrmse_data(data)
      utils::write.csv(nrmse_data, file, row.names = FALSE)
    }
  )
  
  # -----------------------------------------------------------------------
  ## Heatmap
  # -----------------------------------------------------------------------
  
  # Initial inputs
  updateSelectInput(session, "heatmap_tumortype_select", 
                    choices = sort(unique(bench$tumor_type)), 
                    selected = if ("LC" %in% bench$tumor_type) "LC" else sort(unique(bench$tumor_type))[1] )
  updateSelectInput(session, "heatmap_seqdepth_select",
                    choices = sort(unique(bench$seq_depth)),
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] ) 
 
   # Select all / none deconv tools
  observe({
    current_choices <- sort(unique(bench$deconv_tool)) 
    updateCheckboxGroupInput(
      session, "heatmap_deconvtools_select",
      choices = current_choices,
      selected = if (input$heatmap_deconvtools_select_all) current_choices else character(0) 
      )
  })
  
  # Filter data heatmap
  create_heatmap_data <- reactive({
    req(input$heatmap_tumortype_select,
        input$heatmap_seqdepth_select, 
        input$heatmap_deconvtools_select, 
        input$heatmap_dmrtool_select)
    
    data <- bench %>%
      dplyr::filter(tumor_type == input$heatmap_tumortype_select,
                    dmr_tool == input$heatmap_dmrtool_select,
                    expected_tf != 0, 
                    deconv_tool %in% input$heatmap_deconvtools_select,
                    seq_depth == input$heatmap_seqdepth_select) %>%
      dplyr::group_by(deconv_tool,expected_tf, tumor_type) %>%  
      dplyr::summarize(RMSE = rmse(expected_tf, predicted_tf),
                       NRMSE = RMSE / mean(expected_tf),
                       .groups = "drop")
    return(data)
  })
  
  # Heatmap plot function
  create_heatmap_plot <- function(plot_data) {
    
    # Handle NRMSE == 0
    plot_data$NRMSE <- ifelse(plot_data$NRMSE == 0, 1e-6, plot_data$NRMSE)
    
    # Rank tools by median RMSE
    median_diff <- plot_data %>%
      dplyr::group_by(deconv_tool) %>%
      dplyr::filter(expected_tf==0.0001) %>%
      dplyr::arrange(NRMSE)
    
    plot_data$NRMSE <- as.numeric(plot_data$NRMSE)
    
    # Reorder tools globally
    plot_data <- plot_data %>%
      dplyr::mutate(deconv_tool = factor(deconv_tool, levels = median_diff$deconv_tool),
                    expected_tf = factor(expected_tf, levels = sort(unique(expected_tf))),
                    label = ifelse(is.na(NRMSE), "NA", formatC(NRMSE, format = "e", digits = 2))
      )
    
    # Define max value for scaling
    max_val <- max(plot_data$NRMSE, na.rm = TRUE)
    
    # Create and return the heatmap plot
    ggplot2::ggplot(plot_data, aes(x = deconv_tool, y = expected_tf, fill = NRMSE)) +
      geom_tile(color = "gray10") + 
      scale_fill_gradient(
        low = "white", high = "#9c080d",
        limits = c(1e-6, max_val),
        trans = "log2",
        na.value = "gray90",
        labels = scales::label_scientific(digits = 2),
        name = "log2(NRMSE)"
      ) +
      scale_x_discrete(labels = function(x) stringr::str_replace_all(x, "_", " ")) +
      scale_y_discrete(labels = function(x) format(as.numeric(as.character(x)), scientific = FALSE, digits = 3)) +
      labs(
        x = "",
        y = "Expected Tumoral Fraction"
      ) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1, color = "gray10"),
        axis.text.y  = element_text(color = "gray10"),
        axis.title.x = element_text(color = "gray10"),
        axis.title.y = element_text(color = "gray10", margin = margin(r = 25)),
        legend.title = element_text(color = "gray10"),
        legend.text  = element_text(color = "gray10"),
        legend.position = "top",           
        legend.direction = "horizontal",  
        legend.key.width = grid::unit(1.5, "cm"),   # wider legend boxes
        legend.key.height = grid::unit(0.5, "cm"),  # taller legend boxes
        panel.background = element_blank(),
        panel.border     = element_blank(),
        axis.ticks       = element_blank()
      ) +
      annotate(
        "segment",
        x = 0.5, xend = 0.5,
        y = 0.5, yend = length(unique(plot_data$expected_tf)) + 0.5,
        color = "black")
  }
  # Lower NRMSE = better performance (whiter)
  # Higher NRMSE = worse performance (dark red)
  
  # Render heatmap plot
  output$heatmap <- renderPlot({
    plot_data <- create_heatmap_data()
    create_heatmap_plot(plot_data)
  })
  # tools on the left side of the x-axis are considered better (i.e., lower RMSE), but only at expected_tf == 0.0001.
  
  # Save heatmap plot
  download_heatmap_plot <- function(ext) {
    downloadHandler(
      filename = function() paste0("heatmap_tools_", input$heatmap_dmrtool_select, "_", input$heatmap_tumortype_select, "_depth_", input$heatmap_seqdepth_select,"_",Sys.Date(), ".", ext, sep = ""),
      content = function(file) {
        plot_data <- create_heatmap_data()
        plot <- create_heatmap_plot(plot_data)
        ggplot2::ggsave(file, plot = plot, width = 10, height = 6, dpi = 300, device = ext)
        
      }
    )
  }
  output$download_heatmap_svg <- download_heatmap_plot("svg")
  output$download_heatmap_pdf <- download_heatmap_plot("pdf")    
  
  # Save dataframe heatmap as csv
  output$download_heatmap_df <- downloadHandler(
    filename = function() paste0("heatmap_tools_", input$heatmap_dmrtool_select, "_", input$heatmap_tumortype_select, "_depth_", input$heatmap_seqdepth_select,"_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      plot_data <- create_heatmap_data()
      utils::write.csv(plot_data, file, row.names = FALSE)
    }
  )
  
  # -----------------------------------------------------------------------
  # AUCROC complete plot
  # -----------------------------------------------------------------------
  
  # Initial inputs
  updateSelectInput(session, "aucroc_complete_tumortype_select", 
                    choices = sort(unique(bench$tumor_type)), 
                    selected = if ("LC" %in% bench$tumor_type) "LC" else sort(unique(bench$tumor_type))[1] )
  updateSelectInput(session, "aucroc_complete_seqdepth_select", 
                    choices = sort(unique(bench$seq_depth)), 
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] )

  
  # Select all / none deconv tools
  observe({
    current_choices <- sort(unique(bench$deconv_tool))  
    current_choices <- current_choices[current_choices != 0]
    updateCheckboxGroupInput(
      session, "aucroc_complete_deconvtools_select",
      choices = current_choices,
      selected = if (input$aucroc_complete_deconvtools_select_all) current_choices else character(0) 
      )
  })
  
  # Generate AUCROC data
  create_aucroc_complete_data <- reactive({
    req(input$aucroc_complete_tumortype_select,
        input$aucroc_complete_seqdepth_select,
        input$aucroc_complete_deconvtools_select,
        input$aucroc_complete_dmrtool_select)
    
    # Pre-filter dataset
    filtered_bench <- bench %>%
      dplyr::filter(tumor_type ==input$aucroc_complete_tumortype_select,
                    seq_depth == input$aucroc_complete_seqdepth_select,
                    dmr_tool == input$aucroc_complete_dmrtool_select)
    
    # Define all combinations of tools × fractions
    fractions <- c(0.0001, 0.001, 0.01, 0.05)
    combinations <- expand.grid(
      deconv_tool = input$aucroc_complete_deconvtools_select,
      fraction = fractions,
      stringsAsFactors = FALSE
    )
    
    get_auc_data <- function(deconv_tool, fraction) {
      filt_df <- filtered_bench %>%
        dplyr::filter(expected_tf %in% c(0, fraction),
                      deconv_tool == !!deconv_tool)
      
      if (length(unique(filt_df$expected_tf)) != 2 || nrow(filt_df) == 0) {
        # Return an empty data frame instead of NULL
        return(data.frame())
      }
      
      roc_curve <- suppressMessages(
        roc.obj(true_labels = filt_df$expected_tf, predicted_scores = filt_df$predicted_tf)
      )
      
      data.frame(
        fpr = 1 - rev(roc_curve$specificities), # False Positive Rate (1 - specificity), reversed to match increasing thresholds
        tpr = rev(roc_curve$sensitivities),     # True Positive Rate (sensitivity), reversed for increasing thresholds
        thresholds = rev(roc_curve$thresholds), # Thresholds used to compute TPR and FPR, reversed
        auc = rev(roc_curve$auc),               # Area Under the Curve, repeated/reversed to match thresholds
        fraction = fraction,                    # Tumor fraction for this evaluation
        deconv_tool = deconv_tool,              # Name of the deconvolution tool
        dmr_tool = input$aucroc_complete_dmrtool_select, # Name of the DMR tool
        seq_depth = input$aucroc_complete_seqdepth_select,                  # Sequencing depth analysed
        tumor_type = input$aucroc_complete_dmrtool_select,                # Tumor type analysed
        stringsAsFactors = FALSE
      )
    }
    
    # Run the AUC calculation 
    purrr::map2_dfr(
      combinations$deconv_tool,
      combinations$fraction,
      get_auc_data
    )
    
  })
  
  # Generate AUCROC plot with facet_wrap
  create_aucroc_complete_plot <- function(aucroc_complete_data) {
    aucroc_complete_data$deconv_tool <- gsub("_", " ", aucroc_complete_data$deconv_tool)
    
    ggplot2::ggplot(aucroc_complete_data, aes(x = fpr, y = tpr, color = as.factor(fraction), group = fraction)) + 
      geom_line(size = 1) + # ROC Curve Lines
      geom_point(aes(x = 0, y = auc), shape = 1, stroke = 1.5, size = 2, show.legend = FALSE) + # AUC Points (only at x=0)
      labs(
        x = "FPR",
        y = "TPR",
        color = "Tumoral fraction"
      ) + 
      theme_benchmarking +
      facet_wrap(~ deconv_tool, ncol = 4)+ # Adjust ncol to control the number of columns
      theme(
        legend.position = "bottom",
        panel.spacing = grid::unit(1,"lines")
      )
  }
  
  # Render AUCROC plot
  output$aucroc_complete_plot <- renderPlot({
    aucroc_complete_data <- create_aucroc_complete_data()
    req(nrow(aucroc_complete_data) > 0)  
    create_aucroc_complete_plot(aucroc_complete_data)
  })
  
  # Save AUCROC using the function
  download_aucroc_complete_plot <- function(ext) {
    downloadHandler(
      filename = function() paste0("auc_", input$aucroc_complete_tumortype_select, "_depth_",input$aucroc_complete_seqdepth_select,"_",input$aucroc_complete_dmrtool_select,"_" ,Sys.Date(), ".", ext, sep=""),
      content = function(file) {
        req(input$aucroc_complete_deconvtools_select, input$aucroc_complete_dmrtool_select)
        aucroc_complete_data <- create_aucroc_complete_data()
        req(nrow(aucroc_complete_data) > 0)  
        plot <- create_aucroc_complete_plot(aucroc_complete_data)
        ggplot2::ggsave(file, plot = plot, width = 9, height = 10, dpi = 300, device = ext)
      }
    )
  }
  output$download_aucroc_complete_svg <- download_aucroc_complete_plot("svg")
  output$download_aucroc_complete_pdf <- download_aucroc_complete_plot("pdf")
  
  # Save dataframe AUCROC plot as csv
  output$download_aucroc_complete_df <- downloadHandler(
    filename = function() {
      paste0("auc_", input$aucroc_complete_tumortype_select, "_depth_",input$aucroc_complete_seqdepth_select,"_", input$aucroc_complete_dmrtool_select,"_" ,Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(input$aucroc_complete_deconvtools_select, input$aucroc_complete_dmrtool_select)
      aucroc_complete_data <- create_aucroc_complete_data()
      req(nrow(aucroc_complete_data) > 0)  
      utils::write.csv(aucroc_complete_data, file, row.names = FALSE)
    }
  )

  # -----------------------------------------------------------------------
  ## Interactive AUCROC plot
  # -----------------------------------------------------------------------
  
  # Initial inputs
  updateSelectInput(session, "aucroc_tumortype_select", 
                    choices = sort(unique(bench$tumor_type)), 
                    selected = if ("LC" %in% bench$tumor_type) "LC" else sort(unique(bench$tumor_type))[1] )
  updateSelectInput(session, "aucroc_seqdepth_select",
                    choices = sort(unique(bench$seq_depth)),
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] ) 
  updateSelectInput(session, "aucroc_deconvtool_select", 
                    choices = sort(unique(bench$deconv_tool)), 
                    selected = sort(unique(bench$deconv_tool))[1])
  
  # Select all / none expected tfs
  observe({
    current_choices <- sort(unique(bench$expected_tf))  
    current_choices <- current_choices[current_choices != 0]
    updateCheckboxGroupInput(
      session, "aucroc_exptfs_select",
      choices = current_choices,
      selected = if (input$aucroc_exptfs_select_all) current_choices else character(0) 
      )
  })
  
  # Generate AUCROC data
  create_aucroc_data <- reactive({
    req(input$aucroc_tumortype_select,
        input$aucroc_seqdepth_select, 
        input$aucroc_deconvtool_select, 
        input$aucroc_dmrtool_select,
        input$aucroc_exptfs_select)
    
    aucroc_data <- data.frame()
    fractions <- input$aucroc_exptfs_select
    
    for (fraction in unique(fractions)) {
      filt_df <- bench %>%
        dplyr::filter(tumor_type == input$aucroc_tumortype_select,
                      seq_depth == input$aucroc_seqdepth_select,
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
          fpr = 1 - rev(roc_curve$specificities), # False Positive Rate (1 - specificity), reversed to match increasing thresholds
          tpr = rev(roc_curve$sensitivities),     # True Positive Rate (sensitivity), reversed for increasing thresholds
          thresholds = rev(roc_curve$thresholds), # Thresholds used to compute TPR and FPR, reversed
          auc = rev(roc_curve$auc),               # Area Under the Curve, repeated/reversed to match thresholds
          fraction = fraction,                    # Tumor fraction for this evaluation
          deconv_tool = input$aucroc_deconvtool_select,              # Name of the deconvolution tool
          dmr_tool = input$aucroc_dmrtool_select, # Name of the DMR tool
          seq_depth = input$aucroc_seqdepth_select,                  # Sequencing depth analysed
          tumor_type = input$aucroc_tumortype_select,                # Tumor type analysed
          stringsAsFactors = FALSE
        )
        
        
        aucroc_data <- rbind(aucroc_data, tmp)
      }
    }
    return(aucroc_data)
  })
  
  # Function to generate AUC-ROC plot
  create_aucroc_plot <- function(aucroc_data) {
    
    # Ensure correct numeric sorting and scientific labeling of fractions
    aucroc_data <- aucroc_data %>%
      dplyr::mutate(
        fraction = as.numeric(as.character(fraction)),  # ensure numeric
        fraction_label = factor(
          as.character(fraction),  # preserve original display (e.g., "1e-04", "0.001")
          levels = as.character(sort(unique(fraction)))  # sort numerically by value, keep original text
        ),
        tooltip_line = paste0("FPR: ", round(fpr, 3),
                              "<br>TPR: ", round(tpr, 3),
                              "<br>Fraction: ", fraction),
        tooltip_point = paste0("AUC: ", round(auc, 4))
      )
    
    
    ggplot2::ggplot(aucroc_data, aes(x = fpr, y = tpr, color = fraction_label, group = fraction_label)) + 
      geom_line(aes(text = tooltip_line), size = 1) +       # ROC Curve Lines
      geom_point(aes(x = 0, y = auc, text = tooltip_point), shape = 1, stroke = 1.5, size = 2, show.legend = FALSE) + # AUC Points (only at x=0)
      labs(
        x = "FPR",
        y = "TPR",
        color = "Tumoral fraction"
      ) + theme_benchmarking
  }
  
  # Render AUCROC plot
  output$aucroc_plot <- plotly::renderPlotly({
    aucroc_data <- create_aucroc_data()
    req(nrow(aucroc_data) > 0)  
    plot <- create_aucroc_plot(aucroc_data)
    plotly::ggplotly(plot, tooltip = "text") %>%
      plotly::layout(
        legend = list(
          orientation = "h",   # horizontal
          y = -0.5,            # move above the plot
          x = 0.5,             # center horizontally
          xanchor = "center",
          yanchor = "bottom"
        )
      ) %>%
      plotly::config(toImageButtonOptions = list(format = "svg",
                                                 filename = paste0("auc_", input$aucroc_tumortype_select, "_depth_",input$aucroc_seqdepth_select, "_", input$aucroc_deconvtool_select, "_", input$aucroc_dmrtool_select, "_", Sys.Date())
      ))
  })
  
  # Summary table of AUC values per tumoral fraction
  output$aucroc_table <- DT::renderDataTable({
    aucroc_data <- create_aucroc_data()
    req(nrow(aucroc_data) > 0)
    
    # Create display and numeric versions of the fraction
    auc_summary <- aucroc_data %>%
      dplyr::mutate(
        fraction_numeric = as.numeric(as.character(fraction)),
        fraction_label = as.character(fraction) 
      ) %>%
      dplyr::group_by(fraction_numeric, fraction_label) %>%
      dplyr::summarise(AUC = round(mean(auc), 4), .groups = "drop") %>%
      dplyr::arrange(fraction_numeric) %>%
      dplyr::select(`Tumoral Fraction` = fraction_label, AUC)
    
    DT::datatable(
      auc_summary,
      rownames = FALSE,
      options = list(
        dom = 't',
        pageLength = 10,
        autoWidth = TRUE,
        ordering = FALSE
      ),
      class = 'compact'
    )
  })
  
  # Export AUCROC data
  output$download_aucroc_df <- downloadHandler(
    filename = function() {
      paste0("auc_",  input$aucroc_tumortype_select, "_depth_",input$aucroc_seqdepth_select, "_", input$aucroc_deconvtool_select, "_", input$aucroc_dmrtool_select, "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      aucroc_data <- create_aucroc_data()
      req(nrow(aucroc_data) > 0)  
      utils::write.csv(aucroc_data, file, row.names = FALSE)
    }
  )
  
  # -----------------------------------------------------------------------
  ## RMSE comparison Plot
  # -----------------------------------------------------------------------
  
  # Initial inputs
  updateSelectInput(session, "rmse_comparison_tumortype_select", 
                    choices = sort(unique(bench$tumor_type)), 
                    selected = if ("LC" %in% bench$tumor_type) "LC" else sort(unique(bench$tumor_type))[1] )
  updateSelectInput(session, "rmse_comparison_seqdepth_select", 
                    choices = sort(unique(bench$seq_depth)), 
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] )
  updateSelectInput(session, "rmse_comparison_expectedtf_select", 
                    choices = sort(unique(bench$expected_tf[bench$expected_tf != 0])), 
                    selected = if (0.1 %in% bench$expected_tf) 0.1 else sort(unique(bench$expected_tf[bench$expected_tf != 0]))[1] )

  updateCheckboxGroupInput(session, "rmse_comparison_dmrtools_select", 
                           choices = sort(unique(bench$dmr_tool)), 
                           selected = sort(unique(bench$dmr_tool)))
  
  # Select all / none deconvtools
  observe({
    current_choices <- sort(unique(bench$deconv_tool))  
    updateCheckboxGroupInput(
      session, "rmse_comparison_deconvtools_select",
      choices = current_choices,
      selected = if (input$rmse_comparison_deconvtools_select_all) current_choices else character(0)
      )
  })
  
  # RMSE comparison filter data
  filtered_data_rmse_comparison <- reactive({
    req(input$rmse_comparison_tumortype_select,
        input$rmse_comparison_seqdepth_select,
        input$rmse_comparison_expectedtf_select, 
        input$rmse_comparison_deconvtools_select, 
        input$rmse_comparison_dmrtools_select)
    
    bench %>%
      dplyr::filter(tumor_type == input$rmse_comparison_tumortype_select,
                    seq_depth == input$rmse_comparison_seqdepth_select,
                    expected_tf == input$rmse_comparison_expectedtf_select,
                    expected_tf != 0, 
                    dmr_tool %in% input$rmse_comparison_dmrtools_select,
                    deconv_tool %in% input$rmse_comparison_deconvtools_select
      )
  })
  
  # Generate RMSE comparison plot
  create_rmse_comparison_plot <- function(data) {
    
    # Calculate RMSE
    plot_data <- data %>%
      dplyr::group_by(deconv_tool, dmr_tool) %>%
      dplyr::summarise(RMSE = rmse(expected_tf, predicted_tf), .groups = "drop")
    
    # Rank the tools by mean RMSE
    ranked_tools <- plot_data %>%
      dplyr::group_by(deconv_tool) %>%
      dplyr::summarise(MeanRMSE = mean(RMSE, na.rm = TRUE)) %>%
      dplyr::arrange(desc(MeanRMSE)) %>%
      dplyr::pull(deconv_tool)  # Extract ordered deconv_tool names
    
    # Reorder tools based on calculated ranking
    plot_data <- plot_data %>%
      dplyr::mutate(deconv_tool = factor(deconv_tool, levels = ranked_tools)) 
    
    # The tools with lower RMSE (better performance) will appear at the top of the y-axis.
    # The tools with higher RMSE (worse performance) will appear at the bottom of the y-axis.
    
    # Reorder the tools globally
    plot_data <- plot_data %>%
      dplyr::mutate(tooltip_text = paste0("dmr_tool: ", dmr_tool, 
                                          "<br>deconv_tool: ", deconv_tool,
                                          "<br>RMSE: ", round(RMSE, 4)))
    
    # Generate the plot
    ggplot2::ggplot(plot_data, aes(x = RMSE, y = deconv_tool, color = dmr_tool, text = tooltip_text)) +
      geom_point(size = 3, alpha = 0.8, position = position_jitter(width = 0, height = 0) ) +
      scale_y_discrete(labels = function(y) stringr::str_replace_all(y, "_", " ")) +
      labs(
        x = "RMSE",
        y = "",
        color = "dmr_tool",
        shape = "dmr_tool"
      ) + theme_benchmarking + 
      theme(
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey", linewidth = 0.4),
        panel.grid.minor.y = element_blank()
      ) +
      scale_x_continuous(expand = expansion(mult = 0.05)) + 
      custom_color_manual
  }
  
  # Render RMSE comparison plot 
  output$rmse_comparison <- plotly::renderPlotly({
    data <- filtered_data_rmse_comparison()
    req(nrow(data) > 0)
    plot <- create_rmse_comparison_plot(data)
    plotly::ggplotly(plot, tooltip = "text") %>% 
      plotly::layout(
        legend = list(
          orientation = "h",    # horizontal
          y = 1.1,              # move above the plot
          x = 0.5,              # center horizontally
          xanchor = "center",
          yanchor = "bottom"    # anchor from bottom edge of legend box
        )
      ) %>%
      plotly::config(toImageButtonOptions = list(format = c("svg","pdf"),
                                                 filename = paste0("ranking_deconvtools_", input$rmse_comparison_tumortype_select, "_fraction_", as.numeric(input$rmse_comparison_expectedtf_select),"_depth_",input$rmse_comparison_seqdepth_select,"_", Sys.Date())
      ))
  })
  
  # Export RMSE comparison data
  output$download_rmse_comparison_df <- downloadHandler(
    filename = function() paste0("ranking_deconvtools_", input$rmse_comparison_tumortype_select, "_fraction_", as.numeric(input$rmse_comparison_expectedtf_select), "_depth_", input$rmse_comparison_seqdepth_select,"_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      data <- filtered_data_rmse_comparison() %>%
        dplyr::group_by(deconv_tool, dmr_tool) %>%
        dplyr::summarise(RMSE = rmse(expected_tf, predicted_tf), .groups = "drop") %>%
        dplyr::mutate(tumor_type = input$rmse_comparison_tumortype_select) %>%
        dplyr::select(tumor_type, deconv_tool, dmr_tool, RMSE)
      req(nrow(data) > 0)
      utils::write.csv(data, file, row.names = FALSE)
    }
  )
  
  # -----------------------------------------------------------------------
  ## Limit of Detection (LoD)
  # -----------------------------------------------------------------------
  # Initial inputs 
  updateSelectInput(session, "lod_tumortype_select", 
                    choices = sort(unique(bench$tumor_type)), 
                    selected = if ("LC" %in% bench$tumor_type) "LC" else sort(unique(bench$tumor_type))[1] )
  updateSelectInput(session, "lod_seqdepth_select",
                    choices = sort(unique(bench$seq_depth)),
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] ) 
  updateSelectInput(session, "lod_deconvtool_select",
                    choices = sort(unique(bench$deconv_tool)),
                    selected = sort(unique(bench$deconv_tool))[1])    
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
      data.filt <- data %>%
        dplyr::filter(expected_tf %in% comp)
      test <- stats::wilcox.test(predicted_tf ~ expected_tf, data = data.filt, alternative = "less")
      stats <- rbind(stats, data.frame(
        group1 = comp[1],
        group2 = comp[2],
        p = test$p.value
      ))
    }
    stats$p.adj <- stats::p.adjust(stats$p, method = correction.method)
    max_val <- max(data$predicted_tf, na.rm = TRUE)
    stats <- stats %>%
      dplyr::arrange(as.numeric(group2)) %>%
      dplyr::mutate(
        y.position = seq(from = max_val *1, to = max_val *1.5, length.out = nrow(stats)),
        p.adj.stars = dplyr::case_when(
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
  
  
  # Filter LoD data 
  create_lod_data <- reactive({
    req(input$lod_tumortype_select,
        input$lod_seqdepth_select, 
        input$lod_deconvtool_select, 
        input$lod_dmrtool_select)
    
    data <- bench %>%
      dplyr::filter(tumor_type == input$lod_tumortype_select,
                    dmr_tool == input$lod_dmrtool_select,
                    seq_depth == input$lod_seqdepth_select,
                    deconv_tool == input$lod_deconvtool_select)
    return(data)
  })
  
  
  # Create a function to generate the lod plot
  create_lod_plot <- function(data) {
    stats <- perform_custom_wilcox_tests(data, my_comparisons)
    ggplot2::ggplot(data, aes(x = as.factor(expected_tf), y = predicted_tf)) +
      geom_boxplot() +
      labs(
        x = "Expected Tumoral Fraction",
        y = "Estimated Tumoral Fraction"
      ) +
      theme_benchmarking +
      theme(axis.ticks.x = element_blank()) +
      ggpubr::stat_pvalue_manual(stats, label = input$lod_plabel_select)
  }
  
  # Output plot 
  output$lod <- renderPlot({
    data <- create_lod_data()
    create_lod_plot(data)
  })
  
  
  # Save lod plot as svg and pdf
  download_lod <- function(ext) {
    downloadHandler(
      filename = function() paste0("LoD_", input$lod_tumortype_select, "_", input$lod_deconvtool_select, "_depth_", input$lod_seqdepth_select, "_", input$lod_dmrtool_select, "_" , Sys.Date(), ".", ext, sep = ""),
      content = function(file) {
        data <- create_lod_data()
        req(nrow(data) > 0)
        plot <- create_lod_plot(data)
        
        ggplot2::ggsave(file, plot = plot, width = 10, height = 8, dpi = 300, device = ext)
      }
    )
  }
  output$download_lod_svg <- download_lod("svg")
  output$download_lod_pdf <- download_lod("pdf")
  
  
  # Save dataframe lod as csv
  output$download_lod_df <- downloadHandler(
    filename = function() paste0("LoD_", input$lod_tumortype_select, "_", input$lod_deconvtool_select, "_depth_", input$lod_seqdepth_select, "_", input$lod_dmrtool_select, "_" , Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      data <- create_lod_data()
      stats <- perform_custom_wilcox_tests(data, my_comparisons)
      utils::write.csv(stats, file, row.names = FALSE)
    }
  )
  
  # -----------------------------------------------------------------------
  ## Final ranking of the tools 
  # -----------------------------------------------------------------------
  
  ## Helpers: AUC, LoD, min-max, metrics and plotting
  
  # AUC helper ------------------------------------------------------------
  compute_auc <- function(df, fraction, deconv_name) {
    roc_obj <- roc.obj(df$expected_tf, df$predicted_tf)
    data.frame(
      deconv_tool = deconv_name,
      dmr_tool    = unique(df$dmr_tool),
      seq_depth   = unique(df$seq_depth),
      tumor_type  = unique(df$tumor_type),
      expected_tf = fraction,
      auc         = as.numeric(pROC::auc(roc_obj))
    )
  }
  
  # LoD helper ------------------------------------------------------------
  compute_lod <- function(data, comparisons, correction.method = "BH") {
    results <- list()
    
    for (comp in comparisons) {
      group1 <- comp[1]
      group2 <- comp[2]
      
      subset_data <- data %>%
        dplyr::filter(expected_tf %in% comp)
      if (nrow(subset_data) == 0) next
      
      test <- stats::wilcox.test(predicted_tf ~ expected_tf,
                                 data = subset_data,
                                 alternative = "less",
                                 exact       = FALSE   # <- avoid "cannot compute exact p-value with ties"
      )
      
      results[[length(results) + 1]] <- data.frame(
        group1 = group1,
        group2 = group2,
        p      = test$p.value
      )
    }
    
    if (length(results) == 0) return(NULL)
    
    stats <- do.call(rbind, results)
    stats$p.adj <- stats::p.adjust(stats$p, method = correction.method)
    
    max_val <- max(data$predicted_tf, na.rm = TRUE)
    
    stats <- stats %>%
      dplyr::arrange(as.numeric(group2)) %>%
      dplyr::mutate(
        y.position = seq(from = max_val * 1.1,
                         to   = max_val * 1.8,
                         length.out = nrow(stats)),
        star = dplyr::case_when(
          p.adj <= 0.0001 ~ "****",
          p.adj <= 0.001  ~ "***",
          p.adj <= 0.01   ~ "**",
          TRUE            ~ "ns"
        ),
        p     = formatC(p,     format = "e", digits = 1),
        p.adj = formatC(p.adj, format = "e", digits = 1)
      )
    
    return(stats)
  }
  
  # Simple scale01 helper -------------------------------------------------
  scale01 <- function(x) {
    if (all(is.na(x))) return(x)
    rng <- range(x, na.rm = TRUE)
    if (rng[1] == rng[2]) {
      return(rep(0, length(x)))
    } else {
      (x - rng[1]) / (rng[2] - rng[1])
    }
  }
  
  # Filter data helper ----------------------------------------------------
  filter_bench <- function(bench_df,
                           tumor_type_sel,
                           seq_depth_sel,
                           dmrtools_sel,
                           deconvtools_sel) {
    bench_df %>%
      dplyr::filter(
        tumor_type  == tumor_type_sel,
        seq_depth   == seq_depth_sel,
        dmr_tool    %in% dmrtools_sel,
        deconv_tool %in% deconvtools_sel
      )
  }
  
  
  # Compute metrics helper ------------------------------------------------
  compute_metrics <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(list(rmse = NULL, auc = NULL, lod = NULL, scc = NULL))
    }
    
    # Compute RMSE 
    rmse_df <- df %>%
      dplyr::filter(expected_tf != 0) %>%
      dplyr::group_by(deconv_tool, dmr_tool, seq_depth, tumor_type, expected_tf) %>%
      dplyr::summarise(RMSE = rmse(expected_tf, predicted_tf), .groups = "drop") %>%
      dplyr::group_by(dmr_tool, seq_depth, tumor_type, expected_tf) %>%
      dplyr::mutate(
        RMSE_min    = min(RMSE),
        RMSE_max    = max(RMSE),
        RMSE_minmax = ifelse(
          RMSE_max > RMSE_min,
          (RMSE - RMSE_min) / (RMSE_max - RMSE_min),
          0
        )
      ) %>%
      dplyr::group_by(deconv_tool, dmr_tool, seq_depth, tumor_type) %>%
      dplyr::summarise(RMSE = 1 - mean(RMSE_minmax), .groups = "drop")
    
    # Compute AUC
    auc_list <- list()
    unique_fractions <- unique(df$expected_tf)
    unique_fractions <- unique_fractions[unique_fractions != 0]
    
    for (dmrtool in unique(df$dmr_tool)) {
      for (deconv in unique(df$deconv_tool)) {
        
        # Loop over tumoral fractions
        for (fraction in unique_fractions) {
          filt_df <- df %>%
            dplyr::filter(
              expected_tf %in% c(0, fraction),
              dmr_tool    == dmrtool,
              deconv_tool == deconv
            )
          
          # We only skip completely empty combos (to avoid errors),
          if (nrow(filt_df) == 0) {
            next
          }
          auc_list[[length(auc_list) + 1]] <-
            compute_auc(filt_df, fraction, deconv)
        }
      }
    }
    
    auc_df <- if (length(auc_list) > 0) {
      dplyr::bind_rows(auc_list) %>%
        dplyr::group_by(deconv_tool, dmr_tool, seq_depth, tumor_type, expected_tf) %>%
        dplyr::summarise(AUC = mean(auc), .groups = "drop") %>%
        dplyr::group_by(deconv_tool, dmr_tool, seq_depth, tumor_type) %>%
        dplyr::summarise(AUC = mean(AUC), .groups = "drop")
    } else {
      NULL
    }
    
    # Compute LoD
    lod_rows <- list()
    
    for (type in unique(df$tumor_type)) {
      filt_tum <- df %>%
        dplyr::filter(tumor_type == type) 
      
      for (seqdepth in unique(filt_tum$seq_depth)) {
        filt_depth <- filt_tum %>%
          dplyr::filter(seq_depth == seqdepth)
        
        for (dmrtool in unique(filt_depth$dmr_tool)) {
          filt_dmr <- filt_depth %>%
            dplyr::filter(dmr_tool == dmrtool)
          
          for (deconv in unique(filt_dmr$deconv_tool)) {
            filt_deconv <- filt_dmr %>%
              dplyr::filter(deconv_tool == deconv)
            if (nrow(filt_deconv) == 0) next
            
            stats <- compute_lod(filt_deconv, my_comparisons, correction.method = "BH")
            if (is.null(stats)) next
            
            lod_val <- stats %>%
              dplyr::arrange(as.numeric(group2)) %>%
              dplyr::filter(as.numeric(p.adj) < 0.01) %>%
              dplyr::slice(1) %>%
              dplyr::pull(group2)
            
            if (length(lod_val) == 0 || is.na(lod_val)) {
              lod_val <- 1
            }
            
            lod_rows[[length(lod_rows) + 1]] <- data.frame(
              deconv_tool = deconv,
              dmr_tool    = dmrtool,
              tumor_type  = type,
              seq_depth   = seqdepth,
              LoD         = as.numeric(lod_val)
            )
          }
        }
      }
    }
    
    lod_df <- if (length(lod_rows) > 0) {
      dplyr::bind_rows(lod_rows) %>%
        dplyr::group_by(deconv_tool, dmr_tool, seq_depth, tumor_type) %>%
        dplyr::summarise(LoD = mean(LoD, na.rm = TRUE), .groups = "drop")
    } else {
      NULL
    }
    
    # Compute SCC
    scc_df <- df %>%
      dplyr::group_by(deconv_tool, dmr_tool, seq_depth, tumor_type) %>%
      dplyr::summarise(SCC = scc(expected_tf, predicted_tf), .groups = "drop")
    
    list(
      rmse = rmse_df,
      auc  = auc_df,
      lod  = lod_df,
      scc  = scc_df
    )
  }
  
  # Merge to one dataframe helper -----------------------------------------
  merge_metrics_df <- function(metric_list) {
    rmse_df <- metric_list$rmse
    auc_df  <- metric_list$auc
    lod_df  <- metric_list$lod
    scc_df  <- metric_list$scc
    
    if (is.null(rmse_df) || is.null(auc_df) || is.null(lod_df) || is.null(scc_df)) {
      return(NULL)
    }
    
    merged <- rmse_df %>%
      dplyr::left_join(scc_df,
                       by = c("deconv_tool", "dmr_tool", "seq_depth", "tumor_type")) %>%
      dplyr::left_join(auc_df,
                       by = c("deconv_tool", "dmr_tool", "seq_depth", "tumor_type")) %>%
      dplyr::left_join(lod_df,
                       by = c("deconv_tool", "dmr_tool", "seq_depth", "tumor_type"))
    
    merged
  }
  
  # Normalize helper ------------------------------------------------------
  normalize_metrics_df <- function(merged_df) {
    if (is.null(merged_df) || nrow(merged_df) == 0) return(merged_df)
    
    merged_df %>%
      dplyr::group_by(dmr_tool, seq_depth, tumor_type) %>%
      dplyr::mutate(
        normSCC   = scale01(SCC),
        normAUC   = scale01(AUC),
        normRMSE  = scale01(RMSE),
        normLoD   = scale01(LoD),
        Score     = normSCC + normAUC + normRMSE + normLoD,
        normScore = scale01(Score)
      ) %>%
      dplyr::ungroup()
  }
  
  # Plot helper -----------------------------------------------------------
  create_rank_plot <- function(data, metric, tumor_type, seq_depth) {
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Map UI metric -> column name in data
    plot_var <- switch(
      metric,
      "RMSE"  = "normRMSE",
      "SCC"   = "normSCC",
      "AUC"   = "normAUC",
      "LoD"   = "normLoD",
      "Score" = "normScore",
      metric  # fallback (in case you ever pass a norm* directly)
    )
    
    # Nice x-axis label
    x_lab <- if (grepl("^norm", plot_var)) {
      paste0("normalized ", metric)
    } else {
      metric
    }
    
    # Rank tools by mean of the *normalized* metric
    mean_score <- data %>%
      dplyr::group_by(deconv_tool) %>%
      dplyr::summarise(
        Mean = mean(.data[[plot_var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Mean)
    
    data <- data %>%
      dplyr::mutate(
        deconv_tool = factor(deconv_tool, levels = mean_score$deconv_tool)
      )
    
    ggplot2::ggplot(
      data,
      aes(y = deconv_tool, x = .data[[plot_var]], color = dmr_tool
      )
    ) +
      geom_point(size = 3, alpha = 0.8, position = position_jitter(width = 0, height = 0)) +
      scale_y_discrete(labels = function(y) stringr::str_replace_all(y, "_", " ")) +
      labs(
        title = "",
        x     = x_lab,
        y     = ""
      ) +
      theme_benchmarking +
      theme(
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey", linewidth = 0.4),
        panel.grid.minor.y = element_blank()
      )+
      custom_color_manual
    
  }
  
  ## Reactive pipeline for final plot
  
  # Initial inputs
  updateSelectInput(session, "rank_tumortype_select", 
                    choices = sort(unique(bench$tumor_type)), 
                    selected = if ("LC" %in% bench$tumor_type) "LC" else sort(unique(bench$tumor_type))[1] )
  updateSelectInput(session, "rank_seqdepth_select",
                    choices = sort(unique(bench$seq_depth)),
                    selected = if ("20M" %in% bench$seq_depth) "20M" else sort(unique(bench$seq_depth))[1] ) 
  
  updateCheckboxGroupInput(session, "rank_dmrtools_select", 
                           choices = sort(unique(bench$dmr_tool)), 
                           selected = sort(unique(bench$dmr_tool)))
  updateSelectInput(session, "rank_metric_select", 
                    choices =  c("AUC", "RMSE", "SCC", "LoD", "Score"), 
                    selected = "Score")
  
  # Select all / none deconvtools
  observe({
    current_choices <- sort(unique(bench$deconv_tool)) 
    updateCheckboxGroupInput(
      session, "rank_deconvtools_select",
      choices = current_choices,
      selected = if (input$rank_deconvtools_select_all) current_choices else character(0) 
    )
  })
  
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
  
  # 1) filtered data reactive
  filtered_bench_reactive <- reactive({
    req(input$rank_tumortype_select,
        input$rank_seqdepth_select,
        input$rank_dmrtools_select,
        input$rank_deconvtools_select)
    
    filter_bench(
      bench_df         = bench,
      tumor_type_sel   = input$rank_tumortype_select,
      seq_depth_sel    = input$rank_seqdepth_select,
      dmrtools_sel     = input$rank_dmrtools_select,
      deconvtools_sel  = input$rank_deconvtools_select
    )
  })
  
  #  2) compute metrics
  metrics_reactive <- reactive({
    df <- filtered_bench_reactive()
    compute_metrics(df)
  })
  
  # 3) merge metrics
  merged_metrics_reactive <- reactive({
    metric_list <- metrics_reactive()
    merge_metrics_df(metric_list)
  })
  
  # 4) normalize metrics
  norm_metrics_reactive <- reactive({
    merged_df <- merged_metrics_reactive()
    normalize_metrics_df(merged_df)
  })
  
  # 5) plot
  output$rank <- plotly::renderPlotly({
    df <- norm_metrics_reactive()
    req(!is.null(df), nrow(df) > 0)
    p <- create_rank_plot(
      data       = df,
      metric     = input$rank_metric_select,
      tumor_type = input$rank_tumortype_select,
      seq_depth  = input$rank_seqdepth_select
    )
    req(!is.null(p))
    plotly::ggplotly(p) %>%
      plotly::layout(
        legend = list(
          orientation = "h",    # horizontal
          y = 1.1,              # move above the plot
          x = 0.5,              # center horizontally
          xanchor = "center",
          yanchor = "bottom"    # anchor from bottom edge of legend box
        )
      ) %>%
      plotly::config(toImageButtonOptions = list(format = c("svg","pdf"),
                                                 filename = paste0("final_ranking_", input$rank_metric_select, 
                                                                   "_tumortype_", input$rank_tumortype_select,
                                                                   "_depth_",input$rank_seqdepth_select,"_", Sys.Date())
      ))
  })
  
  # 6) Download data
  output$download_rank_df <- downloadHandler(
    filename = function() {
      paste0(
        "Ranking_",
        input$rank_metric_select, "_",
        input$rank_tumortype_select, "_depth_",
        input$rank_seqdepth_select, "_",
        Sys.Date(), ".csv"
      )
    },
    content = function(file) {
      df <- norm_metrics_reactive()
      if (is.null(df) || nrow(df) == 0) {
        utils::write.csv(data.frame(), file, row.names = FALSE)
      } else {
        utils::write.csv(df, file, row.names = FALSE)
      }
      
    })
}) # end moduleServer
}  # end rrbsclTabServer