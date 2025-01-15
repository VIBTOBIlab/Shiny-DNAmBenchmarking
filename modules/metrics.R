# Metrics Module UI
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
                 ns("fraction_select"), 
                 label = "Select Tumoral Fraction:",
                 choices = NULL,
                 selected = NULL
               )
        ),

      # Checkbox for selecting tools to display
        column(2,
                checkboxGroupInput(
                 ns("tools_select"),
                 label = "Select Deconvolution Tools:",
                 choices = NULL,  
                 inline = FALSE,
                 selected = NULL
               )
        ),

      # Checkbox for selecting DMRtools to display
        column(2,
                checkboxGroupInput(
                 ns("dmrtools_select"),
                 label = "Select DMR Tools:",
                 choices = NULL,  
                 inline = FALSE,
                 selected = NULL
               )        
        )
      ),
      
      # Outputs: Plot and Table
      h3("Boxplots of the predictions for each tumoral fraction"),
      fluidRow(
        column(8, 
               plotOutput(ns("boxplot"), height = "600px")
        )
      ),

      h3("Plot each tool performance (NRMSE) per tumoral fractions"),
      p("We will use the normalized RMSE (NRMSE) in order to make the score size-free: otherwise, increasing the expected value will also determine an increase in RMSE, giving a (wrong) perception that tools perform worse when increasing the tumoral fraction.")
      

    )
  )
}

# Metrics Module Server
metricsTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ## 1. Import the dataset and filter it
    # Load the data
    res_limma <- read.csv("data/Results_20M_limma.csv", row.names = 1)
    res_dmrfinder <- read.csv("data/Results_20M_DMRfinder.csv", row.names = 1)
    res_wgbstools <- read.csv("data/Results_20M_wgbstools.csv", row.names = 1)
    res_cibersort_dmrfinder <- read.csv("data/Results_20M_DMRfinder_cibersort.csv", row.names = 1)
    metadata <- read.csv("data/SamplesMetadata.csv", sep = "\t")[, c("Sample", "Exp.nbl")]
    
    # Combine the datasets with samples metadata for the expected tumoral fraction
    bench <- rbind(res_dmrfinder, res_limma, res_wgbstools, res_cibersort_dmrfinder)
    colnames(metadata) <- c("sample", "expected_fraction")
    bench <- as.data.frame(merge(bench, metadata, by = "sample"))

    # Use only the unbiased dataset
    bench <- subset(
      bench,
      bench$reference == "reference_11healthy_9nbl" & 
        bench$expected_fraction %in% c(0, 0.0001, 0.001, 0.003, 0.007, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5)
    )
    bench$nbl <- round(bench$nbl, 4)

    # Remove spaces at the beginning and end of the strings
    bench$sample <- str_trim(bench$sample, side = c("both", "left", "right"))
    bench$tool <- str_trim(bench$tool, side = c("both", "left", "right"))
    bench$DMRtool <- str_trim(bench$DMRtool, side = c("both", "left", "right"))

    # Remove the string due to the bam file format
    bench$sample <- bench$sample %>% str_replace("_R1_001_val_1_bismark_bt2_pe", "")
    bench <- as.data.frame(unique(bench))

    # Populate dropdown with unique fractions
    updateSelectInput(session, "fraction_select", choices = unique(bench$expected_fraction))

    # Populate checkbox with unique tools
    updateCheckboxGroupInput(session, "tools_select", 
                             choices = unique(bench$tool),
                             selected = unique(bench$tool))  # Default selection includes all tools
    
    # Populate checkbox with unique DMRtools
    updateCheckboxGroupInput(session, "dmrtools_select", 
                             choices = unique(bench$DMRtool),
                             selected = unique(bench$DMRtool))  # Default selection includes all DMRtools

    # Reactive expression for filtered data
    filtered_data <- reactive({
      req(input$fraction_select)  # Ensure a fraction is selected
      req(input$tools_select)  # Ensure tools are selected
      req(input$dmrtools_select)  # Ensure DMRtools are selected
      
      bench %>% 
        filter(expected_fraction == as.numeric(input$fraction_select)) %>%
        filter(tool %in% input$tools_select) %>%  # Filter data based on selected tools
        filter(DMRtool %in% input$dmrtools_select)  # Filter data based on selected DMRtools
    })

    ## 2. Plot the results
    # Boxplots of predictions for the selected tumoral fraction and selected tools
    output$boxplot <- renderPlot({
      data <- filtered_data()
      req(nrow(data) > 0)  # Ensure there's data to plot

      # Rank tools by median difference
      median_diff <- bench %>%
        filter(bench$expected_fraction == as.numeric(input$fraction_select)) %>%
        group_by(tool, DMRtool) %>%
        summarise(Diff = abs(median(expected_fraction) - median(nbl))) %>%
        group_by(tool) %>%
        summarise(Mean = mean(Diff, na.rm = TRUE)) %>%
        arrange(Mean)

      # Reorder the tools globally
      data <- data %>%
        mutate(tool = factor(tool, levels = median_diff$tool))

      # Create plot
      data %>%
        group_by(tool) %>%
        ggplot(aes(x = tool, y = nbl, color = DMRtool)) +
        geom_boxplot() +
        geom_hline(yintercept = as.numeric(input$fraction_select), color = "red", linetype = "dashed") +
        labs(
          title = paste("Expected Fraction:", input$fraction_select),
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
        )
    })
  })
}

