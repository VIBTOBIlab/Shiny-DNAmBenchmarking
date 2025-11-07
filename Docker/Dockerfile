# Use minimal R base image without Shiny Server
FROM rocker/r-ver:4.4.3

# Metadata
LABEL maintainer="Sofie Van de Velde <sofvdvel.vandevelde@ugent.be>"
LABEL description="DNAm Benchmarking Shiny App"
LABEL version="1.0"
# Set Shiny app directory
ENV SHINY_APP_DIR=/home/app

# Install only missing system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgsl-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/*

# Install R packages required for the Shiny app
RUN Rscript -e " \
    if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes'); \
    remotes::install_github('daattali/shinycssloaders'); \
    remotes::install_github('drostlab/philentropy'); \
    install.packages(c( \
      'askpass', 'backports', 'base64enc', 'brew', 'brio', 'broom', 'bslib', \
      'cachem', 'callr', 'car', 'conflicted', 'crayon', 'data.table', \
      'evaluate', 'fontawesome', 'forcats', 'furrr', 'future', 'future.apply', \
      'ggplot2', 'nloptr', 'ggpubr','ggsignif', 'glue', 'hms', 'LaplacesDemon', 'later', \
      'lifecycle', 'magrittr', 'Metrics', 'Matrix', 'patchwork', 'plotly', \
      'processx', 'ps', 'purrr', 'quarto', 'readr', 'reshape2', 'rstatix', \
      'rstudioapi', 'scales', 'sever', 'shiny', 'shinythemes', 'shinyWidgets', \
      'spsComps', 'stringr', 'tibble', 'tidyr', 'viridisLite', 'xfun' \
    ), dependencies = TRUE) \
  "

# Set CRAN mirror globally
RUN mkdir -p /etc/R && echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' > /etc/R/Rprofile.site

# Copy Shiny app source code
COPY . ${SHINY_APP_DIR}
WORKDIR ${SHINY_APP_DIR}

# Expose default Shiny port
EXPOSE 3838

# Run app
CMD ["Rscript", "-e", "shiny::runApp(Sys.getenv('SHINY_APP_DIR'), host = '0.0.0.0', port = 3838)"]