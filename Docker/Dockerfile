# Use minimal R base image without Shiny Server
FROM rocker/r-ver:4.4.3

# Metadata
LABEL maintainer="Sofie Van de Velde <sofvdvel.vandevelde@ugent.be>"
LABEL description="DNAm Benchmarking Shiny App"
LABEL version="1.0"
LABEL name="rshiny-dnambenchmarking"

# Set Shiny app directory
ENV SHINY_APP_DIR=/home/app

# Set CRAN mirror globally BEFORE installing packages
RUN mkdir -p /etc/R && \
    echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' \
    > /etc/R/Rprofile.site
# Install R packages required for the Shiny app
RUN Rscript -e "install.packages( \
  c('shiny','shinycssloaders', 'bslib','spsComps','ggplot2','plotly', \
  'ggpubr','DT', 'dplyr','purrr','stringr','readr', 'scales','pROC' \
  ), dependencies = TRUE )"

# Copy Shiny app source code
COPY . ${SHINY_APP_DIR}
WORKDIR ${SHINY_APP_DIR}

# Expose default Shiny port
EXPOSE 3838

# Run app
CMD ["Rscript", "-e", "shiny::runApp(Sys.getenv('SHINY_APP_DIR'), host = '0.0.0.0', port = 3838)"]