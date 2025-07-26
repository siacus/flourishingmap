
# Base ARM-compatible Shiny Server image
FROM hvalev/shiny-server-arm:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    cmake \
    gdal-bin \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    libabsl-dev \
    g++ \
    libzstd-dev \
    liblz4-dev \
    libsnappy-dev \
    libthrift-dev \
    less \
    libprotobuf-dev protobuf-compiler \
    libpng-dev \
    libjpeg-dev \
    libcairo2-dev \
    xvfb \
    && rm -rf /var/lib/apt/lists/*

# Install common R packages for Shiny apps
RUN R -e "install.packages(c( \
  'shinydashboard', \
  'shinythemes', \
  'leaflet', \
  'DT', \
  'plotly', \
  'ggplot2', \
  'dplyr', \
  'tidyverse', \
  'data.table', \
  'shinycssloaders', \
  'shinyWidgets', \
  'shiny', \
  'shinyjs', \
  'fileloc', \
  'httr', \
  'jsonlite', \
  'dplyr', \
  'plotly', \
  'tidyr', \
  'lubridate',\
  'shinycssloaders',\
  'arrow', \
  'sf', \
  'viridis', \
  'bslib', \
  'htmlwidgets', \
  'webshot', \
  'ggplot2', \
  'zoo', \
  'scales' \
  ), \
  repos='https://cloud.r-project.org')"

# Expose Shiny Server port
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]


