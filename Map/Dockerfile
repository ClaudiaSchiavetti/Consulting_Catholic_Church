# Use the official Rocker Shiny image as base
FROM rocker/shiny:latest

# Install system dependencies for spatial packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libv8-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libmagick++-dev \
    cmake \
    wget \
    chromium \
    chromium-driver \
    && rm -rf /var/lib/apt/lists/*

# Use RStudio Package Manager for faster binary package installation
RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/latest')); \
    install.packages(c('shiny', 'leaflet', 'dplyr', 'readr', 'sf', 'DT', 'shinythemes', \
    'lwgeom', 'rnaturalearth', 'rnaturalearthdata', 'RColorBrewer', 'webshot', 'webshot2', 'mapview', \
    'writexl', 'plotly', 'shinyjs', 'viridisLite', 'ggplot2', 'htmlwidgets', 'purrr'))"

# Verify critical packages
RUN R -e "library(leaflet); library(sf); library(shiny); library(mapview)"

# Remove default shiny apps
RUN rm -rf /srv/shiny-server/*

# Copy your app files
COPY map.R /srv/shiny-server/app.R
COPY final_geo_table.csv /srv/shiny-server/
COPY variable_abbreviations.csv /srv/shiny-server/
COPY styles.css /srv/shiny-server/

WORKDIR /srv/shiny-server
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]