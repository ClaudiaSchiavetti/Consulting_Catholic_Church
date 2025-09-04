# Base image with R + Shiny
FROM rocker/shiny:latest

# System libraries (incl. spatial deps). NOTE: removed the bad 'libmariadbd-dev'
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev libcairo2-dev libsqlite3-dev libpq-dev \
    libssh2-1-dev unixodbc-dev libcurl4-openssl-dev libssl-dev \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev gdal-bin && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# R packages (incl. sf/terra/raster)
RUN R -e "install.packages(c( \
  'shiny','dplyr','readr','DT','shinythemes','RColorBrewer','writexl', \
  'plotly','shinyjs','ggplot2','viridis','tibble','leaflet','htmlwidgets', \
  'sf','rnaturalearth','rnaturalearthdata','terra','raster','webshot' \
), repos='https://cran.rstudio.com/')"

# App
WORKDIR /app
COPY . /app

EXPOSE 3838
CMD [\"Rscript\", \"/app/app_map.R\"]
