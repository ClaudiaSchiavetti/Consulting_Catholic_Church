FROM rocker/shiny:latest

# System libs (add bzip2 here)
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev libcairo2-dev libsqlite3-dev libpq-dev \
    libssh2-1-dev unixodbc-dev libcurl4-openssl-dev libssl-dev \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev gdal-bin \
    bzip2 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# R packages
RUN R -e "install.packages(c( \
  'shiny','dplyr','readr','DT','shinythemes','RColorBrewer','writexl', \
  'plotly','shinyjs','ggplot2','viridis','tibble','leaflet','htmlwidgets', \
  'sf','rnaturalearth','rnaturalearthdata','terra','raster','webshot' \
), repos='https://cran.rstudio.com/')"

# Pre-install PhantomJS (avoids runtime install + bzip2 issue)
RUN R -e "webshot::install_phantomjs()"

WORKDIR /app
COPY . /app

EXPOSE 3838
# Make sure this matches your actual entry script name
CMD ["Rscript", "/app_map.R"]
