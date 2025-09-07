FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev libcairo2-dev libsqlite3-dev libpq-dev \
  libssh2-1-dev unixodbc-dev libcurl4-openssl-dev libssl-dev \
  libudunits2-dev libgdal-dev libgeos-dev libproj-dev gdal-bin \
  lbzip2 fontconfig fonts-dejavu-core && \
  apt-get clean && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
 'shiny','dplyr','readr','DT','shinythemes','RColorBrewer','writexl', \
 'plotly','shinyjs','ggplot2','viridis','tibble','leaflet','htmlwidgets', \
 'sf','rnaturalearth','rnaturalearthdata','terra','raster','webshot' \
), repos='https://cran.rstudio.com/')"

# Install PhantomJS for webshot
RUN R -e "webshot::install_phantomjs()"

# Copy your app to the Shiny server directory
COPY . /srv/shiny-server/

# Make sure the shiny user owns the files
RUN chown -R shiny:shiny /srv/shiny-server/

# Expose the Shiny port
EXPOSE 3838

# The rocker/shiny image already has CMD to start shiny-server