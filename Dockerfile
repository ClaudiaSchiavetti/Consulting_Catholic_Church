# Base image with R + Shiny
FROM rocker/shiny:latest

# System libraries often needed by R packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev && \
    apt-get clean

# Copy app into image
WORKDIR /app
COPY . /app

# Install R packages used by your app (edit this list)
RUN R -e "install.packages(c( \
  'shiny','dplyr','readr','DT','shinythemes','RColorBrewer','writexl', \
  'plotly','shinyjs','ggplot2','viridis','tibble','leaflet','htmlwidgets' \
), repos='https://cran.rstudio.com/')"

WORKDIR /app
COPY . /app

EXPOSE 3838

# exec form (correct) – note the space in the path is fine here
CMD ["R", "-e", "shiny::runApp('/app/Map app', host='0.0.0.0', port=3838)"]
