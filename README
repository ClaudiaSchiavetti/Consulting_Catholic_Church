# Catholic Church Statistics – Interactive Shiny Applications

This repository contains interactive Shiny applications developed to visualise and explore administrative data from the *Annuarium Statisticum Ecclesiae* (2022 edition).  
The project was developed within the Statistical Consulting module of the Master’s program in Statistics and Data Science at LMU Munich.

## Repository Structure

The repository is organised into three main folders, each corresponding to a Shiny application:

- **map/** – Main geographical application (world maps, data explorer, time series)
- **ispr_men/** – Application for Institutes and Societies of Pontifical Right (men)
- **ispr_women/** – Application for Institutes and Societies of Pontifical Right (women)

All three folders follow the same internal structure.

## Folder Contents

Each application folder contains:

- **app.R** (or `map.R`)  
  The complete Shiny application source code (UI and server logic).

- **final_geo_table.csv**  
  Cleaned and harmonised dataset used by the application.  

- **variable_abbreviations.csv**  
  Lookup table mapping full variable names to shortened labels used in the app.

- **Dockerfile**  
  Defines a self-contained environment to run the application, including:
  - R version
  - required R packages
  - system dependencies  
  This ensures the app runs consistently across machines.

- **docker-compose.yml**  
  Simplifies running the application with Docker by specifying:
  - container configuration
  - exposed ports
  - runtime options  
  Allows the app to be launched with a single command.

