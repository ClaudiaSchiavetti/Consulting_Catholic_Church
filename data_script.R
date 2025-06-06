# Data needs to be downloaded and locally stored first. We download from the ASE
# private repository (owned by F.H.). Alternatively, files can be directly 
# downloaded from the repository, but access to it must be granted by the owner 
# - and the code should be modified accordingly.

# useful package to manage packages
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

# install packages if needed and load them
pacman::p_load(tidyverse, dplyr)

# local working directory path: change as needed
setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\ASE-main\\")

# get table names
data_files <- list.files(pattern = "\\.CSV$", full.names = T, recursive = T)
data_files

# initialize lists to store file indices
map_list <- numeric()
map_ts_list <- numeric()
ts_list <- numeric()
other_list <- numeric()

### CHANGE FROM HERE

# Iterate through each CSV file
for (file in data_files) {
  # Read only the first row to get column names
  # Using read_csv from readr for efficiency, with col_names = TRUE and n_max = 0
  header <- tryCatch({
    read_csv(file, col_names = TRUE, n_max = 0)
  }, error = function(e) {
    message("Error reading ", file, ": ", e$message)
    return(NULL)
  })
  
  # Skip if reading failed
  if (is.null(header)) next
  
  # Get column names
  col_names <- colnames(header)
  
  # Check if first column is "Countries" and no column contains "20"
  if (length(col_names) > 0 && col_names[1] == "Countries" && !any(grepl("20", col_names))) {
    map_list <- c(map_list, file)
  }
}

# Output map_list
cat("map_list = c(\n", paste0('  "', map_list, '"', collapse = ",\n"), "\n)\n")

map_list <- c(
  "./Chapter01/Table_01.CSV",
  "./Chapter01/Table_02.CSV",
  "./Chapter01/Table_03.CSV",
  "./Chapter01/Table_04.CSV",
  "./Chapter01/Table_I-a.CSV",
  "./Chapter01/Table_I-b-1.CSV",
  "./Chapter01/Table_I-b-2.CSV",
  "./Chapter01/Table_I-c-1.CSV",
  "./Chapter01/Table_I-c-2.CSV",
  "./Chapter01/Table_I-d.CSV",
  "./Chapter01/Table_I-e.CSV",
  "./Chapter01/Table_I-f.CSV",
  "./Chapter02/Table_05.CSV",
  "./Chapter02/Table_06.CSV",
  "./Chapter02/Table_07.CSV",
  "./Chapter02/Table_08.CSV",
  "./Chapter02/Table_09.CSV",
  "./Chapter02/Table_12.CSV",
  "./Chapter02/Table_13.CSV",
  "./Chapter02/Table_15.CSV",
  "./Chapter02/Table_18.CSV",
  "./Chapter02/Table_II-a.CSV",
  "./Chapter02/Table_II-b-1.CSV",
  "./Chapter02/Table_II-b-2.CSV",
  "./Chapter07/Table_51.CSV",
  "./Chapter07/Table_52.CSV",
  "./Chapter07/Table_53.CSV",
  "./Chapter07/Table_54.CSV",
  "./Chapter07/Table_55.CSV",
  "./Chapter07/Table_56.CSV",
  "./Chapter07/Table_57.CSV",
  "./Chapter07/Table_58.CSV",
  "./Chapter07/Table_59.CSV"
)

## DATA WRANGLING STEPS:

# first, TS files should be pivoted longer (take colnames from "tables overview")

# then, a "Year" column with value="2022" should be added to all non-TS files

# finally, make sure all colnames are unique 


