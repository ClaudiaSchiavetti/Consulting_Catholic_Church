# Data needs to be downloaded and locally stored first. We download from the ASE
# private repository (owned by F.H.). Alternatively, files can be directly 
# downloaded from the repository, but access to it must be granted by the owner 
# - and the code should be modified accordingly.

# Load package manager
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

# Load required packages
pacman::p_load(tidyverse)

# Set working directory
setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\ASE-main\\")

# Get all CSV file names
data_files <- list.files(pattern = "\\.CSV$", full.names = T, recursive = T)

# Function to classify dataset according to type
check_file <- function(file) {
  tryCatch({
    # Read only column names
    first_row <- readr::read_csv2(file, n_max = 1, show_col_types = F, 
                                  locale = locale(encoding = "UTF-8"))
    col_names <- colnames(first_row)
    
    # Conditions
    is_countries_first <- col_names[1] %in% c("Countries", "Continents")
    no_year_col <- !any(grepl("20", col_names, fixed = T))
    
    return(list(is_countries_first = is_countries_first, no_year_col = no_year_col))
  }, error = function(e) {
    message("Skipping '", basename(file), "': ", e$message)
    return(list(is_countries_first = F, no_year_col = F))
  })
}

# Filter files for each condition combination
map_names <- data_files[vapply(data_files, function(x) {
  cond <- check_file(x)
  cond$is_countries_first && cond$no_year_col
}, logical(1))]

map_ts_names <- data_files[vapply(data_files, function(x) {
  cond <- check_file(x)
  cond$is_countries_first && !cond$no_year_col
}, logical(1))]

ts_names <- data_files[vapply(data_files, function(x) {
  cond <- check_file(x)
  !cond$is_countries_first && cond$no_year_col
}, logical(1))]

other_names <- data_files[vapply(data_files, function(x) {
  cond <- check_file(x)
  !cond$is_countries_first && !cond$no_year_col
}, logical(1))]

# Read files into named lists
map_list <- lapply(map_names, readr::read_csv2, show_col_types = F, 
                   locale = locale(encoding = "UTF-8"))
names(map_list) <- basename(map_names)

map_ts_list <- lapply(map_ts_names, readr::read_csv2, show_col_types = F, 
                      locale = locale(encoding = "UTF-8"))
names(map_ts_list) <- basename(map_ts_names)

ts_list <- lapply(ts_names, readr::read_csv2, show_col_types = F, 
                  locale = locale(encoding = "UTF-8"))
names(ts_list) <- basename(ts_names)

other_list <- lapply(other_names, readr::read_csv2, show_col_types = F, 
                     locale = locale(encoding = "UTF-8"))
names(other_list) <- basename(other_names)

# Print loaded files
cat("map_list (Countries first, no '20' in cols):", paste(basename(map_names), collapse = ", "), "\n")
cat("map_ts_list (Countries first, some '20' in cols):", paste(basename(map_ts_names), collapse = ", "), "\n")
cat("ts_list (Not Countries first, no '20' in cols):", paste(basename(ts_names), collapse = ", "), "\n")
cat("other_list (Not Countries first, some '20' in cols):", paste(basename(other_names), collapse = ", "), "\n")

## DATA WRANGLING STEPS:

# first, TS files should be pivoted longer (take colnames from "tables overview")

# then, a "Year" column with value="2022" should be added to all non-TS files

# finally, make sure all colnames are unique (be careful: do not change "Year")


