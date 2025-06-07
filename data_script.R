# Data needs to be downloaded and locally stored first. We download from the ASE
# private repository (owned by F.H.). Alternatively, files can be directly 
# downloaded from the repository, but access to it must be granted by the owner 
# - and the code should be modified accordingly.

# Load package manager
if (!require("pacman", quietly = T)) install.packages("pacman")
library(pacman)

# Load required packages
pacman::p_load(tidyverse)

# Set working directory
setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\ASE-main\\")

# Get all CSV files
data_files <- list.files(pattern = "\\.CSV$", full.names = T, recursive = T)

# Read all files and check grouping conditions
read_and_check <- function(file) {
  tryCatch({
    # Read entire file
    data <- readr::read_delim(file, delim = ";", show_col_types = F, 
                              locale = locale(encoding = "UTF-8", decimal_mark = ",", grouping_mark = "."))
    col_names <- colnames(data)
    
    # Conditions
    is_region_first <- col_names[1] %in% c("Countries", "Continents")
    no_year_col <- !any(grepl("20", col_names, fixed = T))
    
    return(list(file = file, data = data, is_region_first = is_region_first, no_year_col = no_year_col))
  }, error = function(e) {
    message("Skipping '", basename(file), "': ", e$message)
    return(list(file = file, data = NULL, is_region_first = F, no_year_col = F))
  })
}

# Process all files
all_tables <- lapply(data_files, read_and_check)

# Post-process all tables with replacements in specified order
process_table <- function(table) {
  # Clean first column for invalid UTF-8 and keep as character
  table[[1]] <- iconv(table[[1]], to = "UTF-8", sub = "")
  
  # Process other columns in specified order
  table <- dplyr::mutate(table, dplyr::across(-1, ~{
    . <- iconv(., to = "UTF-8", sub = "") # Clean UTF-8
    . <- dplyr::case_when(
      . == "..." ~ NA_character_, # Replace "..." with NA
      TRUE ~ .
    )
    . <- dplyr::case_when(
      . == ".." ~ "0", # Replace ".." with 0
      TRUE ~ .
    )
    . <- gsub("\\.(?=\\d)", "", ., perl = T) # Remove individual dots between digits
    . <- gsub(",", ".", .) # Replace commas with dots
    . <- dplyr::case_when(
      . == "-" ~ "0", # Replace "-" with 0
      TRUE ~ .
    )
    as.numeric(.) # Convert to numeric
  }))
  
  table
}

# Apply post-processing to all tables
all_tables <- lapply(all_tables, function(x) {
  if (!is.null(x$data)) {
    x$data <- process_table(x$data)
  }
  x
})

# Split data into four lists based on conditions
map_list <- lapply(all_tables[ vapply(all_tables, function(x) x$is_region_first && x$no_year_col, logical(1)) ], `[[`, "data")
names(map_list) <- basename(vapply(all_tables[ vapply(all_tables, function(x) x$is_region_first && x$no_year_col, logical(1)) ], `[[`, "file", FUN.VALUE = character(1)))

map_ts_list <- lapply(all_tables[ vapply(all_tables, function(x) x$is_region_first && !x$no_year_col, logical(1)) ], `[[`, "data")
names(map_ts_list) <- basename(vapply(all_tables[ vapply(all_tables, function(x) x$is_region_first && !x$no_year_col, logical(1)) ], `[[`, "file", FUN.VALUE = character(1)))

ts_list <- lapply(all_tables[ vapply(all_tables, function(x) !x$is_region_first && x$no_year_col, logical(1)) ], `[[`, "data")
names(ts_list) <- basename(vapply(all_tables[ vapply(all_tables, function(x) !x$is_region_first && x$no_year_col, logical(1)) ], `[[`, "file", FUN.VALUE = character(1)))

other_list <- lapply(all_tables[ vapply(all_tables, function(x) !x$is_region_first && !x$no_year_col, logical(1)) ], `[[`, "data")
names(other_list) <- basename(vapply(all_tables[ vapply(all_tables, function(x) !x$is_region_first && !x$no_year_col, logical(1)) ], `[[`, "file", FUN.VALUE = character(1)))


## DATA WRANGLING STEPS:

# first, TS files should be pivoted longer (take colnames from "tables overview")

# then, a "Year" column with value="2022" should be added to all non-TS files

# finally, make sure all colnames are unique (be careful: do not change "Year")


