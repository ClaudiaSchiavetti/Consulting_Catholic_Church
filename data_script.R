# Data needs to be downloaded and locally stored first. We download from the ASE
# private repository (owned by F.H.). Alternatively, files can be directly 
# downloaded from the repository, but access to it must be granted by the owner 
# - and the code should be modified accordingly.

# Load package manager
if (!require("pacman", quietly = T)) install.packages("pacman")
library(pacman)

# Load required packages
pacman::p_load(tidyverse, readxl, stringdist)

# Set working directory
#setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\ASE-main\\")

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

# Process all files -- CHECK FOR ERRORS
all_tables <- lapply(data_files, read_and_check)

# Post-process all tables with replacements in specified order -- CHECK FOR ERRORS
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

# Apply post-processing to all tables -- CHECK FOR ERRORS
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

ts_list <- lapply(all_tables[ vapply(all_tables, function(x) !x$is_region_first && !x$no_year_col, logical(1)) ], `[[`, "data")
names(ts_list) <- basename(vapply(all_tables[ vapply(all_tables, function(x) !x$is_region_first && !x$no_year_col, logical(1)) ], `[[`, "file", FUN.VALUE = character(1)))

other_list <- lapply(all_tables[ vapply(all_tables, function(x) !x$is_region_first && x$no_year_col, logical(1)) ], `[[`, "data")
names(other_list) <- basename(vapply(all_tables[ vapply(all_tables, function(x) !x$is_region_first && x$no_year_col, logical(1)) ], `[[`, "file", FUN.VALUE = character(1)))

#print.data.frame(map_list[[1]])

# Rename the first colnames "Countries" and "Continents" as "Region" in data
# with spatial resolution/geographic scope
map_list <- lapply(map_list, function(x) {
  colnames(x)[1] <- "Region"
  x
})

map_ts_list <- lapply(map_ts_list, function(x) {
  colnames(x)[1] <- "Region"
  x
})

##### STANDARDIZE REGION NAMES

# Your list of final region names
# Read final_regions from the first column (excluding first cell) of the first sheet in Rownames.xlsx
final_regions <- readxl::read_excel("Rownames.xlsx", sheet = 1, range = "A1:A253", col_types = "text")[[1]]

# Function to find the closest matching region name
match_region <- function(name, final_regions) {
  if (is.na(name)) return(NA_character_)
  distances <- stringdist(name, final_regions, method = "jw") # Jaro-Winkler distance
  best_match <- final_regions[which.min(distances)]
  # Return NA if the match is poor (e.g., distance > 0.2)
  if (min(distances) > 0.8) NA_character_ else best_match
}

# Consolidate each tibble in map_list
map_list <- map(map_list, ~ {
  .x %>%
    mutate(Region = sapply(Region, match_region, final_regions = final_regions)) %>%
    group_by(Region) %>%
    summarise(across(.cols = everything(), .fns = ~ first(na.omit(.x))))
})

# Consolidate each tibble in map_ts_list
map_ts_list <- map(map_ts_list, ~ {
  .x %>%
    mutate(Region = sapply(Region, match_region, final_regions = final_regions)) %>%
    group_by(Region) %>%
    summarise(across(.cols = everything(), .fns = ~ first(na.omit(.x))))
})

#####

## DATA WRANGLING STEPS:

# first, TS files should be pivoted longer (take colnames from "data overview")

# then, a "Year" col with value="2022" should be added to files except those in map_list and other_list

# finally, make sure all colnames are unique (be careful: do not change "Year")

# PART 1: MERGE map_list tables

# Function to merge map_list tables (same rows, different columns, add Year=2022)
merge_map_list <- function(map_data_list) {
  cat("Processing map_list tables...\n")
  
  # Start with the first table
  if (length(map_data_list) == 0) {
    cat("No tables in map_list\n")
    return(NULL)
  }
  
  # Initialize with first table
  merged_map <- map_data_list[[1]]
  if (!is.null(merged_map)) {
    # Add Year column
    merged_map$Year <- "2022"
    cat("Starting with table:", names(map_data_list)[1], "- Rows:", nrow(merged_map), "Cols:", ncol(merged_map), "\n")
  }
  
  # Merge remaining tables
  if (length(map_data_list) > 1) {
    for (i in 2:length(map_data_list)) {
      current_table <- map_data_list[[i]]
      table_name <- names(map_data_list)[i]
      
      if (!is.null(current_table) && nrow(current_table) > 0) {
        cat("Merging table:", table_name, "- Rows:", nrow(current_table), "Cols:", ncol(current_table), "\n")
        
        # Add Year column to current table
        current_table$Year <- "2022"
        
        # Get the first column name (should be the same across tables - Countries/Regions)
        join_col <- colnames(current_table)[1]
        
        # Merge by the first column (Countries/Regions)
        merged_map <- dplyr::full_join(merged_map, current_table, by = c(join_col, "Year"))
        
        cat("After merge - Rows:", nrow(merged_map), "Cols:", ncol(merged_map), "\n")
      }
    }
  }
  
  return(merged_map)
}


# PART 2: MERGE map_ts_list tables  

# Function to merge map_ts_list tables (same rows, different years, one table per variable)
merge_map_ts_list <- function(map_ts_data_list) {
  cat("\nProcessing map_ts_list tables...\n")
  
  if (length(map_ts_data_list) == 0) {
    cat("No tables in map_ts_list\n")
    return(NULL)
  }
  
  # Convert each table to long format first, then merge
  long_tables <- list()
  
  for (i in seq_along(map_ts_data_list)) {
    table_name <- names(map_ts_data_list)[i]
    current_table <- map_ts_data_list[[i]]
    
    if (!is.null(current_table) && nrow(current_table) > 0) {
      cat("Processing table:", table_name, "- Rows:", nrow(current_table), "Cols:", ncol(current_table), "\n")
      
      # Get the first column name (Countries/Regions identifier)
      region_col <- colnames(current_table)[1]
      
      # Find year columns (columns that look like years)
      year_cols <- grep("^(19|20)\\d{2}$", colnames(current_table), value = TRUE)
      
      if (length(year_cols) > 0) {
        # Convert to long format
        long_table <- current_table %>%
          tidyr::pivot_longer(
            cols = all_of(year_cols),
            names_to = "Year",
            values_to = table_name  # Use table name as the variable name
          ) %>%
          dplyr::select(all_of(region_col), Year, all_of(table_name))
        
        long_tables[[table_name]] <- long_table
        cat("Converted to long format - Variable:", table_name, "Years:", paste(year_cols, collapse = ", "), "\n")
      } else {
        cat("No year columns found in table:", table_name, "\n")
      }
    }
  }
  
  # Merge all long tables
  if (length(long_tables) > 0) {
    merged_map_ts <- long_tables[[1]]
    region_col <- colnames(merged_map_ts)[1]
    
    if (length(long_tables) > 1) {
      for (i in 2:length(long_tables)) {
        current_long <- long_tables[[i]]
        variable_name <- names(long_tables)[i]
        
        # Merge by region and year
        merged_map_ts <- dplyr::full_join(
          merged_map_ts, 
          current_long, 
          by = c(region_col, "Year")
        )
        
        cat("Merged variable:", variable_name, "- Total rows:", nrow(merged_map_ts), "Cols:", ncol(merged_map_ts), "\n")
      }
    }
    
    return(merged_map_ts)
  }
  
  return(NULL)
}

# EXECUTE THE MERGING

# Merge map_list tables
merged_map_table <- merge_map_list(map_list)

# Merge map_ts_list tables  
merged_map_ts_table <- merge_map_ts_list(map_ts_list)

# PART 3: MERGE THE TWO TABLES TOGETHER

final_merged_table <- NULL

if (!is.null(merged_map_table) && !is.null(merged_map_ts_table)) {
  # Get the region column name (should be the same in both)
  region_col_map <- colnames(merged_map_table)[1]
  region_col_ts <- colnames(merged_map_ts_table)[1]
  
  cat("Region columns - map_table:", region_col_map, "| map_ts_table:", region_col_ts, "\n")
  
  # Check if we need to align column names
  if (region_col_map != region_col_ts) {
    cat("Warning: Region column names don't match. Renaming", region_col_ts, "to", region_col_map, "\n")
    colnames(merged_map_ts_table)[1] <- region_col_map
  }
  
  # Merge by region and year
  final_merged_table <- dplyr::full_join(
    merged_map_table, 
    merged_map_ts_table, 
    by = c(region_col_map, "Year")
  )
  
  cat("Successfully merged both tables!\n")
  
} else if (!is.null(merged_map_table)) {
  cat("Only map_table available, using as final result\n")
  final_merged_table <- merged_map_table
} else if (!is.null(merged_map_ts_table)) {
  cat("Only map_ts_table available, using as final result\n") 
  final_merged_table <- merged_map_ts_table
} else {
  cat("No tables to merge\n")
}

# Optional: Save the final merged table
# write.csv(final_merged_table, "final_merged_map_table.csv", row.names = FALSE)
# save(final_merged_table, file = "final_merged_table.RData")

#To do: 

# Find a way to fix the titles of the variables efficiently 
# Improve the code to get the tables from a github repo (also not a priority)

# PART 4: MERGE ts_list tables 

# NEW FUNCTION: Process ts_list tables without merging them into one final table
process_ts_list <- function(ts_data_list) {
  cat("\nProcessing ts_list tables into a long-format dataset (wide by table)...\n")
  
  if (length(ts_data_list) == 0) {
    cat("No tables in ts_list\n")
    return(NULL)
  }
  
  long_ts_tables <- list()
  
  for (i in seq_along(ts_data_list)) {
    file_name <- names(ts_data_list)[i]
    table_col <- gsub("\\.CSV$", "", basename(file_name))  # e.g., "Table_43-2"
    current_table <- ts_data_list[[i]]
    
    if (!is.null(current_table) && nrow(current_table) > 0) {
      cat("Processing", table_col, "- Rows:", nrow(current_table), "Cols:", ncol(current_table), "\n")
      
      entity_col <- colnames(current_table)[1]
      year_cols <- grep("^(19|20)\\d{2}$", colnames(current_table), value = TRUE)
      
      if (length(year_cols) > 0) {
        long_table <- current_table %>%
          tidyr::pivot_longer(
            cols = all_of(year_cols),
            names_to = "Year",
            values_to = table_col
          ) %>%
          dplyr::select(all_of(entity_col), Year, all_of(table_col))
        
        colnames(long_table)[1] <- "Entity"  # Standardize first column name
        long_ts_tables[[table_col]] <- long_table
      }
    }
  }
  
  # Merge all tables by Entity and Year
  final_dataset <- Reduce(function(x, y) dplyr::full_join(x, y, by = c("Entity", "Year")), long_ts_tables)
  
  return(final_dataset)
}

# EXECUTE
merged_ts_table <- process_ts_list(ts_list)
