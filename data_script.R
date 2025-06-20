# ============================================================================
# DATA PROCESSING SCRIPT
# ============================================================================

# Data needs to be downloaded and locally stored first. We download from the ASE
# private repository (owned by F.H.). Alternatively, files can be directly 
# downloaded from the repository, but access to it must be granted by the owner 
# - and the code should be modified accordingly.

# ============================================================================
# SETUP AND DEPENDENCIES
# ============================================================================

# Load package manager and additional packages
if (!require("pacman", quietly = T)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, readxl, stringdist)

# Set working directory
setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\ASE-main\\")

# ============================================================================
# STEP 1: DATA DISCOVERY AND INITIAL PROCESSING
# ============================================================================

# Find all CSV files in the directory and subdirectories
data_files <- list.files(pattern = "\\.CSV$", full.names = T, recursive = T)

# Function to read CSV files and label them based on structure
read_and_check <- function(file) {
  tryCatch({
    # Read file with ad-hoc formatting (UTF-8, semicolon delimiter, comma as decimal)
    data <- readr::read_delim(file, delim = ";", show_col_types = F, 
                              locale = locale(encoding = "UTF-8", 
                                              decimal_mark = ",", 
                                              grouping_mark = "."))
    col_names <- iconv(colnames(data), to = "UTF-8", sub = "")
    
    # Categorization conditions:
    # - is_region_first: first column contains geographic regions (Countries/Continents)
    # - no_year_col: table has no Year columns (cross-sectional/snapshot data)
    is_region_first <- col_names[1] %in% c("Countries", "Continents")
    no_year_col <- !any(grepl("20", col_names, fixed = T))
    
    return(list(file = file, data = data, is_region_first = is_region_first, 
                no_year_col = no_year_col))
  }, error = function(e) {
    message("Skipping '", basename(file), "': ", e$message)
    return(list(file = file, data = NULL, is_region_first = F, no_year_col = F))
  })
}

# Read and label CSV files
all_tables <- lapply(data_files, read_and_check)

# ============================================================================
# STEP 2: DATA CLEANING AND TRANSFORMATION
# ============================================================================

# Note: data transformation is performed based on the data notation employed by
# the Annuarium, which can be found in the "Observations" section.

# Function to clean and transform datasets
process_table <- function(table) {
  # Clean first column (region names) for invalid UTF-8 characters
  table[[1]] <- iconv(table[[1]], to = "UTF-8", sub = "")
  
  # Process all other columns with standardized replacements
  table <- dplyr::mutate(table, dplyr::across(-1, ~{
    . <- iconv(., to = "UTF-8", sub = "")                         # Clean UTF-8 encoding
    . <- dplyr::case_when(. == "..." ~ NA_character_, TRUE ~ .)   # Replace "..." with NA
    . <- dplyr::case_when(. == ".." ~ "0", TRUE ~ .)              # Replace ".." with 0
    . <- gsub("\\.(?=\\d)", "", ., perl = T)                      # Remove dots between digits (thousands separator)
    . <- gsub(",", ".", .)                                        # Replace comma with dot (as decimal separator)
    . <- dplyr::case_when(. == "-" ~ "0", TRUE ~ .)               # Replace "-" with 0
    as.numeric(.)                                                 # Convert to numeric
  }))
  
  return(table)
}

# Apply function to all successfully loaded tables
all_tables <- lapply(all_tables, function(x) {
  if (!is.null(x$data)) {
    x$data <- process_table(x$data)
  }
  x
})

#print.data.frame(all_tables[[1]]$data)

# ============================================================================
# STEP 3: DATA QUALITY ASSESSMENT
# ============================================================================

# Function to analyze missing values in each table
check_na_summary <- function(df, file_name) {
  na_counts <- colSums(is.na(df))
  na_percent <- round(100 * na_counts / nrow(df), 1)
  data.frame(
    File = basename(file_name),
    Column = names(na_counts),
    NA_Count = na_counts,
    NA_Percent = na_percent,
    row.names = NULL
  )
}

# Generate missing value summary for all tables
na_summary_list <- lapply(all_tables, function(x) {
  if (!is.null(x$data)) {
    check_na_summary(x$data, x$file)
  }
})

# Combine all missing value summaries into one dataframe
na_summary_df <- do.call(rbind, na_summary_list)

# ============================================================================
# STEP 4: TABLE CATEGORIZATION
# ============================================================================

# Categorize tables into 4 types based on structure:
# 1. map_list: geographic regions + cross-sectional data (no years)
# 2. map_ts_list: geographic regions + time series data (with years)
# 3. ts_list: global entities + time series data
# 4. other_list: global entities + cross-sectional data

map_list <- lapply(all_tables[ vapply(all_tables, 
                                      function(x) x$is_region_first && x$no_year_col, logical(1)) ], `[[`, "data")
names(map_list) <- basename(vapply(all_tables[ vapply(all_tables, 
                                                      function(x) x$is_region_first && x$no_year_col, logical(1)) ], `[[`, "file", FUN.VALUE = character(1)))

map_ts_list <- lapply(all_tables[ vapply(all_tables, 
                                         function(x) x$is_region_first && !x$no_year_col, logical(1)) ], `[[`, "data")
names(map_ts_list) <- basename(vapply(all_tables[ vapply(all_tables, 
                                                         function(x) x$is_region_first && !x$no_year_col, logical(1)) ], `[[`, "file", FUN.VALUE = character(1)))

ts_list <- lapply(all_tables[ vapply(all_tables, 
                                     function(x) !x$is_region_first && !x$no_year_col, logical(1)) ], `[[`, "data")
names(ts_list) <- basename(vapply(all_tables[ vapply(all_tables, 
                                                     function(x) !x$is_region_first && !x$no_year_col, logical(1)) ], `[[`, "file", FUN.VALUE = character(1)))

other_list <- lapply(all_tables[ vapply(all_tables, 
                                        function(x) !x$is_region_first && x$no_year_col, logical(1)) ], `[[`, "data")
names(other_list) <- basename(vapply(all_tables[ vapply(all_tables, 
                                                        function(x) !x$is_region_first && x$no_year_col, logical(1)) ], `[[`, "file", FUN.VALUE = character(1)))

# ============================================================================
# STEP 5: REGION NAME HARMONIZATION
# ============================================================================

# Harmonize first column names for geographic tables
map_list <- lapply(map_list, function(x) {
  colnames(x)[1] <- "Region"
  x
})

# List of subtotal region names to remove
regions_to_remove <- c(
  "Europe Total", "Oceania Total", "Africa Total", "North America Total", 
  "Cental America Mainland Total", "Central America Antilles Total", 
  "South America Total", "America Total", "Asia Middle East Total", 
  "Asia South East Far East Total", "Asia Total"
)

# Function to remove subtotal rows from map_list tables
remove_total_regions <- function(map_list, regions_to_remove) {
  map_list %>%
    purrr::map(~ dplyr::filter(.x, !tolower(Region) %in% tolower(regions_to_remove)))
}
map_list <- remove_total_regions(map_list, regions_to_remove)
map_ts_list <- lapply(map_ts_list, function(x) {
  colnames(x)[1] <- "Region"
  x
})

# Load harmonized region names from reference file
final_regions <- readxl::read_excel("Rownames.xlsx", sheet = 1, 
                                    range = "A1:A253", col_types = "text")[[1]]

# Function to match region names using string similarity
match_region <- function(name, final_regions) {
  if (is.na(name)) return(NA_character_)
  distances <- stringdist(name, final_regions, method = "jw") # Jaro-Winkler distance
  best_match <- final_regions[which.min(distances)]
  # Return NA if match quality is poor (distance > 0.8)
  if (min(distances) > 0.8) NA_character_ else best_match
}

# Apply region name standardization to map_list tables
map_list <- purrr::map(map_list, ~ {
  .x %>%
    mutate(Region = sapply(Region, match_region, 
                           final_regions = final_regions)) %>%
    group_by(Region) %>%
    # Take first non-NA value for each region (consolidate duplicates)
    summarise(across(.cols = everything(), .fns = ~ first(na.omit(.x))), 
              .groups = "drop")
})

# Apply region name standardization to map_ts_list tables
map_ts_list <- purrr::map(map_ts_list, ~ {
  .x %>%
    mutate(Region = sapply(Region, match_region, 
                           final_regions = final_regions)) %>%
    group_by(Region) %>%
    # Take first non-NA value for each region (consolidate duplicates)
    summarise(across(.cols = everything(), .fns = ~ first(na.omit(.x))))
})

# ============================================================================
# STEP 5.1: REGION CLASSIFICATION SETUP
# ============================================================================

# Define macroregions and subregions based on common geographic classifications
# You may need to adjust these lists based on your specific data

macroregions <- c("Africa", "America North", "Central America Mainland", "Central America Antilles", "America South",
                  "America", "Asia Middle East", "Asia South East Far East","Asia", "Europe", "Oceania","World")

# Function to classify regions as macroregion or country-level
classify_region_type <- function(region_names) {
  sapply(region_names, function(region_name) {
    if (is.na(region_name)) return(NA_character_)
    if (tolower(region_name) %in% tolower(macroregions)) {
      return("Macroregion")
    } else {
      return("Country")
    }
  })
}


# ============================================================================
# STEP 6: TABLE DESCRIPTIONS SETUP
# ============================================================================

# Set working directory
setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\")

# Load table descriptions from overview files
overview_roman <- readxl::read_excel("data overview.xlsx", 
                                     sheet = "summary tables (Roman#)", 
                                     col_types = "text") %>%
  dplyr::filter(!is.na(`TABLE #`)) %>%
  dplyr::mutate(table_number = str_replace(`TABLE #`, "\\.0$", ""))

overview_arabic <- readxl::read_excel("data overview.xlsx", 
                                      sheet = "analytical tables (Arabic#)", 
                                      col_types = "text") %>%
  dplyr::filter(!is.na(`TABLE #`)) %>%
  dplyr::mutate(table_number = str_replace(`TABLE #`, "\\.0$", ""))

# Combine descriptions into single reference dataframe
table_description_df <- dplyr::bind_rows(overview_roman, overview_arabic) %>%
  dplyr::select(table_number, DESCRIPTION) %>%
  dplyr::distinct()

# Helper function to extract table number from filename
get_table_number <- function(filename) {
  stringr::str_match(basename(filename), "Table_([^.]+)\\.CSV")[,2]
}

# Function to attach descriptions to tables
add_descriptions <- function(table_list) {
  mapply(function(tbl, filename) {
    table_id <- get_table_number(filename)
    
    desc <- table_description_df %>%
      filter(table_number == table_id) %>%
      pull(DESCRIPTION) %>%
      first()
    
    if (is.null(desc)) {
      warning("No description found for: ", filename, " (id: ", table_id, ")")
      desc <- NA_character_
    }
    
    attr(tbl, "description") <- desc
    tbl
  }, table_list, names(table_list), SIMPLIFY = F)
}

# ============================================================================
# STEP 7: MERGE MAP_LIST TABLES (Cross-sectional geographic data)
# ============================================================================

# Function to merge cross-sectional geographic tables
merge_map_list <- function(map_data_list) {
  cat("Processing map_list tables...\n")
  
  if (length(map_data_list) == 0) {
    cat("No tables in map_list\n")
    return(NULL)
  }
  
  merged_map <- NULL
  variable_descriptions <- list()
  
  for (i in seq_along(map_data_list)) {
    table_name <- names(map_data_list)[i]
    current_table <- map_data_list[[i]]
    
    if (!is.null(current_table) && nrow(current_table) > 0) {
      desc <- attr(current_table, "description")
      if (is.null(desc)) desc <- paste0("Unknown description ", i)
      
      cat("Processing table:", table_name, "- Rows:", nrow(current_table), 
          "Cols:", ncol(current_table), "\n")
      
      # Add Year column (2022 for cross-sectional data)
      current_table <- current_table %>%
        mutate(Year = "2022") %>%
        relocate(Year, .after = 1)
      id_col <- colnames(current_table)[1]
      
      data_cols <- setdiff(colnames(current_table), c(id_col, "Year"))
      
      # Store description for each variable
      for (col in data_cols) {
        variable_descriptions[[col]] <- desc
      }
      
      # Merge tables by Region and Year
      if (is.null(merged_map)) {
        merged_map <- current_table
      } else {
        merged_map <- dplyr::full_join(merged_map, current_table, 
                                       by = c(id_col, "Year"))
      }
      
      cat("After merge - Rows:", nrow(merged_map), "Cols:", 
          ncol(merged_map), "\n")
    }
  }
  
  attr(merged_map, "variable_descriptions") <- variable_descriptions
  return(merged_map)
}

# Apply descriptions and merge map_list tables
map_list <- add_descriptions(map_list)

# Classify region types
map_list <- purrr::map(map_list, ~ dplyr::mutate(.x, Region_Type = classify_region_type(Region)))

# Merge map tables
merged_map_table <- merge_map_list(map_list)

# ============================================================================
# STEP 8: MERGE MAP_TS_LIST TABLES (Time series geographic data)
# ============================================================================

# Function to merge time series geographic tables
merge_map_ts_list <- function(map_ts_data_list) {
  cat("\nProcessing map_ts_list tables...\n")
  
  if (length(map_ts_data_list) == 0) {
    cat("No tables in map_ts_list\n")
    return(NULL)
  }
  
  long_tables <- list()
  
  # Convert each table from wide to long format
  for (i in seq_along(map_ts_data_list)) {
    table_name <- names(map_ts_data_list)[i]
    current_table <- map_ts_data_list[[i]]
    
    if (!is.null(current_table) && nrow(current_table) > 0) {
      cat("Processing table:", table_name, "- Rows:", nrow(current_table), 
          "Cols:", ncol(current_table), "\n")
      
      region_col <- colnames(current_table)[1]
      
      # Identify year columns (columns that look like years: 19XX or 20XX)
      year_cols <- grep("^(19|20)\\d{2}$", colnames(current_table), value = TRUE)
      
      if (length(year_cols) > 0) {
        # Get table description for variable naming
        table_id <- stringr::str_match(table_name, "Table_([^.]+)\\.CSV")[,2]
        desc <- table_description_df %>% 
          dplyr::filter(table_number == table_id) %>% 
          dplyr::pull(DESCRIPTION) %>% 
          dplyr::first()
        
        variable_name <- if (!is.null(desc) && !is.na(desc)) desc else table_name
        
        # Pivot from wide to long format
        long_table <- current_table %>%
          tidyr::pivot_longer(
            cols = all_of(year_cols),
            names_to = "Year",
            values_to = variable_name
          ) %>%
          dplyr::select(all_of(region_col), Year, all_of(variable_name))
        
        long_tables[[variable_name]] <- long_table
        
        cat("Converted to long format - Variable:", variable_name, "Years:", 
            paste(year_cols, collapse = ", "), "\n")
        
      } else {
        cat("No year columns found in table:", table_name, "\n")
      }
    }
  }
  
  # Merge all long-format tables by region and year
  if (length(long_tables) > 0) {
    merged_map_ts <- long_tables[[1]]
    region_col <- colnames(merged_map_ts)[1]
    
    if (length(long_tables) > 1) {
      for (i in 2:length(long_tables)) {
        current_long <- long_tables[[i]]
        variable_name <- names(long_tables)[i]
        
        merged_map_ts <- dplyr::full_join(
          merged_map_ts, 
          current_long, 
          by = c(region_col, "Year")
        )
        
        cat("Merged variable:", variable_name, "- Total rows:", 
            nrow(merged_map_ts), "Cols:", ncol(merged_map_ts), "\n")
      }
    }
    
    return(merged_map_ts)
  }
  
  return(NULL)
}

# Apply descriptions and merge map_ts_list tables
map_ts_list <- add_descriptions(map_ts_list)

map_ts_list <- purrr::map(map_ts_list, ~ dplyr::mutate(.x, Region_Type = classify_region_type(Region)))

merged_map_ts_table <- merge_map_ts_list(map_ts_list)

# ============================================================================
# STEP 9: COMBINE GEOGRAPHIC TABLES (map_list + map_ts_list)
# ============================================================================

# Merge cross-sectional and time series geographic data
final_geo_table <- NULL

if (!is.null(merged_map_table) && !is.null(merged_map_ts_table)) {
  # Ensure region column names match
  region_col_map <- colnames(merged_map_table)[1]
  region_col_ts <- colnames(merged_map_ts_table)[1]
  
  cat("Region columns - map_table:", region_col_map, "| map_ts_table:", region_col_ts, "\n")
  
  if (region_col_map != region_col_ts) {
    cat("Warning: Region column names don't match. Renaming", region_col_ts, "to", region_col_map, "\n")
    colnames(merged_map_ts_table)[1] <- region_col_map
  }
  
  # Merge by region and year
  final_geo_table <- dplyr::full_join(
    merged_map_table, 
    merged_map_ts_table, 
    by = c(region_col_map, "Year")
  )
  
  cat("Successfully merged both geographic tables!\n")
  
} else if (!is.null(merged_map_table)) {
  cat("Only cross-sectional geographic table available\n")
  final_geo_table <- merged_map_table
} else if (!is.null(merged_map_ts_table)) {
  cat("Only time series geographic table available\n") 
  final_geo_table <- merged_map_ts_table
} else {
  cat("No geographic tables to merge\n")
}

# Clean UTF-8 encoding in final table
if (!is.null(final_geo_table)) {
  final_geo_table[] <- lapply(final_geo_table, function(col) {
    if (is.character(col)) iconv(col, from = "", to = "UTF-8", sub = "")
    else col
  })
  
  # Export final geographic table
  readr::write_csv(final_geo_table, "final_geo_table.csv")
}

# ============================================================================
# STEP 10: PROCESS TS_LIST TABLES (Non-geographic time series data)
# ============================================================================

# Function to process non-geographic time series tables
process_ts_list <- function(ts_data_list) {
  cat("\nProcessing ts_list tables into long-format dataset...\n")
  
  if (length(ts_data_list) == 0) {
    cat("No tables in ts_list\n")
    return(NULL)
  }
  
  long_ts_tables <- list()
  
  # Convert each table to long format
  for (i in seq_along(ts_data_list)) {
    file_name <- names(ts_data_list)[i]
    current_table <- ts_data_list[[i]]
    
    if (!is.null(current_table) && nrow(current_table) > 0) {
      table_id <- str_match(file_name, "Table_([^.]+)\\.CSV")[,2]
      table_col <- gsub("\\.CSV$", "", basename(file_name))
      
      # Get description for variable naming
      desc <- table_description_df %>% 
        filter(table_number == table_id) %>% 
        pull(DESCRIPTION) %>% 
        first()
      
      variable_name <- if (!is.null(desc) && !is.na(desc)) desc else table_col
      
      cat("Processing", variable_name, "- Rows:", nrow(current_table), "Cols:", ncol(current_table), "\n")
      
      entity_col <- colnames(current_table)[1]
      year_cols <- grep("^(19|20)\\d{2}$", colnames(current_table), value = TRUE)
      
      if (length(year_cols) > 0) {
        # Pivot to long format
        long_table <- current_table %>%
          tidyr::pivot_longer(
            cols = all_of(year_cols),
            names_to = "Year",
            values_to = variable_name
          ) %>%
          dplyr::select(all_of(entity_col), Year, all_of(variable_name))
        
        # Standardize entity column name
        colnames(long_table)[1] <- "Entity"
        long_ts_tables[[variable_name]] <- long_table
        
        cat("Converted to long format - Variable:", variable_name, "Years:", paste(year_cols, collapse = ", "), "\n")
      } else {
        cat("No year columns found in table:", file_name, "\n")
      }
    }
  }
  
  # Merge all long-format tables by Entity and Year
  if (length(long_ts_tables) > 0) {
    final_dataset <- Reduce(function(x, y) dplyr::full_join(x, y, by = c("Entity", "Year")), long_ts_tables)
    return(final_dataset)
  }
  
  return(NULL)
}

# Process and merge non-geographic time series tables
ts_list <- add_descriptions(ts_list)
merged_ts_table <- process_ts_list(ts_list)

# Export non-geographic time series table
if (!is.null(merged_ts_table)) {
  readr::write_csv(merged_ts_table, "merged_ts_table.csv")
}

