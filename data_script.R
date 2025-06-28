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
pacman::p_load(tidyverse, readxl, stringdist, stringi)


# ============================================================================
# STEP 1: DATA CLEANING AND TRANSFORMATION
# ============================================================================

# Note: data transformation is performed based on the data notation employed by
# the Annuarium, which can be found in the "Observations" section.

# Set working directory
setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\ASE-main\\")

# Find all CSV files in the directory and subdirectories
data_files <- list.files(pattern = "\\.CSV$", full.names = T, recursive = T)

# Function to read CSV files and label them based on structure
read_and_check <- function(file) {
  tryCatch({
    # Read as character data to preserve all formatting
    raw_data <- read_delim(file, delim = ";", 
                           col_types = cols(.default = col_character()),
                           na = character(0),  # Don't convert to NA yet
                           locale = locale(encoding = "UTF-8"))
    
    # Get column names for later use
    col_names <- iconv(colnames(raw_data), to = "UTF-8", sub = "")
    
    # Categorization conditions:
    # - is_region_first: first column contains geographic regions (Countries/Continents)
    # - no_year_col: table has no Year columns (cross-sectional/snapshot data)
    is_region_first <- col_names[1] %in% c("Countries", "Continents")
    no_year_col <- !any(grepl("20", col_names, fixed = T))
    
    raw_data[[1]] <- iconv(raw_data[[1]], to = "UTF-8", sub = "")
    small_constant <- "0.007297"
    
    # Process all other columns with standardized replacements
    cleaned_data <- dplyr::mutate(raw_data, dplyr::across(-1, ~{
      . <- iconv(., to = "UTF-8", sub = "")                                   # Clean UTF-8 encoding
      . <- dplyr::case_when(. == "-" ~ "0", TRUE ~ .)                         # Replace "-" with 0
      . <- dplyr::case_when(. %in% c("...", "…") ~ NA_character_, TRUE ~ .)   # Replace "..." with NA
      . <- dplyr::case_when(. == ".." ~ small_constant, TRUE ~ .)             # Replace ".." with a small constant
      . <- gsub("\\.(?=\\d)", "", ., perl = T)                                # Remove dots as thousands separators
      . <- gsub(",", ".", .)                                                  # Replace decimal separator (,) with (.)
      as.numeric(.)                                                           # Convert to numeric
    }))
    
    return(list(file = file, data = cleaned_data, 
                is_region_first = is_region_first, no_year_col = no_year_col))
  }, error = function(e) {
    message("Skipping '", basename(file), "': ", e$message)
    return(list(file = file, data = NULL, is_region_first = F, no_year_col = F))
  })
}

# Read and label CSV files
all_tables <- lapply(data_files, read_and_check)


# ============================================================================
# STEP 2: AD-HOC TABLE ADJUSTMENTS
# ============================================================================

# Table 32

# Find index of list with file == "./Chapter04/Table_32.CSV"
idx <- which(map_lgl(all_tables, ~ .x$file == "./Chapter04/Table_32.CSV"))

# Check if found
if (length(idx) == 0) stop("Table_32.CSV not found in all_tables")

# Extract original list and tibble
orig_list <- all_tables[[idx]]
orig_data <- orig_list$data

# Verify tibble has enough columns
if (ncol(orig_data) != 13) stop("Table_32.CSV data does not have 13 columns")

# Split tibble
table_32_1 <- orig_data[, 1:7]
table_32_2 <- orig_data[, c(1, (ncol(orig_data)-5):ncol(orig_data))]

# Rename columns
colnames(table_32_1) <- c("Countries", "2017", "2018", "2019", "2020", "2021", "2022")
colnames(table_32_2) <- c("Countries", "2017", "2018", "2019", "2020", "2021", "2022")

# Create new lists
new_list_1 <- orig_list
new_list_1$file <- "./Chapter04/Table_32-1.CSV"
new_list_1$data <- table_32_1

new_list_2 <- orig_list
new_list_2$file <- "./Chapter04/Table_32-2.CSV"
new_list_2$data <- table_32_2

# Replace original list with two new lists
all_tables <- append(all_tables[-idx], list(new_list_1, new_list_2), after = idx - 1)

#print(map(all_tables, ~ .x$file))


# ============================================================================
# STEP 3: MISSING VALUES ASSESSMENT
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
# 1. map_list: cross-sectional spatial data (regions, no years)
# 2. map_ts_list: spatial time series data (regions, years)
# 3. ts_list: non-spatial time series data (no regions, years)
# 4. other_list: cross-sectional non-spatial data (no regions, no years)

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
# STEP 5: REGION NAMES HARMONIZATION
# ============================================================================

# Change first column names to "Region" for geographic tables
map_list <- lapply(map_list, function(x) {
  colnames(x)[1] <- "Region"
  x
})

map_ts_list <- lapply(map_ts_list, function(x) {
  colnames(x)[1] <- "Region"
  x
})

# Set working directory
setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\")

# Load harmonized region names
final_regions <- readxl::read_excel("rownames adjusted.xlsx", sheet = 1, range = "A1:A242", col_types = "text")[[1]]

# Function to filter out rows with "Total" in Region
filter_totals <- function(data_list) {
  map(data_list, ~ .x %>% filter(!grepl("Total", Region, ignore.case = TRUE)))
}

# Function to preprocess strings
preprocess <- function(x) {
  x <- stringi::stri_trans_general(tolower(gsub("[[:punct:]]", "", x)), "Latin-ASCII")
  if (is.na(x) || nchar(trimws(x)) == 0) return("")
  paste(sort(unlist(strsplit(trimws(x), "\\s+"))), collapse = " ")
}

# Function to replace Region with one-to-one matching
replace_regions <- function(data_list, final_regions) {
  map(data_list, function(tbl) {
    proc_regions <- map_chr(tbl$Region, preprocess)
    proc_final <- map_chr(final_regions, preprocess)
    matches <- rep(NA_character_, nrow(tbl))
    available <- seq_along(final_regions)
    for (i in seq_along(proc_regions)) {
      if (is.na(proc_regions[i]) || nchar(proc_regions[i]) == 0) {
        matches[i] <- tbl$Region[i]
        next
      }
      dists <- stringdist(proc_regions[i], proc_final[available], method = "jaccard", q = 2)
      if (length(available) == 0 || all(is.na(dists))) {
        matches[i] <- tbl$Region[i]
        next
      }
      if (min(dists, na.rm = TRUE) <= 0.5) {
        j <- available[which.min(dists)]
        matches[i] <- final_regions[j]
        available <- setdiff(available, j)
      } else {
        matches[i] <- tbl$Region[i]
      }
    }
    mutate(tbl, Region = matches)
  })
}

# Apply to map_list and map_ts_list
map_list <- filter_totals(map_list) %>% replace_regions(final_regions)
map_ts_list <- filter_totals(map_ts_list) %>% replace_regions(final_regions)

#print(map_list)
#print(map_ts_list)

# Macroregions defined by the Holy See geoscheme
macroregions <- c("Africa", "North America", "Central America (Mainland)", 
                  "Central America (Antilles)", "South America", "America", 
                  "Middle East Asia", "South East and Far East Asia", "Asia", 
                  "Europe", "Oceania", "World")

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

# Apply classify_region_type to map_list
map_list <- lapply(map_list, function(df) {
  df$`Region type` <- as.factor(classify_region_type(df$Region))
  return(df)
})

# Apply classify_region_type to map_ts_list
map_ts_list <- lapply(map_ts_list, function(df) {
  df$`Region type` <- as.factor(classify_region_type(df$Region))
  return(df)
})

#print.data.frame(map_list[[10]])
#print.data.frame(map_ts_list[[10]])


# ============================================================================
# STEP 6: LONG COLUMN NAMES SETUP
# ============================================================================

# Set working directory
setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\")

# Load the Excel file with data overview
data_overview_arabic <- readxl::read_excel("data overview.xlsx", sheet = 1, col_types = "text")

data_overview_roman <- readxl::read_excel("data overview.xlsx", sheet = 2, col_types = "text")

# Backup tibble names
map_list_names <- names(map_list)

# Function to extract column names from data overview based on table number
get_new_colnames <- function(table_num, sheet_data) {
  # Find the row where the table number matches
  row_idx <- which(sheet_data[[2]] == table_num)
  if (length(row_idx) == 0) return(NULL)
  # Extract column names from row D onwards (column 4 and beyond)
  new_cols <- unlist(sheet_data[row_idx, 4:ncol(sheet_data)], use.names = FALSE)
  #  # Remove any NA values and trim whitespace
  #  new_cols <- new_cols[!is.na(new_cols)]
  #  new_cols <- trimws(new_cols)
  return(new_cols)
}

# Update column names for each tibble in map_list
map_list <- lapply(names(map_list), function(file_name) {
  # Extract table number from file name (e.g., "Table_01.CSV" -> "01")
  table_num <- sub("Table_([^.]+).CSV", "\\1", file_name)
  
  # Select the right sheet
  if (grepl("^[0-9]", table_num)) sheet_data <- data_overview_arabic
  else sheet_data <- data_overview_roman
  
  # Get new column names
  new_cols <- get_new_colnames(table_num, sheet_data)
  
  if (!is.null(new_cols) && length(new_cols) > 1) {  # Ensure we have enough columns to update
    # Keep the first column as "Region", update the rest
    colnames(map_list[[file_name]])[2:(length(colnames(map_list[[file_name]]))-1)] <- new_cols
  }
  
  return(map_list[[file_name]])
})

# Restore tibble names
names(map_list) <- map_list_names


# ============================================================================
# STEP 7: MERGE MAP_LIST TABLES (cross-sectional spatial data)
# ============================================================================

# Function to merge same-named columns for a Region
merge_columns <- function(values, region, col_name) {
  non_na <- values[!is.na(values)]
  if (length(unique(non_na)) > 1) {
    warning(sprintf("Conflict in Region '%s', Column '%s': Multiple non-NA values %s. Using first.", 
                    region, col_name, paste(unique(non_na), collapse = ", ")))
    return(unique(non_na)[1])
  }
  if (length(non_na) > 0) return(non_na[1])
  return(NA)
}

# Merge all tibbles in map_list
merged_map_table <- bind_rows(map_list) %>%
  group_by(Region) %>%
  summarise(across(everything(), ~ merge_columns(.x, Region[1], cur_column()), .names = "{.col}"), .groups = "drop")

#last_dplyr_warnings()

#print(merged_map_table)


# ============================================================================
# STEP 8: MERGE MAP_TS_LIST TABLES (spatial time series data)
# ============================================================================

# Backup tibble names
map_ts_list_names <- names(map_ts_list)

# Update column names for each tibble in map_ts_list and prepare for merging
map_ts_list <- lapply(names(map_ts_list), function(file_name) {
  # Extract table number from file name (e.g., "Table_01.CSV" -> "01", "Table_32-1.CSV" -> "32-1")
  table_num <- sub("Table_([^.]+).CSV", "\\1", file_name)
  
  # Select the right sheet
  if (grepl("^[0-9]", table_num)) {
    sheet_data <- data_overview_arabic
  } else {
    sheet_data <- data_overview_roman
  }
  
  # Get new column names for years (excluding Region and Region type)
  new_cols <- get_new_colnames(table_num, sheet_data)
  
  # Update year column names if valid
  if (!is.null(new_cols) && length(new_cols) > 1) {  # Ensure we have enough columns to update
    # Keep the first column as "Region" and last as "Region type", update the rest
    colnames(map_ts_list[[file_name]])[2:(length(colnames(map_ts_list[[file_name]]))-1)] <- new_cols
  }
  
  # Get the new name for the Value column from the first column of sheet_data
  row_idx <- which(sheet_data[[2]] == table_num)
  if (length(row_idx) == 0) {
    warning(sprintf("No matching row found for table number '%s' in sheet_data. Keeping default 'Value' column name.", table_num))
    value_col_name <- "Value"
  } else {
    value_col_name <- sheet_data[row_idx, 1][[1]]  # Extract value from first column
    value_col_name <- trimws(value_col_name)  # Trim whitespace
    if (is.na(value_col_name) || nchar(value_col_name) == 0) {
      warning(sprintf("Value column name for table number '%s' is NA or empty. Using default 'Value'.", table_num))
      value_col_name <- "Value"
    }
  }
  
  # Pivot the tibble to long format
  tbl_long <- map_ts_list[[file_name]] %>%
    pivot_longer(
      cols = -c("Region", "Region type"),
      names_to = "Year",
      values_to = value_col_name,  # Use the new value column name
      values_drop_na = FALSE
    )
  
  return(tbl_long)
})

# Restore tibble names
names(map_ts_list) <- map_ts_list_names

# Function to merge time series tables in map_ts_list
merge_geo_ts_tables <- function(data_list) {
  # Combine all long-format tables
  merged_geo_ts_table <- bind_rows(data_list) %>%
    # Group by Region, Year, and Region type to handle potential conflicts
    group_by(Region, Year, `Region type`) %>%
    summarise(
      across(.cols = everything(), 
             ~ merge_columns(.x, Region[1], cur_column()), 
             .names = "{.col}"),
      .groups = "drop"
    )
  
  return(merged_geo_ts_table)
}

# Apply the merge function
merged_map_ts_table <- merge_geo_ts_tables(map_ts_list)

#last_dplyr_warnings()


# ============================================================================
# STEP 9: MERGE SPATIAL DATA (map_list + map_ts_list)
# ============================================================================

# Add "Year"<-"2022" column for merged_map_table
merged_map_table <- merged_map_table %>% 
  mutate(Year = "2022")

# Create a list with both tables
final_geo_table_list <- list(merged_map_table, merged_map_ts_table)

# Merge them with TS merging rules
final_geo_table <- merge_geo_ts_tables(final_geo_table_list)

# Export final geographic table
readr::write_csv(final_geo_table, "final_geo_table.csv")

#last_dplyr_warnings()


# ============================================================================
# STEP 10: PROCESS AND MERGE TABLES ABOUT ISPR FOR MEN (non-spatial time series data)
# ============================================================================

# Create table_50_list with tibbles containing "_50" in their names
table_50_list <- ts_list[grep("_50", names(ts_list))]

# Remove tibbles containing "_50" from ts_list
ts_list <- ts_list[!grepl("_50", names(ts_list))]

# Load the first column from the sixth sheet of "rownames adjusted.xlsx"
setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\")
new_row_names <- readxl::read_excel("rownames adjusted.xlsx", sheet = 6, 
                                    range = "A1:A11", col_types = "text")[[1]]

# Function to replace the first column of a tibble with new row names
replace_first_column <- function(tbl, new_names) {
  # Ensure the number of rows matches
  if (nrow(tbl) != length(new_names)) {
    warning(sprintf("Number of rows in tibble (%d) does not match number of new names (%d). Truncating/padding with NA.", 
                    nrow(tbl), length(new_names)))
    new_names <- new_names[seq_len(nrow(tbl))]
  }
  # Replace the first column
  tbl[[1]] <- new_names
  return(tbl)
}

# Apply the first column replacement to all tibbles in ts_list
ts_list <- lapply(ts_list, function(tbl) {
  replace_first_column(tbl, new_row_names)
})

# Backup tibble names
ts_list_names <- names(ts_list)

# Update column names and pivot ts_list to long format
ts_list <- lapply(names(ts_list), function(file_name) {
  # Extract table number from file name (e.g., "Table_01.CSV" -> "01")
  table_num <- sub("Table_([^.]+).CSV", "\\1", file_name)
  
  # Load the first sheet of data overview for value column name
  sheet_data <- data_overview_arabic
  
  # Get the new name for the Value column from the first column of sheet_data
  row_idx <- which(sheet_data[[2]] == table_num)
  if (length(row_idx) == 0) {
    warning(sprintf("No matching row found for table number '%s' in data_overview_arabic. Using default 'Value'.", table_num))
    value_col_name <- "Value"
  } else {
    value_col_name <- sheet_data[row_idx, 1][[1]]  # Extract value from first column
    value_col_name <- trimws(value_col_name)  # Trim whitespace
    if (is.na(value_col_name) || nchar(value_col_name) == 0) {
      warning(sprintf("Value column name for table number '%s' is NA or empty. Using default 'Value'.", table_num))
      value_col_name <- "Value"
    }
  }
  
  # Pivot the tibble to long format
  tbl_long <- ts_list[[file_name]] %>%
    pivot_longer(
      cols = -1,  # Keep the first column
      names_to = "Year",
      values_to = value_col_name,  # Use the new value column name
      values_drop_na = FALSE
    )
  
  return(tbl_long)
})

# Restore tibble names
names(ts_list) <- ts_list_names

# Function to merge time series tables in map_ts_list
merge_ts_tables <- function(data_list) {
  # Combine all long-format tables
  merged_ts_table <- bind_rows(data_list) %>%
    # Group by Region, Year, and Region type to handle potential conflicts
    group_by(`Categories of Institutes`, Year) %>%
    summarise(
      across(.cols = everything(), 
             ~ merge_columns(.x, Region[1], cur_column()), 
             .names = "{.col}"),
      .groups = "drop"
    )
  
  return(merged_ts_table)
}

# Apply the merge function
final_ispr_men_table <- merge_ts_tables(ts_list)


# ============================================================================
# STEP 11: PROCESS AND MERGE TABLES ABOUT ISPR FOR WOMEN (non-spatial time series data)
# ============================================================================

