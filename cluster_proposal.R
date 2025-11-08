# ---- Libraries and work setup ----

rm(list = ls())

# Load libraries
library(reshape2)
library(viridis)
library(ggplot2)
library(psych)
library(moments)
library(gridExtra)
library(scales)
library(tidyr)
library(dplyr)
library(stringr)
library(VIM)
library(fpc)
library(fmsb)
library(mclust)
library(clValid)
library(ggdendro)
library(plotly)
library(NbClust)
library(cluster)
library(factoextra)

# Set working directory
#path_data <- "C:/Users/schia/Documents/GitHub/Consulting_Catholic_Church"
path_data <- "C:/Users/soffi/Documents/Consulting_Catholic_Church"
setwd(path_data)


# ---- Data Loading and Initial Processing ----

# Read the dataset, preserving original column names
final_geo_table <- read.csv("final_geo_table.csv", sep = ";", check.names = FALSE)

# Define columns to keep
columns_to_keep <- c(
  "Region",
  "Year",
  "Region type",
  "Catholics per 100 inhabitants",
  "Catholics in thousands",
  "Area in km^2",
  "Catholics per pastoral centre",
  "Parishes with diocesan pastor",
  "Parishes with religious pastor",
  "Parishes without pastor administered by another priest",
  "Parishes without pastor entrusted to permanent deacons",
  "Parishes without pastor entrusted to non-priest religious men",
  "Parishes without pastor entrusted to religious women",
  "Parishes without pastor entrusted to laypeople",
  "Parishes entirely vacant",
  "Catholics per priest",
  "Yearly ordinations of diocesan priests as share of those incardinated on January 1",
  "Yearly deaths of diocesan priests as share of those incardinated on January 1",
  "Yearly defections of diocesan priests as share of those incardinated at January 1",
  "Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics",
  "Philosophy+theology candidates for diocesan and religious clergy per 100 priests",
  "Candidates for diocesan clergy in theology centres",
  "Candidates for religious clergy in theology centres",
  "Infant baptisms (people up to 7 years old)",
  "Adult baptisms (people over 7 years old)",
  "Baptisms",
  "Infant baptisms (people up to 7 years old) per 1000 Catholics",
  "Marriages between Catholics",
  "Mixed marriages",
  "Marriages",
  "Marriages per 1000 Catholics",
  "Confirmations per 1000 Catholics",
  "First Communions per 1000 Catholics",
  "Inhabitants in thousands"
)

# Subset to relevant columns
cluster_data <- final_geo_table %>% select(all_of(columns_to_keep))

# Apply manual corrections for specific data issues
cluster_data <- cluster_data %>%
  mutate(
    `Yearly deaths of diocesan priests as share of those incardinated on January 1` = if_else(
      Region == "French Guiana",
      `Yearly deaths of diocesan priests as share of those incardinated on January 1` / 100,
      `Yearly deaths of diocesan priests as share of those incardinated on January 1`
    ),
    `Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1` = if_else(
      Region == "Finland",
      `Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1` / 100,
      `Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1`
    ),
    `Confirmations per 1000 Catholics` = if_else(
      Region == "Sri Lanka",
      `Confirmations per 1000 Catholics` / 100,
      `Confirmations per 1000 Catholics`
    ),
    `Confirmations per 1000 Catholics` = if_else(
      Region == "Mali",
      8.29,
      `Confirmations per 1000 Catholics`
    )
  )

# ---- Missing values ----

country_data <- cluster_data %>%
  filter(Year == 2022, `Region type` == "Country") %>%
  select(-Year, -`Region type`)

summary(country_data)

missing_info_tidy <- country_data%>%
  mutate(Row_Index = row_number()) %>%
  pivot_longer(cols = -c(Row_Index, Region), names_to = "Variable", values_to = "Value") %>%
  filter(is.na(Value)) %>%
  select(Row_Index, Region, Variable) %>%
  arrange(Region, Variable)

print(missing_info_tidy)

# Check of the missing values per variable and per country + graphs

#Per variable

# Number of missing values per variable (column)
missing_per_variable <- colSums(is.na(country_data))
print("Missing values per variable:")
print(missing_per_variable)

# Bar plot of missing values per variable

missing_df <- data.frame(
  Variable = names(missing_per_variable),
  Missing_Count = as.numeric(missing_per_variable)
)


p1 <- ggplot(missing_df, aes(x = reorder(Variable, Missing_Count), y = Missing_Count, fill = Missing_Count)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "D") +  # (options: A, B, C, D, E)
  coord_flip() +
  labs(title = "Missing Values per Variable",
       x = "Variable",
       y = "Number of Missing Values") +
  theme_minimal() +
  geom_text(aes(label = Missing_Count), hjust = -0.2, size = 3.5) +
  theme(legend.position = "none")  # Optional: hide legend since color represents same as y-axis

print(p1)

#Per country

# First, add a temporary column for row-wise missing count
country_data$missing_count <- rowSums(is.na(country_data))

# Then, aggregate by Region
missing_per_region <- aggregate(missing_count ~ Region, data = country_data, FUN = sum)

# Remove the temporary column
country_data$missing_count <- NULL

print("Missing values per Region:")
print(missing_per_region)

# Heatmap

# Get the Country column (which is the "Region" column for countries)
country_names <- country_data$Region

# Create a binary matrix for missing values
# Exclude Region, Region type columns
vars_to_check <- !names(country_data) %in% c("Region", "Region type")
missing_matrix <- is.na(country_data[, vars_to_check])
missing_matrix <- as.data.frame(missing_matrix)

# Add Country column
missing_matrix$Country <- country_names

# Only show countries with at least one missing value
rows_with_missing <- rowSums(missing_matrix[, names(missing_matrix) != "Country"]) > 0
countries_with_missing <- missing_matrix[rows_with_missing, ]

if(nrow(countries_with_missing) > 0) {
  # Reshape for heatmap
  missing_long <- melt(countries_with_missing, id.vars = "Country", 
                       variable.name = "Variable", value.name = "Missing")
  
  p2 <- ggplot(missing_long, aes(x = Variable, y = Country, fill = Missing)) +
    geom_tile(color = "white") +
    scale_fill_viridis(discrete = TRUE, option = "D", 
                       labels = c("Present", "Missing"), 
                       direction = -1) +
    labs(title = "Missing Data Pattern by Country",
         x = "Variable",
         y = "Country",
         fill = "Status") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 8))
  
  print(p2)
} else {
  print("No countries with missing values found")
}

# Data columns only
vars_to_check <- setdiff(names(country_data), "Region")

# Row-wise missing count
country_data$Missing_Count <- rowSums(is.na(country_data[, vars_to_check]))

# Summary (only countries with any missing)
missing_summary <- subset(country_data, Missing_Count > 0, select = c("Region","Missing_Count"))
names(missing_summary)[1] <- "Country"
missing_summary$Missing_Percentage <- round(missing_summary$Missing_Count / length(vars_to_check) * 100, 1)

# Bar "heatmap" (p3)
if (nrow(missing_summary) > 0) {
  library(ggplot2); library(viridis)
  p3 <- ggplot(missing_summary,
               aes(x = reorder(Country, Missing_Percentage),
                   y = Missing_Percentage,
                   fill = Missing_Percentage)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis(option = "D") +
    coord_flip() +
    labs(title = "Percentage of Missing Data per Country",
         x = "Country", y = "% of Variables Missing", fill = "% Missing") +
    theme_minimal() +
    geom_text(aes(label = paste0(Missing_Percentage, "%")), hjust = -0.2, size = 3) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.7) +
    annotate("text", x = nrow(missing_summary), y = 50,
             label = "50% threshold", vjust = -0.5, color = "red") +
    expand_limits(y = max(50, max(missing_summary$Missing_Percentage)) * 1.05)
  print(p3)
} else {
  message("No countries with missing values found")
}

#remove temp column
country_data$Missing_Count <- NULL


# Filter to 2022 country-level data and remove unwanted countries
countries_to_remove <- c("Dem. Peoples Rep. Of Korea", "China (Mainland)")

country_data <- country_data %>%
  filter(!Region %in% countries_to_remove)

# Print summary statistics
summary(country_data)

# ---- Variable Mutations and Feature Engineering + Imputation ----

# Perform mutations and select only the final desired columns
analysis_data <- country_data %>%
  mutate(
    # Simple divisions - these are fine
    `Catholics per km^2` = (`Catholics in thousands` * 1000) / `Area in km^2`,
    
    # Share variables with 0/0 protection
    `Share of diocesan pastors` = ifelse(
      (`Parishes with diocesan pastor` + `Parishes with religious pastor`) == 0,
      0,
      `Parishes with diocesan pastor` / (`Parishes with diocesan pastor` + `Parishes with religious pastor`)
    ),
    
    # Per-Catholic rates - ADD PROTECTION for 0 Catholics
    `Non-vacant parishes administered by non-pastor priests per Catholic` = ifelse(
      `Catholics in thousands` == 0,
      0,
      `Parishes without pastor administered by another priest` / (`Catholics in thousands` * 1000)
    ),
    
    `Share of non-vacant parishes entrusted to religious women or laypeople` = {
      numerator <- `Parishes without pastor entrusted to religious women` + `Parishes without pastor entrusted to laypeople`
      denominator <- `Parishes with diocesan pastor` + `Parishes with religious pastor` + 
        `Parishes without pastor administered by another priest` +
        `Parishes without pastor entrusted to permanent deacons` + 
        `Parishes without pastor entrusted to non-priest religious men` +
        `Parishes without pastor entrusted to religious women` + 
        `Parishes without pastor entrusted to laypeople`
      ifelse(denominator == 0, 0, numerator / denominator)
    },
    
    `Parishes entirely vacant per Catholic` = ifelse(
      `Catholics in thousands` == 0,
      0,
      `Parishes entirely vacant` / (`Catholics in thousands` * 1000)
    ),
    
    `Candidates for diocesan clergy in theology centres per Catholic` = ifelse(
      `Catholics in thousands` == 0,
      0,
      `Candidates for diocesan clergy in theology centres` / (`Catholics in thousands` * 1000)
    ),
    
    `Candidates for religious clergy in theology centres per Catholic` = ifelse(
      `Catholics in thousands` == 0,
      0,
      `Candidates for religious clergy in theology centres` / (`Catholics in thousands` * 1000)
    ),
    
    `Infant baptisms (people up to 7 years old) per Catholic` = ifelse(
      `Catholics in thousands` == 0,
      0,
      `Infant baptisms (people up to 7 years old)` / (`Catholics in thousands` * 1000)
    ),
    
    `Adult baptisms (people over 7 years old) per Catholic` = ifelse(
      `Catholics in thousands` == 0,
      0,
      `Adult baptisms (people over 7 years old)` / (`Catholics in thousands` * 1000)
    ),
    
    `Baptisms per inhabitant` = ifelse(
      `Inhabitants in thousands` == 0,
      0,
      `Baptisms` / (`Inhabitants in thousands` * 1000)
    ),
    
    `Marriages between Catholics per Catholic` = ifelse(
      `Catholics in thousands` == 0,
      0,
      `Marriages between Catholics` / (`Catholics in thousands` * 1000)
    ),
    
    `Mixed marriages per inhabitant` = ifelse(
      `Inhabitants in thousands` == 0,
      0,
      `Mixed marriages` / (`Inhabitants in thousands` * 1000)
    ),
    
    `Share of mixed marriages` = ifelse(
      (`Mixed marriages` + `Marriages between Catholics`) == 0,
      0,
      `Mixed marriages` / (`Mixed marriages` + `Marriages between Catholics`)
    ),
    
    # Simple conversions - these are fine
    `Confirmations per Catholic` = `Confirmations per 1000 Catholics` / 1000,
    `First Communions per Catholic` = `First Communions per 1000 Catholics` / 1000
  ) %>%
  select(
    Region,
    `Catholics per 100 inhabitants`,
    `Catholics per km^2`,
    `Catholics per pastoral centre`,
    `Share of diocesan pastors`,
    `Non-vacant parishes administered by non-pastor priests per Catholic`,
    `Share of non-vacant parishes entrusted to religious women or laypeople`,
    `Parishes entirely vacant per Catholic`,
    `Catholics per priest`,
    `Yearly ordinations of diocesan priests as share of those incardinated on January 1`,
    `Yearly deaths of diocesan priests as share of those incardinated on January 1`,
    `Yearly defections of diocesan priests as share of those incardinated at January 1`,
    `Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants`,
    `Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics`,
    `Philosophy+theology candidates for diocesan and religious clergy per 100 priests`,
    `Candidates for diocesan clergy in theology centres per Catholic`,
    `Candidates for religious clergy in theology centres per Catholic`,
    `Infant baptisms (people up to 7 years old) per Catholic`,
    `Adult baptisms (people over 7 years old) per Catholic`,
    `Baptisms per inhabitant`,
    `Marriages between Catholics per Catholic`,
    `Mixed marriages per inhabitant`,
    `Share of mixed marriages`,
    `Confirmations per Catholic`,
    `First Communions per Catholic`
  )

# Print column names and summary for verification
names(analysis_data)
summary(analysis_data)

#Check for "new" missing values and decide what to do 

missingness_per_country <- analysis_data %>%
  mutate(
    # Count NAs per row (country)
    n_missing = rowSums(is.na(select(., -Region))),
    # Count total variables (excluding Region)
    n_total_vars = ncol(.) - 1,
    # Calculate percentage missing
    pct_missing = (n_missing / n_total_vars) * 100
  ) %>%
  select(Region, n_missing, n_total_vars, pct_missing) %>%
  arrange(desc(n_missing))

# View countries with ANY missing data
missingness_per_country %>%
  filter(n_missing > 0)

# Summary statistics
cat("Total countries:", nrow(analysis_data), "\n")
cat("Countries with ANY missing:", sum(missingness_per_country$n_missing > 0), "\n")
cat("Countries with >20% missing:", sum(missingness_per_country$pct_missing > 20), "\n")
cat("Countries with >10% missing:", sum(missingness_per_country$pct_missing > 10), "\n")
cat("Countries with >5% missing:", sum(missingness_per_country$pct_missing > 5), "\n")

high_missingness_countries <- missingness_per_country %>%
  filter(pct_missing > 20)

print(high_missingness_countries)

if (nrow(high_missingness_countries) > 0) {
  
  for (country in high_missingness_countries$Region) {
    cat("Country:", country, "\n")
    
    # Get all variables that are NA for this country
    missing_vars <- analysis_data %>%
      filter(Region == country) %>%
      select(-Region) %>%
      select(where(~is.na(.))) %>%
      names()
    
    cat("Missing variables (", length(missing_vars), "):\n", sep = "")
    for (var in missing_vars) {
      cat("  -", var, "\n")
    }
    cat("\n")
  }
}

moderate_missingness_countries <- missingness_per_country %>%
  filter(pct_missing > 5 & pct_missing <= 20)

print(moderate_missingness_countries)

if (nrow(moderate_missingness_countries) > 0) {
  
  for (country in moderate_missingness_countries$Region) {
    cat("Country:", country, "\n")
    
    missing_vars <- analysis_data %>%
      filter(Region == country) %>%
      select(-Region) %>%
      select(where(~is.na(.))) %>%
      names()
    
    cat("Missing variables (", length(missing_vars), "):\n", sep = "")
    for (var in missing_vars) {
      cat("  -", var, "\n")
    }
    cat("\n")
  }
}

#Check exactly from where these NaN originate, to see if they are true O


# # Get all countries with any missing values
# countries_with_missing <- missingness_per_country %>%
#   filter(n_missing > 0) %>%
#   pull(Region)
# 
# diagnose_variable <- function(var_name, data_full) {
#   
#   # Get countries where this variable is NA
#   na_countries <- data_full %>%
#     filter(is.na(.data[[var_name]])) %>%
#     pull(Region)
#   
#   if (length(na_countries) == 0) {
#     return(NULL)
#   }
#   
#   # Define which raw columns to check based on variable name
#   # This maps derived variables back to their input columns
#   
#   raw_cols <- list()
#   
#   if (var_name == "Share of diocesan pastors") {
#     raw_cols <- c("Parishes with diocesan pastor", "Parishes with religious pastor")
#   } else if (var_name == "Share of non-vacant parishes entrusted to religious women or laypeople") {
#     raw_cols <- c("Parishes with diocesan pastor", "Parishes with religious pastor",
#                   "Parishes without pastor administered by another priest",
#                   "Parishes without pastor entrusted to permanent deacons",
#                   "Parishes without pastor entrusted to non-priest religious men",
#                   "Parishes without pastor entrusted to religious women",
#                   "Parishes without pastor entrusted to laypeople")
#   } else if (grepl("per Catholic", var_name)) {
#     raw_cols <- c("Catholics in thousands")
#     # Add numerator based on variable name
#     if (grepl("Infant baptisms", var_name)) {
#       raw_cols <- c(raw_cols, "Infant baptisms (people up to 7 years old)")
#     } else if (grepl("Adult baptisms", var_name)) {
#       raw_cols <- c(raw_cols, "Adult baptisms (people over 7 years old)")
#     } else if (grepl("Marriages between Catholics", var_name)) {
#       raw_cols <- c(raw_cols, "Marriages between Catholics")
#     } else if (grepl("Candidates for diocesan clergy in theology", var_name)) {
#       raw_cols <- c(raw_cols, "Candidates for diocesan clergy in theology centres")
#     } else if (grepl("Candidates for religious clergy in theology", var_name)) {
#       raw_cols <- c(raw_cols, "Candidates for religious clergy in theology centres")
#     } else if (grepl("Non-vacant parishes administered by non-pastor", var_name)) {
#       raw_cols <- c(raw_cols, "Parishes without pastor administered by another priest")
#     } else if (grepl("Parishes entirely vacant", var_name)) {
#       raw_cols <- c(raw_cols, "Parishes entirely vacant")
#     }
#   } else if (grepl("per inhabitant", var_name)) {
#     raw_cols <- c("Inhabitants in thousands")
#     if (grepl("Baptisms per inhabitant", var_name)) {
#       raw_cols <- c(raw_cols, "Baptisms")
#     } else if (grepl("Mixed marriages", var_name)) {
#       raw_cols <- c(raw_cols, "Mixed marriages")
#     }
#   } else if (var_name == "Share of mixed marriages") {
#     raw_cols <- c("Marriages between Catholics", "Mixed marriages", 
#                   "Catholics in thousands", "Inhabitants in thousands")
#   }
#   
#   # Get raw data for these countries
#   raw_data <- country_data %>%
#     filter(Region %in% na_countries) %>%
#     select(Region, any_of(raw_cols))
#   
#   return(raw_data)
# }
# 
# 
# vars_with_missing <- analysis_data %>%
#   select(-Region) %>%
#   summarise(across(everything(), ~sum(is.na(.)))) %>%
#   pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
#   filter(n_missing > 0) %>%
#   arrange(desc(n_missing))
# 
# cat("Variables with missing values:\n")
# print(vars_with_missing)
# 
# for (i in 1:nrow(vars_with_missing)) {
#   var <- vars_with_missing$variable[i]
#   n_miss <- vars_with_missing$n_missing[i]
#   
#   cat("\n")
#   cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
#   cat("VARIABLE:", var, "\n")
#   cat("Number of countries with NA:", n_miss, "\n")
#   cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")
#   
#   # Get the raw data that drives this variable
#   raw_diag <- diagnose_variable(var, analysis_data)
#   
#   if (!is.null(raw_diag)) {
#     print(raw_diag)
#     
#     # Analyze the pattern
#     cat("\nPATTERN ANALYSIS:\n")
#     
#     # Check if all values are 0
#     numeric_cols <- raw_diag %>% select(-Region) %>% select(where(is.numeric))
#     
#     if (ncol(numeric_cols) > 0) {
#       all_zeros <- raw_diag %>%
#         select(-Region) %>%
#         select(where(is.numeric)) %>%
#         summarise(across(everything(), ~all(. == 0, na.rm = TRUE)))
#       
#       for (col in names(all_zeros)) {
#         if (all_zeros[[col]]) {
#           cat("  - ALL countries have", col, "= 0 (STRUCTURAL ZERO)\n")
#         } else {
#           # Check individual values
#           has_zero <- raw_diag %>% filter(.data[[col]] == 0) %>% pull(Region)
#           has_nonzero <- raw_diag %>% filter(.data[[col]] > 0) %>% pull(Region)
#           has_na <- raw_diag %>% filter(is.na(.data[[col]])) %>% pull(Region)
#           
#           if (length(has_zero) > 0) {
#             cat("  -", col, "= 0 for:", paste(has_zero, collapse = ", "), "\n")
#           }
#           if (length(has_nonzero) > 0) {
#             cat("  -", col, "> 0 for:", paste(has_nonzero, collapse = ", "), "\n")
#           }
#           if (length(has_na) > 0) {
#             cat("  -", col, "= NA (TRULY MISSING) for:", paste(has_na, collapse = ", "), "\n")
#           }
#         }
#       }
#       
#       # Determine cause of NaN
#       cat("\nCAUSE OF NaN:\n")
#       for (country in raw_diag$Region) {
#         country_data_slice <- raw_diag %>% filter(Region == country)
#         numeric_values <- country_data_slice %>% select(-Region) %>% select(where(is.numeric))
#         
#         if (all(numeric_values == 0, na.rm = TRUE)) {
#           cat("  -", country, ": Division by zero (0/0) - STRUCTURAL ZERO\n")
#         } else if (any(is.na(numeric_values))) {
#           cat("  -", country, ": Contains TRUE missing data (NA in raw input)\n")
#         } else {
#           cat("  -", country, ": Check formula logic\n")
#         }
#       }
#     }
#   } else {
#     cat("(Could not trace back to raw data - check variable mapping)\n")
#   }
# }
# 
# # For each country, categorize their missing values
# for (country in countries_with_missing) {
#   cat("\nCountry:", country, "\n")
#   cat("-" %>% rep(40) %>% paste(collapse = ""), "\n")
#   
#   # Get raw data for this country
#   raw_country <- country_data %>%
#     filter(Region == country) %>%
#     select(
#       `Catholics in thousands`,
#       `Inhabitants in thousands`,
#       `Parishes with diocesan pastor`,
#       `Parishes with religious pastor`,
#       `Parishes without pastor administered by another priest`,
#       `Parishes entirely vacant`,
#       `Candidates for diocesan clergy in theology centres`,
#       `Candidates for religious clergy in theology centres`,
#       `Infant baptisms (people up to 7 years old)`,
#       `Adult baptisms (people over 7 years old)`,
#       `Baptisms`,
#       `Marriages between Catholics`,
#       `Mixed marriages`
#     )
#   
#   # Key indicators
#   catholics <- raw_country$`Catholics in thousands`[1]
#   total_parishes <- sum(
#     raw_country$`Parishes with diocesan pastor`[1],
#     raw_country$`Parishes with religious pastor`[1],
#     raw_country$`Parishes without pastor administered by another priest`[1],
#     raw_country$`Parishes entirely vacant`[1],
#     na.rm = TRUE
#   )
#   
#   cat("Catholics (thousands):", catholics, "\n")
#   cat("Total parishes:", total_parishes, "\n")
#   cat("Baptisms:", raw_country$Baptisms[1], "\n")
#   cat("Marriages (Catholic):", raw_country$`Marriages between Catholics`[1], "\n")
#   cat("Mixed marriages:", raw_country$`Mixed marriages`[1], "\n\n")
#   
#   # Get missing variables for this country
#   missing_vars <- analysis_data %>%
#     filter(Region == country) %>%
#     select(-Region) %>%
#     select(where(~is.na(.))) %>%
#     names()
#   
#   cat("Missing variables (", length(missing_vars), "):\n", sep = "")
#   
#   # Categorize each
#   for (var in missing_vars) {
#     if (catholics == 0 && grepl("per Catholic", var)) {
#       cat("  - ", var, " → STRUCTURAL ZERO (0 Catholics)\n", sep = "")
#     } else if (total_parishes == 0 && grepl("parish|pastor", var, ignore.case = TRUE)) {
#       cat("  - ", var, " → STRUCTURAL ZERO (0 parishes)\n", sep = "")
#     } else if (raw_country$`Marriages between Catholics`[1] == 0 && 
#                raw_country$`Mixed marriages`[1] == 0 && 
#                grepl("marriage", var, ignore.case = TRUE)) {
#       cat("  - ", var, " → STRUCTURAL ZERO (0 marriages)\n", sep = "")
#     } else {
#       cat("  - ", var, " → CHECK: May be TRUE missing or other cause\n", sep = "")
#     }
#   }
# }

# ---- Transformation Functions ----

# Small constant used in place of ".."
small_constant <- 0.007297

# Log transformation with adjusted epsilon
log_transform <- function(x) {
  # Exclude zeros and specific value 0.007297 for min calculation
  non_zero <- x[x > 0 & x != small_constant]
  
  # Fallback epsilon if no valid non-zero values
  if (length(non_zero) == 0) {
    epsilon <- 1e-6
  } else {
    min_val <- min(non_zero, na.rm = TRUE)
    epsilon <- 0.5 * min_val
  }
  
  # Apply adjusted log
  log(x + epsilon)
}

# Logit transformation with adjusted epsilon
logit_transform <- function(p) {
  # Exclude zeros and specific value 0.007297 for min calculation
  non_zero <- p[p > 0 & p != small_constant]
  
  # Fallback epsilon if no valid non-zero values
  if (length(non_zero) == 0) {
    epsilon <- 1e-6
  } else {
    min_val <- min(non_zero, na.rm = TRUE)
    epsilon <- 0.5 * min_val
  }
  
  # Adjust p to avoid log(0) or division by zero
  p_adjusted <- pmin(pmax(p, epsilon), 1 - epsilon)
  
  # Apply adjusted logit
  log(p_adjusted / (1 - p_adjusted))
}


# ---- Standardization ----

# Function to apply transformations and renames
standardize_data <- function(data) {
  data_std <- data %>%
    mutate(
      `Share of Catholics` = logit_transform(`Catholics per 100 inhabitants` / 100),
      `Catholics per km^2` = log_transform(`Catholics per km^2`),
      `Catholics per pastoral centre` = log_transform(`Catholics per pastoral centre`),
      `Share of diocesan pastors` = logit_transform(`Share of diocesan pastors`),
      `Non-vacant parishes administered by non-pastor priests per Catholic` = log_transform(`Non-vacant parishes administered by non-pastor priests per Catholic`),
      `Share of non-vacant parishes entrusted to religious women or laypeople` = logit_transform(`Share of non-vacant parishes entrusted to religious women or laypeople`),
      `Parishes entirely vacant per Catholic` = log_transform(`Parishes entirely vacant per Catholic`),
      `Catholics per priest` = log_transform(`Catholics per priest`),
      `Yearly ordinations of diocesan priests as share of those incardinated on January 1` = logit_transform(`Yearly ordinations of diocesan priests as share of those incardinated on January 1` / 100),
      `Yearly deaths of diocesan priests as share of those incardinated on January 1` = logit_transform(`Yearly deaths of diocesan priests as share of those incardinated on January 1` / 100),
      `Yearly defections of diocesan priests as share of those incardinated at January 1` = logit_transform(`Yearly defections of diocesan priests as share of those incardinated at January 1` / 100),
      `Vocation rate - philosophy+theology candidates for diocesan and religious clergy per inhabitant` = logit_transform(`Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants` / 100000),
      `Vocation rate - philosophy+theology candidates for diocesan and religious clergy per Catholic` = logit_transform(`Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics` / 100000),
      `Philosophy+theology candidates for diocesan and religious clergy per priest` = log_transform(`Philosophy+theology candidates for diocesan and religious clergy per 100 priests` * 100),
      `Candidates for diocesan clergy in theology centres per Catholic` = log_transform(`Candidates for diocesan clergy in theology centres per Catholic`),
      `Candidates for religious clergy in theology centres per Catholic` = log_transform(`Candidates for religious clergy in theology centres per Catholic`),
      `Infant baptisms (people up to 7 years old) per Catholic` = log_transform(`Infant baptisms (people up to 7 years old) per Catholic`),
      `Adult baptisms (people over 7 years old) per Catholic` = log_transform(`Adult baptisms (people over 7 years old) per Catholic`),
      `Baptisms per inhabitant` = logit_transform(`Baptisms per inhabitant`),
      `Marriages between Catholics per Catholic` = log_transform(`Marriages between Catholics per Catholic`),
      `Mixed marriages per inhabitant` = log_transform(`Mixed marriages per inhabitant`),
      `Share of mixed marriages` = logit_transform(`Share of mixed marriages`),
      `Confirmations per Catholic` = log_transform(`Confirmations per Catholic`),
      `First Communions per Catholic` = log_transform(`First Communions per Catholic`)
    ) %>%
    select(
      Region,
      `Share of Catholics`,
      `Catholics per km^2`,
      `Catholics per pastoral centre`,
      `Share of diocesan pastors`,
      `Non-vacant parishes administered by non-pastor priests per Catholic`,
      `Share of non-vacant parishes entrusted to religious women or laypeople`,
      `Parishes entirely vacant per Catholic`,
      `Catholics per priest`,
      `Yearly ordinations of diocesan priests as share of those incardinated on January 1`,
      `Yearly deaths of diocesan priests as share of those incardinated on January 1`,
      `Yearly defections of diocesan priests as share of those incardinated at January 1`,
      `Vocation rate - philosophy+theology candidates for diocesan and religious clergy per inhabitant`,
      `Vocation rate - philosophy+theology candidates for diocesan and religious clergy per Catholic`,
      `Philosophy+theology candidates for diocesan and religious clergy per priest`,
      `Candidates for diocesan clergy in theology centres per Catholic`,
      `Candidates for religious clergy in theology centres per Catholic`,
      `Infant baptisms (people up to 7 years old) per Catholic`,
      `Adult baptisms (people over 7 years old) per Catholic`,
      `Baptisms per inhabitant`,
      `Marriages between Catholics per Catholic`,
      `Mixed marriages per inhabitant`,
      `Share of mixed marriages`,
      `Confirmations per Catholic`,
      `First Communions per Catholic`
    )
  
  return(data_std)
}

# Design 1 variables (transformed names after standardize_data)
population_territory_transformed <- c(
  "Share of Catholics",
  "Catholics per km^2",
  "Catholics per pastoral centre",
  "Share of diocesan pastors",
  "Non-vacant parishes administered by non-pastor priests per Catholic",
  "Share of non-vacant parishes entrusted to religious women or laypeople",
  "Parishes entirely vacant per Catholic",
  "Catholics per priest"
)

# Design 2 variables (transformed names after standardize_data)
clergy_sacraments_transformed <- c(
  "Yearly ordinations of diocesan priests as share of those incardinated on January 1",
  "Yearly deaths of diocesan priests as share of those incardinated on January 1",
  "Yearly defections of diocesan priests as share of those incardinated at January 1",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per inhabitant",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per Catholic",
  "Philosophy+theology candidates for diocesan and religious clergy per priest",
  "Candidates for diocesan clergy in theology centres per Catholic",
  "Candidates for religious clergy in theology centres per Catholic",
  "Infant baptisms (people up to 7 years old) per Catholic",
  "Adult baptisms (people over 7 years old) per Catholic",
  "Baptisms per inhabitant",
  "Marriages between Catholics per Catholic",
  "Mixed marriages per inhabitant",
  "Share of mixed marriages",
  "Confirmations per Catholic",
  "First Communions per Catholic"
)

# Apply standardization
cluster_data_2022_std <- standardize_data(analysis_data)

# Verify transformations with summary (numeric columns only)
summary(cluster_data_2022_std %>% select(-Region))

# Use all countries for Design 1 
design1_data <- cluster_data_2022_std %>%
  select(Region, all_of(population_territory_transformed))

# Apply z-score standardization
design1_final <- design1_data %>%
  mutate(across(-Region, ~ scale(.)[,1]))

#For Design 2 
# Step 1: Filter analysis_data to exclude 0-Catholic countries BEFORE transformation
design2_raw <- analysis_data %>%
  filter(`Catholics per 100 inhabitants` > 0)

# Step 2: Transform the filtered data
design2_transformed <- standardize_data(design2_raw)

# Step 3: Select Design 2 variables
design2_data <- design2_transformed %>%
  select(Region, all_of(clergy_sacraments_transformed))

# Step 4: Apply z-score standardization
design2_final <- design2_data %>%
  mutate(across(-Region, ~ scale(.)[,1]))


# ---- Transformation diagnostics ----

# Create diagnostics for BOTH designs separately since they have different 
# country sets and transformations

# DESIGN 1 DIAGNOSTICS

# Define sigmoid function for logit inverse
sigmoid <- function(y) {
  1 / (1 + exp(-y))
}

# Get the transformed data BEFORE z-score (design1_data has log/logit transforms but not z-scores yet)
design1_transformed_only <- cluster_data_2022_std %>%
  select(Region, all_of(population_territory_transformed))

# Create back-transformed dataset for Design 1
design1_back_transformed <- design1_transformed_only %>%
  mutate(
    # Logit inverses
    `Catholics per 100 inhabitants_back` = sigmoid(`Share of Catholics`) * 100,
    `Share of diocesan pastors_back` = sigmoid(`Share of diocesan pastors`),
    `Share of non-vacant parishes entrusted to religious women or laypeople_back` = 
      sigmoid(`Share of non-vacant parishes entrusted to religious women or laypeople`),
    
    # Log inverses
    `Catholics per km^2_back` = exp(`Catholics per km^2`),
    `Catholics per pastoral centre_back` = exp(`Catholics per pastoral centre`),
    `Non-vacant parishes administered by non-pastor priests per Catholic_back` = 
      exp(`Non-vacant parishes administered by non-pastor priests per Catholic`),
    `Parishes entirely vacant per Catholic_back` = exp(`Parishes entirely vacant per Catholic`),
    `Catholics per priest_back` = exp(`Catholics per priest`)
  ) %>%
  select(Region, ends_with("_back"))

# Join with original data
design1_plot_data <- analysis_data %>%
  select(Region, 
         `Catholics per 100 inhabitants`,
         `Catholics per km^2`,
         `Catholics per pastoral centre`,
         `Share of diocesan pastors`,
         `Non-vacant parishes administered by non-pastor priests per Catholic`,
         `Share of non-vacant parishes entrusted to religious women or laypeople`,
         `Parishes entirely vacant per Catholic`,
         `Catholics per priest`) %>%
  inner_join(design1_back_transformed, by = "Region")

# Variable names for Design 1
design1_var_names <- c(
  "Catholics per 100 inhabitants",
  "Catholics per km^2",
  "Catholics per pastoral centre",
  "Share of diocesan pastors",
  "Non-vacant parishes administered by non-pastor priests per Catholic",
  "Share of non-vacant parishes entrusted to religious women or laypeople",
  "Parishes entirely vacant per Catholic",
  "Catholics per priest"
)

design1_back_names <- paste0(design1_var_names, "_back")

# Create scatterplots for Design 1
design1_plots <- list()
for (i in seq_along(design1_var_names)) {
  orig_col <- design1_var_names[i]
  back_col <- design1_back_names[i]
  
  temp_data <- design1_plot_data %>%
    select(all_of(c(orig_col, back_col, "Region"))) %>%
    filter(complete.cases(select(., -Region)))
  
  if (nrow(temp_data) > 0) {
    p <- ggplot(temp_data, aes(x = .data[[orig_col]], y = .data[[back_col]])) +
      geom_point(alpha = 0.6, color = "#440154FF") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = str_wrap(paste("Design 1:", orig_col), width = 35),
           x = "Original", y = "Back-Transformed") +
      theme_minimal() +
      theme(plot.title = element_text(size = 9))
    
    design1_plots[[i]] <- p
  }
}

# Display Design 1 plots
grid.arrange(grobs = design1_plots[1:4], ncol = 2, 
             top = "Design 1: Transformation Verification (Part 1)")
grid.arrange(grobs = design1_plots[5:8], ncol = 2, 
             top = "Design 1: Transformation Verification (Part 2)")

# DESIGN 2 DIAGNOSTICS

# Get Design 2 transformed data (BEFORE z-score)
design2_transformed_only <- design2_transformed %>%
  select(Region, all_of(clergy_sacraments_transformed))

# Create back-transformed dataset for Design 2
design2_back_transformed <- design2_transformed_only %>%
  mutate(
    # Logit inverses
    `Yearly ordinations of diocesan priests as share of those incardinated on January 1_back` = 
      sigmoid(`Yearly ordinations of diocesan priests as share of those incardinated on January 1`) * 100,
    `Yearly deaths of diocesan priests as share of those incardinated on January 1_back` = 
      sigmoid(`Yearly deaths of diocesan priests as share of those incardinated on January 1`) * 100,
    `Yearly defections of diocesan priests as share of those incardinated at January 1_back` = 
      sigmoid(`Yearly defections of diocesan priests as share of those incardinated at January 1`) * 100,
    `Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants_back` = 
      sigmoid(`Vocation rate - philosophy+theology candidates for diocesan and religious clergy per inhabitant`) * 100000,
    `Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics_back` = 
      sigmoid(`Vocation rate - philosophy+theology candidates for diocesan and religious clergy per Catholic`) * 100000,
    `Baptisms per inhabitant_back` = sigmoid(`Baptisms per inhabitant`),
    `Share of mixed marriages_back` = sigmoid(`Share of mixed marriages`),
    
    # Log inverses
    `Philosophy+theology candidates for diocesan and religious clergy per 100 priests_back` = 
      exp(`Philosophy+theology candidates for diocesan and religious clergy per priest`) / 100,
    `Candidates for diocesan clergy in theology centres per Catholic_back` = 
      exp(`Candidates for diocesan clergy in theology centres per Catholic`),
    `Candidates for religious clergy in theology centres per Catholic_back` = 
      exp(`Candidates for religious clergy in theology centres per Catholic`),
    `Infant baptisms (people up to 7 years old) per Catholic_back` = 
      exp(`Infant baptisms (people up to 7 years old) per Catholic`),
    `Adult baptisms (people over 7 years old) per Catholic_back` = 
      exp(`Adult baptisms (people over 7 years old) per Catholic`),
    `Marriages between Catholics per Catholic_back` = 
      exp(`Marriages between Catholics per Catholic`),
    `Mixed marriages per inhabitant_back` = exp(`Mixed marriages per inhabitant`),
    `Confirmations per Catholic_back` = exp(`Confirmations per Catholic`),
    `First Communions per Catholic_back` = exp(`First Communions per Catholic`)
  ) %>%
  select(Region, ends_with("_back"))

# Join with original filtered data (design2_raw)
design2_plot_data <- design2_raw %>%
  select(Region, all_of(c(
    "Yearly ordinations of diocesan priests as share of those incardinated on January 1",
    "Yearly deaths of diocesan priests as share of those incardinated on January 1",
    "Yearly defections of diocesan priests as share of those incardinated at January 1",
    "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants",
    "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics",
    "Philosophy+theology candidates for diocesan and religious clergy per 100 priests",
    "Candidates for diocesan clergy in theology centres per Catholic",
    "Candidates for religious clergy in theology centres per Catholic",
    "Infant baptisms (people up to 7 years old) per Catholic",
    "Adult baptisms (people over 7 years old) per Catholic",
    "Baptisms per inhabitant",
    "Marriages between Catholics per Catholic",
    "Mixed marriages per inhabitant",
    "Share of mixed marriages",
    "Confirmations per Catholic",
    "First Communions per Catholic"
  ))) %>%
  inner_join(design2_back_transformed, by = "Region")

# Variable names for Design 2
design2_var_names <- c(
  "Yearly ordinations of diocesan priests as share of those incardinated on January 1",
  "Yearly deaths of diocesan priests as share of those incardinated on January 1",
  "Yearly defections of diocesan priests as share of those incardinated at January 1",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics",
  "Philosophy+theology candidates for diocesan and religious clergy per 100 priests",
  "Candidates for diocesan clergy in theology centres per Catholic",
  "Candidates for religious clergy in theology centres per Catholic",
  "Infant baptisms (people up to 7 years old) per Catholic",
  "Adult baptisms (people over 7 years old) per Catholic",
  "Baptisms per inhabitant",
  "Marriages between Catholics per Catholic",
  "Mixed marriages per inhabitant",
  "Share of mixed marriages",
  "Confirmations per Catholic",
  "First Communions per Catholic"
)

design2_back_names <- paste0(design2_var_names, "_back")

# Create scatterplots for Design 2
design2_plots <- list()
for (i in seq_along(design2_var_names)) {
  orig_col <- design2_var_names[i]
  back_col <- design2_back_names[i]
  
  temp_data <- design2_plot_data %>%
    select(all_of(c(orig_col, back_col, "Region"))) %>%
    filter(complete.cases(select(., -Region)))
  
  if (nrow(temp_data) > 0) {
    p <- ggplot(temp_data, aes(x = .data[[orig_col]], y = .data[[back_col]])) +
      geom_point(alpha = 0.6, color = "#21908CFF") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = str_wrap(paste("Design 2:", orig_col), width = 35),
           x = "Original", y = "Back-Transformed") +
      theme_minimal() +
      theme(plot.title = element_text(size = 9))
    
    design2_plots[[i]] <- p
  }
}

# Display Design 2 plots (4 grids of 4 plots each)
grid.arrange(grobs = design2_plots[1:4], ncol = 2, 
             top = "Design 2: Transformation Verification (Part 1)")
grid.arrange(grobs = design2_plots[5:8], ncol = 2, 
             top = "Design 2: Transformation Verification (Part 2)")
grid.arrange(grobs = design2_plots[9:12], ncol = 2, 
             top = "Design 2: Transformation Verification (Part 3)")
grid.arrange(grobs = design2_plots[13:16], ncol = 2, 
             top = "Design 2: Transformation Verification (Part 4)")

# ---- Correlation Analysis and Visualization ----

# DESIGN 1 CORRELATION ANALYSIS

# Compute correlation matrix for Design 1 (after transformations, before z-scores)
design1_numeric <- design1_transformed_only %>% select(-Region)
design1_cor_matrix <- cor(design1_numeric, use = "pairwise.complete.obs")

# Melt for plotting
design1_melted_cor <- melt(design1_cor_matrix)

# Create correlation heatmap for Design 1
design1_cor_plot <- ggplot(data = design1_melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = ifelse(abs(value) > 0.7, round(value, 2), "")), 
            color = "black", size = 2.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_fixed() +
  labs(title = "Design 1: Correlation Heatmap (Population & Territory)")

print(design1_cor_plot)

# DESIGN 2 CORRELATION ANALYSIS

# Compute correlation matrix for Design 2 (after transformations, before z-scores)
design2_numeric <- design2_transformed_only %>% select(-Region)
design2_cor_matrix <- cor(design2_numeric, use = "pairwise.complete.obs")

# Melt for plotting
design2_melted_cor <- melt(design2_cor_matrix)

# Create correlation heatmap for Design 2
design2_cor_plot <- ggplot(data = design2_melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = ifelse(abs(value) > 0.7, round(value, 2), "")), 
            color = "black", size = 2.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_fixed() +
  labs(title = "Design 2: Correlation Heatmap (Clergy & Sacraments)")

print(design2_cor_plot)


# ---- Density and skewness diagnostics ----

# Function to create before/after comparison with density and skewness
create_comparison_plot <- function(data_before, data_after, var_name_before, 
                                   var_name_after, transformation_type) {
  
  # Prepare data
  df <- data.frame(
    Before = data_before[[var_name_before]],
    After = data_after[[var_name_after]]
  )
  
  # Remove any infinite or NA values
  df <- df[is.finite(df$Before) & is.finite(df$After), ]
  
  # Calculate skewness
  skew_before <- round(skewness(df$Before, na.rm = TRUE), 2)
  skew_after <- round(skewness(df$After, na.rm = TRUE), 2)
  
  # Reshape for ggplot
  df_long <- pivot_longer(df, cols = c(Before, After), 
                          names_to = "Stage", values_to = "Value")
  
  # Set factor levels
  df_long$Stage <- factor(df_long$Stage, levels = c("Before", "After"))
  
  # Create density + histogram comparison
  p <- ggplot(df_long, aes(x = Value, fill = Stage)) +
    geom_histogram(aes(y = after_stat(density)), alpha = 0.5, 
                   position = "identity", bins = 30) +
    geom_density(alpha = 0.3, linewidth = 1) +
    facet_wrap(~Stage, scales = "free") +
    scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.3, end = 0.8) +
    labs(title = str_wrap(var_name_before, width = 40),
         subtitle = paste0("Transform: ", transformation_type,
                           " | Skew Before: ", skew_before, 
                           " | Skew After: ", skew_after),
         x = "Value", y = "Density") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 9, face = "bold"),
          plot.subtitle = element_text(size = 7.5, color = "gray40"))
  
  return(p)
}

# DESIGN 1: Density & Skewness Diagnostics

# Select a few representative variables from Design 1
design1_density_plots <- list()

# Variable 1: Catholics per 100 inhabitants (logit transform)
design1_density_plots[[1]] <- create_comparison_plot(
  analysis_data, design1_transformed_only,
  "Catholics per 100 inhabitants", "Share of Catholics",
  "Logit"
)

# Variable 2: Catholics per km^2 (log transform)
design1_density_plots[[2]] <- create_comparison_plot(
  analysis_data, design1_transformed_only,
  "Catholics per km^2", "Catholics per km^2",
  "Log"
)

# Variable 3: Share of diocesan pastors (logit transform)
design1_density_plots[[3]] <- create_comparison_plot(
  analysis_data, design1_transformed_only,
  "Share of diocesan pastors", "Share of diocesan pastors",
  "Logit"
)

# Variable 4: Catholics per priest (log transform)
design1_density_plots[[4]] <- create_comparison_plot(
  analysis_data, design1_transformed_only,
  "Catholics per priest", "Catholics per priest",
  "Log"
)

# Display Design 1 plots
grid.arrange(grobs = design1_density_plots[1:4], ncol = 2,
             top = "Design 1: Distribution Changes After Transformation")

# DESIGN 2: Density & Skewness Diagnostics

# Select representative variables from Design 2
design2_density_plots <- list()

# Variable 1: Yearly ordinations (logit transform)
design2_density_plots[[1]] <- create_comparison_plot(
  design2_raw, design2_transformed_only,
  "Yearly ordinations of diocesan priests as share of those incardinated on January 1",
  "Yearly ordinations of diocesan priests as share of those incardinated on January 1",
  "Logit"
)

# Variable 2: Infant baptisms per Catholic (log transform)
design2_density_plots[[2]] <- create_comparison_plot(
  design2_raw, design2_transformed_only,
  "Infant baptisms (people up to 7 years old) per Catholic",
  "Infant baptisms (people up to 7 years old) per Catholic",
  "Log"
)

# Variable 3: Share of mixed marriages (logit transform)
design2_density_plots[[3]] <- create_comparison_plot(
  design2_raw, design2_transformed_only,
  "Share of mixed marriages",
  "Share of mixed marriages",
  "Logit"
)

# Variable 4: Confirmations per Catholic (log transform)
design2_density_plots[[4]] <- create_comparison_plot(
  design2_raw, design2_transformed_only,
  "Confirmations per Catholic",
  "Confirmations per Catholic",
  "Log"
)

# Display Design 2 plots
grid.arrange(grobs = design2_density_plots[1:4], ncol = 2,
             top = "Design 2: Distribution Changes After Transformation")

# ---- Imputation of NaNs ----

# zscores <- cluster_data_2022_std %>%
#   mutate(across(where(is.numeric), ~ (.-mean(., na.rm=TRUE))/sd(., na.rm=TRUE)))
# summary(zscores)
# 
# numeric_z <- zscores %>% select(-Region)
# 
# # Perform KNN imputation: k=15 nearest neighbors, using Euclidean distance (default in VIM::kNN).
# # It computes distances based on available (non-NA) values, scaling them appropriately.
# # Set imp_var=FALSE to avoid adding imputation indicator columns.
# imputed_numeric <- kNN(numeric_z, k = 15, dist_var = colnames(numeric_z), imp_var = FALSE)
# 
# # Combine the imputed numeric data with the original 'Region' column
# imputed_z <- data.frame(Region = zscores$Region, imputed_numeric)
# 
# # View the first few rows to check
# colnames(imputed_z) <- colnames(zscores)
# head(imputed_z)


# ---- Cluster analysis: population and territory ----

# Design 1 ready for clustering
popu_terr_final <- design1_final
rownames(popu_terr_final) <- popu_terr_final$Region

# Extract numeric variables (exclude Region column)
popu_terr <- popu_terr_final %>% select(-Region)

# Compute PCA on the four highly correlated variables
correlated_vars <- c("Share of Catholics", "Catholics per km^2", "Catholics per pastoral centre", "Catholics per priest")
four_vars <- popu_terr[, correlated_vars]
pca <- prcomp(four_vars, center = FALSE, scale = FALSE)
summary(pca)
pc_scores <- pca$x[, 1:2] # PC1 and PC2
pc_scores_std <- scale(pc_scores)

# Create new data: remove the four correlated vars, add the standardized weighted PCs
other_vars <- setdiff(names(popu_terr), correlated_vars)
popu_terr <- cbind(
  popu_terr[, other_vars],
  PC1 = pc_scores_std[,1],
  PC2 = pc_scores_std[,2]
)

# Quick check: sds should be 1 for all columns now
print(apply(popu_terr, 2, sd))

# Convert to matrix for easier computation
pt_data_matrix <- as.matrix(popu_terr)

# Define the Huber loss function
huber_loss <- function(e, delta = 1.345) {
  ifelse(abs(e) <= delta, 0.5 * e^2, delta * abs(e) - 0.5 * delta^2)
}

# Define the Huber distance function between two vectors
huber_distance <- function(x, y, delta = 1.345) {
  diffs <- x - y
  sqrt(sum(huber_loss(diffs, delta)))
}

# Compute the pairwise Huber distance matrix
n <- nrow(pt_data_matrix)
pt_dist_matrix <- matrix(0, n, n)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    pt_dist_matrix[i, j] <- huber_distance(pt_data_matrix[i, ], pt_data_matrix[j, ])
    pt_dist_matrix[j, i] <- pt_dist_matrix[i, j]
  }
}

# Convert to dist object
pt_huber_dist <- as.dist(pt_dist_matrix)

# --- 1) Hierarchical clustering (avg linkage, Huber distance) ---
pt_hc <- hclust(pt_huber_dist, method = "average")

# Dendrogram
plot(pt_hc,
     labels = popu_terr_final$Region,
     main = "Exploratory hierarchical clustering (Huber distance)",
     xlab = "Countries", sub = NULL, horiz = TRUE, cex = 0.4)

# Interactive dendrogram
pt_ggdend <- ggdendrogram(pt_hc, rotate = TRUE, size = 2) +
  theme(axis.text.x = element_text(size = 6, angle = 90))
pt_interactive_dend <- ggplotly(pt_ggdend)
pt_interactive_dend

# Silhouette method (on HC cutree, Huber distance) to choose k
fviz_nbclust(popu_terr,
             FUNcluster = function(x, k) list(cluster = cutree(pt_hc, k = k)),
             method = "silhouette", k.max = 15, diss = pt_huber_dist) +
  labs(title = "Silhouette Method (Huber distance)")

# Compute silhouette widths explicitly to extract optimal k
library(cluster)
ks <- 2:15
sil_hc <- sapply(ks, function(k) {
  cl <- cutree(pt_hc, k = k)
  summary(silhouette(cl, pt_huber_dist))$avg.width
})
k_opt_hc <- ks[which.max(sil_hc)]
message(sprintf("Optimal k (HC avg Huber + silhouette) = %d", k_opt_hc))

# --- 2) PAM clustering (Huber distance) with k from HC silhouette ---
pam_fit <- pam(pt_huber_dist, k = k_opt_hc, diss = TRUE)

# Add cluster labels to data
popu_terr_final$Cluster_HuberPAM <- pam_fit$clustering
medoid_ids <- pam_fit$id.med
medoid_regions <- rownames(popu_terr_final)[medoid_ids]
medoid_regions
fviz_silhouette(silhouette(pam_fit), label = FALSE) +
  labs(title = sprintf("Silhouette (PAM + Huber), k = %d", k_opt_hc))

# --- 3) Euclidean distance (Ward's method) ---
eu_dist <- dist(pt_data_matrix, method = "euclidean")

# Hierarchical clustering with Ward.D2 (Euclidean only)
pt_hc_eu <- hclust(eu_dist, method = "ward.D2")
plot(pt_hc_eu,
     labels = popu_terr_final$Region,
     main = "Robustness check: HC (Euclidean, Ward.D2)",
     xlab = "Countries", sub = NULL, horiz = TRUE, cex = 0.4)

#CH index for optimal k (Euclidean only)
pt_nb_res <- NbClust(pt_data_matrix, distance = "euclidean",
                     min.nc = 2, max.nc = 15,
                     method = "ward.D2", index = "ch")

#Show all CH values for k=2 to 15
ch_values <- pt_nb_res$All.index
print("CH indices for k=2 to 15:")
print(ch_values)

#Plot CH indices
plot(2:15, ch_values, type = "b", pch = 19, col = "blue",
     main = "Calinski-Harabasz (CH) Index vs Number of Clusters",
     xlab = "Number of Clusters (k)", ylab = "CH Index")
k_opt_ward <- pt_nb_res$Best.nc[1]
message(sprintf("Optimal k (HC Ward Euclidean + CH index) = %d", k_opt_ward))

#User-defined k (k <- k_opt_ward for optimal)
chosen_k_pt <- 3

#Cut tree at chosen k and assign clusters for completeness
popu_terr_final$Cluster_WardEu <- cutree(pt_hc_eu, k = chosen_k_pt)

#k = chosen_k_pt to have clusters (optimal by CH index or user-defined, e.g., 3)
#Interpretation (optional but useful: median profiles)
prof_huberpam <- cbind(Region = popu_terr_final$Region,
                       cl = popu_terr_final$Cluster_HuberPAM) %>%
  as_tibble() %>%
  left_join(as.data.frame(design1_final), by = "Region") %>%
  group_by(cl) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
prof_huberpam

##  VISUALIZATIONS & DIAGNOSTICS (Ward clusters only)

# Median profiles (raw values)
prof_ward <- popu_terr_final %>%
  select(Region, Cluster_WardEu) %>%
  left_join(design1_final, by = "Region") %>%
  group_by(Cluster_WardEu) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE)) %>%
  rename(Cluster = Cluster_WardEu)
prof_ward

# Radar / spider chart (base R - works everywhere)
radar_data <- prof_ward %>%
  select(-Cluster)

# Assume original variable names are kept as colnames(radar_data)

# Compute global max/min per variable (for consistent scaling)
max_row <- apply(radar_data, 2, max) * 1.1
min_row <- apply(radar_data, 2, min) * 0.9

# Check if number of variables >= 3
num_vars <- ncol(radar_data)
if (num_vars < 3) {
  message("Skipping radar charts: At least 3 variables required.")
} else {
  # Set up side-by-side layout
  num_clusters <- nrow(radar_data)
  par(mfrow = c(1, num_clusters), mar = c(2, 2, 3, 2))
  
  cluster_colors <- viridis::viridis(num_clusters)
  cluster_labels <- paste("C", prof_ward$Cluster, sep = "")
  
  for (i in 1:num_clusters) {
    # Single cluster row
    cluster_row <- radar_data[i, , drop = FALSE]
    
    # Bind with global max/min
    single_data <- rbind(max_row, min_row, cluster_row)
    
    radarchart(single_data,
               axistype = 1,
               pcol = cluster_colors[i],
               pfcol = scales::alpha(cluster_colors[i], 0.3),
               plwd = 3,
               cglcol = "grey70",
               title = cluster_labels[i])
  }
  par(mfrow = c(1, 1))  # Reset layout
}

# Cluster scatter in first two PCs of the full data
fviz_cluster(list(data = pt_data_matrix, cluster = popu_terr_final$Cluster_WardEu),
             geom = "point", ellipse.type = "convex", show.clust.cent = TRUE) +
  labs(title = "Ward Clusters in Reduced Space (PC1-PC2)")

# Silhouette plot
sil_ward <- silhouette(popu_terr_final$Cluster_WardEu, eu_dist)
fviz_silhouette(sil_ward) +
  labs(title = "Silhouette Plot - Ward Clusters") +
  theme_minimal()
message(sprintf("Average silhouette width: %.3f", mean(sil_ward[, 3])))

# Cophenetic correlation
coph_cor <- cor(cophenetic(pt_hc_eu), eu_dist)
message(sprintf("Cophenetic correlation (Ward tree): %.3f", coph_cor))

# Dunn index (compactness & separation)
dunn_idx <- dunn(distance = eu_dist, clusters = popu_terr_final$Cluster_WardEu)
message(sprintf("Dunn index: %.4f", dunn_idx))

# Bootstrap stability (Jaccard indices)
set.seed(123)
boot_ward <- clusterboot(pt_data_matrix,
                         B = 100,
                         clustermethod = hclustCBI,
                         method = "ward.D2",
                         k = chosen_k_pt)
boot_ward$bootmean   # mean Jaccard per cluster
boot_ward$bootbrd    # number of times each cluster dissolved

# Optional: k-means for same k (Euclidean baseline)
set.seed(123)
kmeans_fit <- kmeans(pt_data_matrix, centers = pt_nb_res$Best.nc[1], nstart = 50)
popu_terr_final$Cluster_KmeansEu <- kmeans_fit$cluster
eu_dist <- dist(pt_data_matrix, "euclidean")
sil_km3 <- silhouette(kmeans_fit$cluster, eu_dist)
mean_sil_km3 <- summary(sil_km3)$avg.width
mean_sil_km3

#Interpretation:
prof2 <- cbind(Region = popu_terr_final$Region,
               cl2 = popu_terr_final$Cluster_HuberPAM) %>%
  as_tibble() %>% # make sure it’s a tibble
  left_join(as.data.frame(design1_final), by = "Region") %>%
  group_by(cl2) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
prof2


# ---- Cluster analysis: clergy and sacraments ----

# Design 2 ready for clustering
clergy_sac_final <- design2_final
rownames(clergy_sac_final) <- clergy_sac_final$Region

# Extract numeric variables (exclude Region column)
clergy_sac <- clergy_sac_final %>% select(-Region)

# Compute PCA on the first group of correlated variables (pair: first PC only)
correlated_vars1 <- c("First Communions per Catholic", "Confirmations per Catholic")
two_vars <- clergy_sac[, correlated_vars1]
pca1 <- prcomp(two_vars, center = FALSE, scale = FALSE)
summary(pca1)
pc1_group1 <- pca1$x[,1]

# Standardize PC1 to sd=1 (like other variables)
pc1_group1_std <- pc1_group1 / sd(pc1_group1)

# Compute PCA on the second group of correlated variables (four: first two PCs)
correlated_vars2 <- c(
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per inhabitant",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per Catholic",
  "Philosophy+theology candidates for diocesan and religious clergy per priest",
  "Candidates for diocesan clergy in theology centres per Catholic"
)
four_vars <- clergy_sac[, correlated_vars2]
pca2 <- prcomp(four_vars, center = FALSE, scale = FALSE)
summary(pca2)
pc_scores_2 <- pca2$x[, 1:2] # PC1 and PC2
pc_scores_std_2 <- scale(pc_scores_2)

# Create new data: remove the correlated vars from both groups, add the standardized PCs
all_correlated <- c(correlated_vars1, correlated_vars2)
other_vars <- setdiff(names(clergy_sac), all_correlated)
clergy_sac <- cbind(
  clergy_sac[, other_vars],
  PC_group1 = pc1_group1_std,
  PC_group2_1 = pc_scores_std_2[,1],
  PC_group2_2 = pc_scores_std_2[,2]
)

# Quick check: sds should be 1 for all columns now
print(apply(clergy_sac, 2, sd))

# Convert to matrix for easier computation
cs_data_matrix <- as.matrix(clergy_sac)

# Define the Huber loss function
huber_loss <- function(e, delta = 1.345) {
  ifelse(abs(e) <= delta, 0.5 * e^2, delta * abs(e) - 0.5 * delta^2)
}

# Define the Huber distance function between two vectors
huber_distance <- function(x, y, delta = 1.345) {
  diffs <- x - y
  sqrt(sum(huber_loss(diffs, delta)))
}

# Compute the pairwise Huber distance matrix
n <- nrow(cs_data_matrix)
cs_dist_matrix <- matrix(0, n, n)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    cs_dist_matrix[i, j] <- huber_distance(cs_data_matrix[i, ], cs_data_matrix[j, ])
    cs_dist_matrix[j, i] <- cs_dist_matrix[i, j]
  }
}

# Convert to dist object
cs_huber_dist <- as.dist(cs_dist_matrix)

# --- 1) Hierarchical clustering (avg linkage, Huber distance) ---
cs_hc <- hclust(cs_huber_dist, method = "average")

# Dendrogram
plot(cs_hc,
     labels = clergy_sac_final$Region,
     main = "Exploratory hierarchical clustering (Huber distance)",
     xlab = "Countries", sub = NULL, horiz = TRUE, cex = 0.4)

# Interactive dendrogram
cs_ggdend <- ggdendrogram(cs_hc, rotate = TRUE, size = 2) +
  theme(axis.text.x = element_text(size = 6, angle = 90))
cs_interactive_dend <- ggplotly(cs_ggdend)
cs_interactive_dend

# Silhouette method (on HC cutree, Huber distance) to choose k
fviz_nbclust(clergy_sac,
             FUNcluster = function(x, k) list(cluster = cutree(cs_hc, k = k)),
             method = "silhouette", k.max = 15, diss = cs_huber_dist) +
  labs(title = "Silhouette Method (Huber distance)")

# Compute silhouette widths explicitly to extract optimal k
ks <- 2:15
sil_hc <- sapply(ks, function(k) {
  cl <- cutree(cs_hc, k = k)
  summary(silhouette(cl, cs_huber_dist))$avg.width
})
k_opt_hc <- ks[which.max(sil_hc)]
message(sprintf("Optimal k (HC avg Huber + silhouette) = %d", k_opt_hc))

# --- 2) PAM clustering (Huber distance) with k from HC silhouette ---
pam_fit <- pam(cs_huber_dist, k = k_opt_hc, diss = TRUE)

# Add cluster labels to data
clergy_sac_final$Cluster_HuberPAM <- pam_fit$clustering
medoid_ids <- pam_fit$id.med
medoid_regions <- rownames(clergy_sac_final)[medoid_ids]
medoid_regions
fviz_silhouette(silhouette(pam_fit), label = FALSE) +
  labs(title = sprintf("Silhouette (PAM + Huber), k = %d", k_opt_hc))

# --- 3) Euclidean distance (Ward's method) ---
eu_dist <- dist(cs_data_matrix, method = "euclidean")

# Hierarchical clustering with Ward.D2 (Euclidean only)
cs_hc_eu <- hclust(eu_dist, method = "ward.D2")
plot(cs_hc_eu,
     labels = clergy_sac_final$Region,
     main = "Robustness check: HC (Euclidean, Ward.D2)",
     xlab = "Countries", sub = NULL, horiz = TRUE, cex = 0.4)

# CH index for optimal k (Euclidean only)
cs_nb_res <- NbClust(cs_data_matrix, distance = "euclidean",
                     min.nc = 2, max.nc = 15,
                     method = "ward.D2", index = "ch")

# Show all CH values for k=2 to 15
ch_values <- cs_nb_res$All.index
print("CH indices for k=2 to 15:")
print(ch_values)

# Plot CH indices
plot(2:15, ch_values, type = "b", pch = 19, col = "blue",
     main = "Calinski-Harabasz (CH) Index vs Number of Clusters",
     xlab = "Number of Clusters (k)", ylab = "CH Index")
k_opt_ward <- cs_nb_res$Best.nc[1]
message(sprintf("Optimal k (HC Ward Euclidean + CH index) = %d", k_opt_ward))

# User-defined k (default to optimal; change as needed, e.g., chosen_k_cs <- 3)
chosen_k_cs <- 3

# Cut tree at chosen k and assign clusters for completeness
clergy_sac_final$Cluster_WardEu <- cutree(cs_hc_eu, k = chosen_k_cs)

# Interpretation (optional but useful: median profiles)
prof_huberpam <- cbind(Region = clergy_sac_final$Region,
                       cl = clergy_sac_final$Cluster_HuberPAM) %>%
  as_tibble() %>%
  left_join(as.data.frame(design2_final), by = "Region") %>%
  group_by(cl) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
prof_huberpam

## VISUALIZATIONS & DIAGNOSTICS (Ward clusters only)

# Median profiles (raw values)
prof_ward <- clergy_sac_final %>%
  select(Region, Cluster_WardEu) %>%
  left_join(design2_final, by = "Region") %>%
  group_by(Cluster_WardEu) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE)) %>%
  rename(Cluster = Cluster_WardEu)
prof_ward

# Radar / spider chart (base R - works everywhere)
radar_data <- prof_ward %>%
  select(-Cluster)

# Assume original variable names are kept as colnames(radar_data)

# Compute global max/min per variable (for consistent scaling)
max_row <- apply(radar_data, 2, max) * 1.1
min_row <- apply(radar_data, 2, min) * 0.9

# Check if number of variables >= 3
num_vars <- ncol(radar_data)
if (num_vars < 3) {
  message("Skipping radar charts: At least 3 variables required.")
} else {
  # Set up side-by-side layout
  num_clusters <- nrow(radar_data)
  par(mfrow = c(1, num_clusters), mar = c(2, 2, 3, 2))
  
  cluster_colors <- viridis::viridis(num_clusters)
  cluster_labels <- paste("C", prof_ward$Cluster, sep = "")
  
  for (i in 1:num_clusters) {
    # Single cluster row
    cluster_row <- radar_data[i, , drop = FALSE]
    
    # Bind with global max/min
    single_data <- rbind(max_row, min_row, cluster_row)
    
    radarchart(single_data,
               axistype = 1,
               pcol = cluster_colors[i],
               pfcol = scales::alpha(cluster_colors[i], 0.3),
               plwd = 3,
               cglcol = "grey70",
               title = cluster_labels[i])
  }
  par(mfrow = c(1, 1))  # Reset layout
}

# Cluster scatter in first two PCs of the full data
fviz_cluster(list(data = cs_data_matrix, cluster = clergy_sac_final$Cluster_WardEu),
             geom = "point", ellipse.type = "convex", show.clust.cent = TRUE) +
  labs(title = "Ward Clusters in Reduced Space (PC1-PC2)")

# Silhouette plot
sil_ward <- silhouette(clergy_sac_final$Cluster_WardEu, eu_dist)
fviz_silhouette(sil_ward) +
  labs(title = "Silhouette Plot - Ward Clusters") +
  theme_minimal()
message(sprintf("Average silhouette width: %.3f", mean(sil_ward[, 3])))

# Cophenetic correlation
coph_cor <- cor(cophenetic(cs_hc_eu), eu_dist)
message(sprintf("Cophenetic correlation (Ward tree): %.3f", coph_cor))

# Dunn index (compactness & separation)
dunn_idx <- dunn(distance = eu_dist, clusters = clergy_sac_final$Cluster_WardEu)
message(sprintf("Dunn index: %.4f", dunn_idx))

# Bootstrap stability (Jaccard indices)
set.seed(123)
boot_ward <- clusterboot(cs_data_matrix,
                         B = 100,
                         clustermethod = hclustCBI,
                         method = "ward.D2",
                         k = chosen_k_cs)
boot_ward$bootmean   # mean Jaccard per cluster
boot_ward$bootbrd    # number of times each cluster dissolved

# 8. Final cluster sizes
table(clergy_sac_final$Cluster_WardEu)


#####
# ROBUSTNESS CHECKS
# δ-sensitivity for Huber distance

# Define δ values to test
delta_values <- c(1.0, 1.345, 2.0)

# Storage for results
delta_results <- list()

for (delta_val in delta_values) {
  
  message(sprintf("\n========== Testing δ = %.3f ==========", delta_val))
  
  # --- Population & Territory Analysis ---
  
  # Recompute Huber distance with current δ
  pt_dist_matrix_delta <- matrix(0, n, n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      pt_dist_matrix_delta[i, j] <- huber_distance(pt_data_matrix[i, ], 
                                                   pt_data_matrix[j, ], 
                                                   delta = delta_val)
      pt_dist_matrix_delta[j, i] <- pt_dist_matrix_delta[i, j]
    }
  }
  pt_huber_dist_delta <- as.dist(pt_dist_matrix_delta)
  
  # Hierarchical clustering
  pt_hc_delta <- hclust(pt_huber_dist_delta, method = "average")
  
  # Find optimal k via silhouette
  ks <- 2:15
  sil_pt_delta <- sapply(ks, function(k) {
    pam_fit <- pam(pt_huber_dist_delta, k = k, diss = TRUE)
    summary(silhouette(pam_fit$clustering, pt_huber_dist_delta))$avg.width
  })
  k_opt_pt_delta <- ks[which.max(sil_pt_delta)]
  
  # PAM clustering
  pam_fit_pt_delta <- pam(pt_huber_dist_delta, k = k_opt_pt_delta, diss = TRUE)
  
  # Medoids
  medoid_ids_pt_delta <- pam_fit_pt_delta$id.med
  medoid_regions_pt_delta <- rownames(popu_terr_final)[medoid_ids_pt_delta]
  
  # Within-cluster dispersion
  cluster_dispersion_pt <- sapply(1:k_opt_pt_delta, function(cl) {
    idx <- which(pam_fit_pt_delta$clustering == cl)
    medoid_idx <- medoid_ids_pt_delta[cl]
    sum(sapply(idx, function(i) {
      sum(huber_loss(pt_data_matrix[i, ] - pt_data_matrix[medoid_idx, ], delta = delta_val))
    }))
  })
  
  # Cophenetic correlation
  pt_coph_cor_delta <- cor(as.numeric(pt_huber_dist_delta), 
                           as.numeric(cophenetic(pt_hc_delta)))
  
  # Store results
  delta_results[[paste0("pt_delta_", delta_val)]] <- list(
    delta = delta_val,
    k = k_opt_pt_delta,
    avg_silhouette = max(sil_pt_delta),
    cluster_sizes = table(pam_fit_pt_delta$clustering),
    medoids = medoid_regions_pt_delta,
    dispersion = cluster_dispersion_pt,
    cophenetic_cor = pt_coph_cor_delta,
    clustering = pam_fit_pt_delta$clustering
  )
  
  
  # --- Clergy & Sacraments Analysis ---
  
  # Recompute Huber distance with current δ
  cs_dist_matrix_delta <- matrix(0, n, n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      cs_dist_matrix_delta[i, j] <- huber_distance(cs_data_matrix[i, ], 
                                                   cs_data_matrix[j, ], 
                                                   delta = delta_val)
      cs_dist_matrix_delta[j, i] <- cs_dist_matrix_delta[i, j]
    }
  }
  cs_huber_dist_delta <- as.dist(cs_dist_matrix_delta)
  
  # Hierarchical clustering
  cs_hc_delta <- hclust(cs_huber_dist_delta, method = "average")
  
  # Find optimal k via silhouette
  sil_cs_delta <- sapply(ks, function(k) {
    pam_fit <- pam(cs_huber_dist_delta, k = k, diss = TRUE)
    summary(silhouette(pam_fit$clustering, cs_huber_dist_delta))$avg.width
  })
  k_opt_cs_delta <- ks[which.max(sil_cs_delta)]
  
  # PAM clustering
  pam_fit_cs_delta <- pam(cs_huber_dist_delta, k = k_opt_cs_delta, diss = TRUE)
  
  # Medoids
  medoid_ids_cs_delta <- pam_fit_cs_delta$id.med
  medoid_regions_cs_delta <- rownames(clergy_sac_final)[medoid_ids_cs_delta]
  
  # Within-cluster dispersion
  cluster_dispersion_cs <- sapply(1:k_opt_cs_delta, function(cl) {
    idx <- which(pam_fit_cs_delta$clustering == cl)
    medoid_idx <- medoid_ids_cs_delta[cl]
    sum(sapply(idx, function(i) {
      sum(huber_loss(cs_data_matrix[i, ] - cs_data_matrix[medoid_idx, ], delta = delta_val))
    }))
  })
  
  # Cophenetic correlation
  cs_coph_cor_delta <- cor(as.numeric(cs_huber_dist_delta), 
                           as.numeric(cophenetic(cs_hc_delta)))
  
  # Store results
  delta_results[[paste0("cs_delta_", delta_val)]] <- list(
    delta = delta_val,
    k = k_opt_cs_delta,
    avg_silhouette = max(sil_cs_delta),
    cluster_sizes = table(pam_fit_cs_delta$clustering),
    medoids = medoid_regions_cs_delta,
    dispersion = cluster_dispersion_cs,
    cophenetic_cor = cs_coph_cor_delta,
    clustering = pam_fit_cs_delta$clustering
  )
}

# --- Compare across δ values ---

# Population & Territory comparison
pt_delta_comparison <- data.frame(
  delta = delta_values,
  k = sapply(delta_values, function(d) delta_results[[paste0("pt_delta_", d)]]$k),
  avg_silhouette = sapply(delta_values, function(d) delta_results[[paste0("pt_delta_", d)]]$avg_silhouette),
  cophenetic_cor = sapply(delta_values, function(d) delta_results[[paste0("pt_delta_", d)]]$cophenetic_cor)
)

# ARI between different δ solutions (Population & Territory)
pt_ari_matrix <- matrix(NA, length(delta_values), length(delta_values))
rownames(pt_ari_matrix) <- colnames(pt_ari_matrix) <- paste0("δ=", delta_values)
for (i in 1:length(delta_values)) {
  for (j in 1:length(delta_values)) {
    cl_i <- delta_results[[paste0("pt_delta_", delta_values[i])]]$clustering
    cl_j <- delta_results[[paste0("pt_delta_", delta_values[j])]]$clustering
    pt_ari_matrix[i, j] <- mclust::adjustedRandIndex(cl_i, cl_j)
  }
}

# Clergy & Sacraments comparison
cs_delta_comparison <- data.frame(
  delta = delta_values,
  k = sapply(delta_values, function(d) delta_results[[paste0("cs_delta_", d)]]$k),
  avg_silhouette = sapply(delta_values, function(d) delta_results[[paste0("cs_delta_", d)]]$avg_silhouette),
  cophenetic_cor = sapply(delta_values, function(d) delta_results[[paste0("cs_delta_", d)]]$cophenetic_cor)
)

# ARI between different δ solutions (Clergy & Sacraments)
cs_ari_matrix <- matrix(NA, length(delta_values), length(delta_values))
rownames(cs_ari_matrix) <- colnames(cs_ari_matrix) <- paste0("δ=", delta_values)
for (i in 1:length(delta_values)) {
  for (j in 1:length(delta_values)) {
    cl_i <- delta_results[[paste0("cs_delta_", delta_values[i])]]$clustering
    cl_j <- delta_results[[paste0("cs_delta_", delta_values[j])]]$clustering
    cs_ari_matrix[i, j] <- mclust::adjustedRandIndex(cl_i, cl_j)
  }
}

# Print results
cat("\n========== Population & Territory: δ-sensitivity summary ==========\n")
print(pt_delta_comparison)
cat("\nARI matrix (Population & Territory):\n")
print(round(pt_ari_matrix, 3))

cat("\n========== Clergy & Sacraments: δ-sensitivity summary ==========\n")
print(cs_delta_comparison)
cat("\nARI matrix (Clergy & Sacraments):\n")
print(round(cs_ari_matrix, 3))

# Medoid persistence
cat("\n--- Medoid Persistence (Population & Territory) ---\n")
for (d in delta_values) {
  cat(sprintf("δ=%.3f: %s\n", d, 
              paste(delta_results[[paste0("pt_delta_", d)]]$medoids, collapse=", ")))
}

cat("\n--- Medoid Persistence (Clergy & Sacraments) ---\n")
for (d in delta_values) {
  cat(sprintf("δ=%.3f: %s\n", d, 
              paste(delta_results[[paste0("cs_delta_", d)]]$medoids, collapse=", ")))
}

# Identify regions that change clusters
cat("\n--- Membership Flips (Population & Territory) ---\n")
pt_cl_1.0 <- delta_results[["pt_delta_1"]]$clustering
pt_cl_2.0 <- delta_results[["pt_delta_2"]]$clustering
pt_flips <- which(pt_cl_1.0 != pt_cl_2.0)
if (length(pt_flips) > 0) {
  cat("Regions changing clusters between δ=1.0 and δ=2.0:\n")
  for (idx in pt_flips) {
    cat(sprintf("  %s: cluster %d (δ=1.0) → cluster %d (δ=2.0)\n",
                rownames(pt_data_matrix)[idx], pt_cl_1.0[idx], pt_cl_2.0[idx]))
  }
} else {
  cat("No membership changes between δ=1.0 and δ=2.0\n")
}

cat("\n--- Membership Flips (Clergy & Sacraments) ---\n")
cs_cl_1.0 <- delta_results[["cs_delta_1"]]$clustering
cs_cl_2.0 <- delta_results[["cs_delta_2"]]$clustering
cs_flips <- which(cs_cl_1.0 != cs_cl_2.0)
if (length(cs_flips) > 0) {
  cat("Regions changing clusters between δ=1.0 and δ=2.0:\n")
  for (idx in cs_flips) {
    cat(sprintf("  %s: cluster %d (δ=1.0) → cluster %d (δ=2.0)\n",
                rownames(cs_data_matrix)[idx], cs_cl_1.0[idx], cs_cl_2.0[idx]))
  }
} else {
  cat("No membership changes between δ=1.0 and δ=2.0\n")
}


# ------------------------------------------------------------------------------
# (f) Stability via Bootstrap (Jaccard indices)
# ------------------------------------------------------------------------------

library(clue)  # for solve_LSAP (Hungarian matching)

# Bootstrap parameters
B <- 500  # number of bootstrap replicates
set.seed(42)
delta_bootstrap <- 1.345  # Use your preferred δ for bootstrap

# Helper function: Hungarian matching of clusters
match_clusters <- function(ref_cl, boot_cl) {
  k_ref <- max(ref_cl)
  k_boot <- max(boot_cl)
  
  # Contingency table
  cont_table <- table(ref_cl, boot_cl)
  
  # Pad if needed
  if (k_ref > k_boot) {
    cont_table <- cbind(cont_table, matrix(0, k_ref, k_ref - k_boot))
  } else if (k_boot > k_ref) {
    cont_table <- rbind(cont_table, matrix(0, k_boot - k_ref, k_boot))
  }
  
  # Hungarian algorithm (maximize overlap)
  assignment <- solve_LSAP(cont_table, maximum = TRUE)
  
  # Relabel boot_cl
  boot_cl_matched <- boot_cl
  for (i in 1:length(assignment)) {
    boot_cl_matched[boot_cl == i] <- assignment[i]
  }
  
  return(boot_cl_matched)
}

# Helper function: Compute Jaccard per cluster
jaccard_per_cluster <- function(ref_cl, boot_cl, boot_idx) {
  k <- max(ref_cl)
  jaccard_vals <- numeric(k)
  
  for (cl in 1:k) {
    ref_members <- which(ref_cl == cl)
    boot_members_full <- which(boot_cl == cl)
    
    # Restrict to bootstrap sample
    boot_members <- intersect(boot_members_full, boot_idx)
    ref_members_in_boot <- intersect(ref_members, boot_idx)
    
    intersection <- length(intersect(ref_members_in_boot, boot_members))
    union <- length(union(ref_members_in_boot, boot_members))
    
    jaccard_vals[cl] <- if (union > 0) intersection / union else NA
  }
  
  return(jaccard_vals)
}


# --- Population & Territory Bootstrap ---

cat("\n========== Bootstrap Stability: Population & Territory ==========\n")

# Reference solution (using δ=1.345)
ref_cl_pt <- delta_results[["pt_delta_1.345"]]$clustering
k_ref_pt <- max(ref_cl_pt)

# Storage
jaccard_pt <- matrix(NA, B, k_ref_pt)
medoid_freq_pt <- matrix(0, nrow(pt_data_matrix), k_ref_pt)
rownames(medoid_freq_pt) <- rownames(pt_data_matrix)
co_assoc_pt <- matrix(0, nrow(pt_data_matrix), nrow(pt_data_matrix))

for (b in 1:B) {
  if (b %% 100 == 0) cat(sprintf("  Bootstrap replicate %d/%d\n", b, B))
  
  # Bootstrap sample
  boot_idx <- sample(1:nrow(pt_data_matrix), replace = TRUE)
  boot_data <- pt_data_matrix[boot_idx, ]
  
  # Compute Huber distance on bootstrap sample
  n_boot <- nrow(boot_data)
  boot_dist_matrix <- matrix(0, n_boot, n_boot)
  for (i in 1:(n_boot-1)) {
    for (j in (i+1):n_boot) {
      d <- huber_distance(boot_data[i, ], boot_data[j, ], delta = delta_bootstrap)
      boot_dist_matrix[i, j] <- d
      boot_dist_matrix[j, i] <- d
    }
  }
  boot_dist <- as.dist(boot_dist_matrix)
  
  # PAM with fixed k
  pam_boot <- pam(boot_dist, k = k_ref_pt, diss = TRUE)
  boot_cl <- pam_boot$clustering
  
  # Map back to original indices
  boot_cl_full <- rep(NA, nrow(pt_data_matrix))
  boot_cl_full[boot_idx] <- boot_cl
  
  # Match clusters
  boot_cl_matched <- match_clusters(ref_cl_pt, boot_cl_full)
  
  # Jaccard
  jaccard_pt[b, ] <- jaccard_per_cluster(ref_cl_pt, boot_cl_matched, boot_idx)
  
  # Medoid frequency
  boot_medoids <- boot_idx[pam_boot$id.med]
  for (i in 1:k_ref_pt) {
    medoid_freq_pt[boot_medoids[i], i] <- medoid_freq_pt[boot_medoids[i], i] + 1
  }
  
  # Co-association matrix
  for (i in 1:(nrow(pt_data_matrix)-1)) {
    for (j in (i+1):nrow(pt_data_matrix)) {
      if (!is.na(boot_cl_matched[i]) && !is.na(boot_cl_matched[j])) {
        if (boot_cl_matched[i] == boot_cl_matched[j]) {
          co_assoc_pt[i, j] <- co_assoc_pt[i, j] + 1
          co_assoc_pt[j, i] <- co_assoc_pt[j, i] + 1
        }
      }
    }
  }
}

# Normalize co-association
co_assoc_pt <- co_assoc_pt / B

# Summary statistics
jaccard_summary_pt <- data.frame(
  Cluster = 1:k_ref_pt,
  Median = apply(jaccard_pt, 2, median, na.rm = TRUE),
  Q25 = apply(jaccard_pt, 2, quantile, probs = 0.25, na.rm = TRUE),
  Q75 = apply(jaccard_pt, 2, quantile, probs = 0.75, na.rm = TRUE),
  Mean = apply(jaccard_pt, 2, mean, na.rm = TRUE)
)

cat("\nJaccard Index Summary (Population & Territory):\n")
print(round(jaccard_summary_pt, 3))

# Medoid selection frequency
cat("\nTop medoid candidates per cluster (Population & Territory):\n")
for (cl in 1:k_ref_pt) {
  top_medoids <- order(medoid_freq_pt[, cl], decreasing = TRUE)[1:3]
  cat(sprintf("Cluster %d: %s (%.1f%%), %s (%.1f%%), %s (%.1f%%)\n",
              cl,
              rownames(pt_data_matrix)[top_medoids[1]], 
              100 * medoid_freq_pt[top_medoids[1], cl] / B,
              rownames(pt_data_matrix)[top_medoids[2]], 
              100 * medoid_freq_pt[top_medoids[2], cl] / B,
              rownames(pt_data_matrix)[top_medoids[3]], 
              100 * medoid_freq_pt[top_medoids[3], cl] / B))
}


# --- Clergy & Sacraments Bootstrap ---

cat("\n========== Bootstrap Stability: Clergy & Sacraments ==========\n")

# Reference solution (using δ=1.345)
ref_cl_cs <- delta_results[["cs_delta_1.345"]]$clustering
k_ref_cs <- max(ref_cl_cs)

# Storage
jaccard_cs <- matrix(NA, B, k_ref_cs)
medoid_freq_cs <- matrix(0, nrow(cs_data_matrix), k_ref_cs)
rownames(medoid_freq_cs) <- rownames(cs_data_matrix)
co_assoc_cs <- matrix(0, nrow(cs_data_matrix), nrow(cs_data_matrix))

for (b in 1:B) {
  if (b %% 100 == 0) cat(sprintf("  Bootstrap replicate %d/%d\n", b, B))
  
  # Bootstrap sample
  boot_idx <- sample(1:nrow(cs_data_matrix), replace = TRUE)
  boot_data <- cs_data_matrix[boot_idx, ]
  
  # Compute Huber distance on bootstrap sample
  n_boot <- nrow(boot_data)
  boot_dist_matrix <- matrix(0, n_boot, n_boot)
  for (i in 1:(n_boot-1)) {
    for (j in (i+1):n_boot) {
      d <- huber_distance(boot_data[i, ], boot_data[j, ], delta = delta_bootstrap)
      boot_dist_matrix[i, j] <- d
      boot_dist_matrix[j, i] <- d
    }
  }
  boot_dist <- as.dist(boot_dist_matrix)
  
  # PAM with fixed k
  pam_boot <- pam(boot_dist, k = k_ref_cs, diss = TRUE)
  boot_cl <- pam_boot$clustering
  
  # Map back to original indices
  boot_cl_full <- rep(NA, nrow(cs_data_matrix))
  boot_cl_full[boot_idx] <- boot_cl
  
  # Match clusters
  boot_cl_matched <- match_clusters(ref_cl_cs, boot_cl_full)
  
  # Jaccard
  jaccard_cs[b, ] <- jaccard_per_cluster(ref_cl_cs, boot_cl_matched, boot_idx)
  
  # Medoid frequency
  boot_medoids <- boot_idx[pam_boot$id.med]
  for (i in 1:k_ref_cs) {
    medoid_freq_cs[boot_medoids[i], i] <- medoid_freq_cs[boot_medoids[i], i] + 1
  }
  
  # Co-association matrix
  for (i in 1:(nrow(cs_data_matrix)-1)) {
    for (j in (i+1):nrow(cs_data_matrix)) {
      if (!is.na(boot_cl_matched[i]) && !is.na(boot_cl_matched[j])) {
        if (boot_cl_matched[i] == boot_cl_matched[j]) {
          co_assoc_cs[i, j] <- co_assoc_cs[i, j] + 1
          co_assoc_cs[j, i] <- co_assoc_cs[j, i] + 1
        }
      }
    }
  }
}

# Normalize co-association
co_assoc_cs <- co_assoc_cs / B

# Summary statistics
jaccard_summary_cs <- data.frame(
  Cluster = 1:k_ref_cs,
  Median = apply(jaccard_cs, 2, median, na.rm = TRUE),
  Q25 = apply(jaccard_cs, 2, quantile, probs = 0.25, na.rm = TRUE),
  Q75 = apply(jaccard_cs, 2, quantile, probs = 0.75, na.rm = TRUE),
  Mean = apply(jaccard_cs, 2, mean, na.rm = TRUE)
)

cat("\nJaccard Index Summary (Clergy & Sacraments):\n")
print(round(jaccard_summary_cs, 3))

# Medoid selection frequency
cat("\nTop medoid candidates per cluster (Clergy & Sacraments):\n")
for (cl in 1:k_ref_cs) {
  top_medoids <- order(medoid_freq_cs[, cl], decreasing = TRUE)[1:3]
  cat(sprintf("Cluster %d: %s (%.1f%%), %s (%.1f%%), %s (%.1f%%)\n",
              cl,
              rownames(cs_data_matrix)[top_medoids[1]], 
              100 * medoid_freq_cs[top_medoids[1], cl] / B,
              rownames(cs_data_matrix)[top_medoids[2]], 
              100 * medoid_freq_cs[top_medoids[2], cl] / B,
              rownames(cs_data_matrix)[top_medoids[3]], 
              100 * medoid_freq_cs[top_medoids[3], cl] / B))
}


# --- Visualization: Jaccard boxplots ---

library(ggplot2)
library(reshape2)

# Population & Territory
jaccard_pt_long <- melt(jaccard_pt)
colnames(jaccard_pt_long) <- c("Replicate", "Cluster", "Jaccard")
jaccard_pt_long$Cluster <- factor(jaccard_pt_long$Cluster)

p_pt_jaccard <- ggplot(jaccard_pt_long, aes(x = Cluster, y = Jaccard)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = c(0.6, 0.75), linetype = "dashed", color = "red") +
  labs(title = "Bootstrap Jaccard Stability (Population & Territory)",
       subtitle = sprintf("B=%d replicates, δ=%.3f", B, delta_bootstrap),
       x = "Cluster", y = "Jaccard Index") +
  theme_minimal()
print(p_pt_jaccard)

# Clergy & Sacraments
jaccard_cs_long <- melt(jaccard_cs)
colnames(jaccard_cs_long) <- c("Replicate", "Cluster", "Jaccard")
jaccard_cs_long$Cluster <- factor(jaccard_cs_long$Cluster)

p_cs_jaccard <- ggplot(jaccard_cs_long, aes(x = Cluster, y = Jaccard)) +
  geom_boxplot(fill = "darkorange", alpha = 0.7) +
  geom_hline(yintercept = c(0.6, 0.75), linetype = "dashed", color = "red") +
  labs(title = "Bootstrap Jaccard Stability (Clergy & Sacraments)",
       subtitle = sprintf("B=%d replicates, δ=%.3f", B, delta_bootstrap),
       x = "Cluster", y = "Jaccard Index") +
  theme_minimal()
print(p_cs_jaccard)


# --- Visualization: Co-association heatmaps ---

library(pheatmap)

# Population & Territory
rownames(co_assoc_pt) <- colnames(co_assoc_pt) <- rownames(pt_data_matrix)
pheatmap(co_assoc_pt,
         cluster_rows = TRUE, cluster_cols = TRUE,
         color = colorRampPalette(c("white", "steelblue", "darkblue"))(100),
         main = "Co-association Matrix (Population & Territory)",
         fontsize_row = 6, fontsize_col = 6)

# Clergy & Sacraments
rownames(co_assoc_cs) <- colnames(co_assoc_cs) <- rownames(cs_data_matrix)
pheatmap(co_assoc_cs,
         cluster_rows = TRUE, cluster_cols = TRUE,
         color = colorRampPalette(c("white", "orange", "darkorange"))(100),
         main = "Co-association Matrix (Clergy & Sacraments)",
         fontsize_row = 6, fontsize_col = 6)


# ==============================================================================
# SUMMARY TABLES FOR REPORTING
# ==============================================================================

cat("\n========== SUMMARY: δ-sensitivity ==========\n")
cat("\nPopulation & Territory:\n")
print(pt_delta_comparison)
cat("\nClergy & Sacraments:\n")
print(cs_delta_comparison)

cat("\n========== SUMMARY: Bootstrap Stability ==========\n")
cat("\nPopulation & Territory:\n")
print(jaccard_summary_pt)
cat("\nClergy & Sacraments:\n")
print(jaccard_summary_cs)

# Export for paper/slides
write.csv(pt_delta_comparison, "delta_sensitivity_pt.csv", row.names = FALSE)
write.csv(cs_delta_comparison, "delta_sensitivity_cs.csv", row.names = FALSE)
write.csv(jaccard_summary_pt, "jaccard_stability_pt.csv", row.names = FALSE)
write.csv(jaccard_summary_cs, "jaccard_stability_cs.csv", row.names = FALSE)

cat("\n========== Robustness checks complete! ==========\n")