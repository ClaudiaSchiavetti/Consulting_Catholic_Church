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
library(ggdendro)
library(plotly)
library(NbClust)
library(cluster)
library(factoextra)

# Set working directory
path_data <- "C:/Users/schia/Documents/GitHub/Consulting_Catholic_Church"
#path_data <- "C:/Users/soffi/Documents/Consulting_Catholic_Church"
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

# Summary with tidyverse
missing_summary_tidy <- missing_info_tidy %>%
  count(Region, Variable, name = "Missing_Count")

print(missing_summary_tidy, n = nrow(missing_summary_tidy))


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

# ---- Variable Mutations and Feature Engineering ----

# Perform mutations and select only the final desired columns
analysis_data <- country_data %>%
  mutate(
    `Catholics per km^2` = (`Catholics in thousands` * 1000) / `Area in km^2`,
    `Share of diocesan pastors` = `Parishes with diocesan pastor` / (`Parishes with diocesan pastor` + `Parishes with religious pastor`),
    `Non-vacant parishes administered by non-pastor priests per Catholic` = `Parishes without pastor administered by another priest` / (`Catholics in thousands` * 1000),
    `Share of non-vacant parishes entrusted to religious women or laypeople` = (`Parishes without pastor entrusted to religious women` + `Parishes without pastor entrusted to laypeople`) /
      (`Parishes with diocesan pastor` + `Parishes with religious pastor` + `Parishes without pastor administered by another priest` +
         `Parishes without pastor entrusted to permanent deacons` + `Parishes without pastor entrusted to non-priest religious men` +
         `Parishes without pastor entrusted to religious women` + `Parishes without pastor entrusted to laypeople`),
    `Parishes entirely vacant per Catholic` = `Parishes entirely vacant` / (`Catholics in thousands` * 1000),
    `Candidates for diocesan clergy in theology centres per Catholic` = `Candidates for diocesan clergy in theology centres` / (`Catholics in thousands` * 1000),
    `Candidates for religious clergy in theology centres per Catholic` = `Candidates for religious clergy in theology centres` / (`Catholics in thousands` * 1000),
    `Infant baptisms (people up to 7 years old) per Catholic` = `Infant baptisms (people up to 7 years old)` / (`Catholics in thousands` * 1000),
    `Adult baptisms (people over 7 years old) per Catholic` = `Adult baptisms (people over 7 years old)` / (`Catholics in thousands` * 1000),
    `Baptisms per inhabitant` = `Baptisms` / (`Inhabitants in thousands` * 1000),
    `Marriages between Catholics per Catholic` = `Marriages between Catholics` / (`Catholics in thousands` * 1000),
    `Mixed marriages per inhabitant` = `Mixed marriages` / (`Inhabitants in thousands` * 1000),
    `Share of mixed marriages` = `Mixed marriages per inhabitant` / (`Marriages between Catholics per Catholic` + `Mixed marriages per inhabitant`),
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

# Apply standardization
cluster_data_2022_std <- standardize_data(analysis_data)

# Verify transformations with summary (numeric columns only)
summary(cluster_data_2022_std %>% select(-Region))


# ---- Transformation diagnostics ----

# Define sigmoid function for logit inverse
sigmoid <- function(y) {
  1 / (1 + exp(-y))
}

# Create back-transformed dataset directly from standardized data
back_transformed_data <- cluster_data_2022_std %>%
  mutate(
    # Logit inverses: simple sigmoid, then scale back to original range
    `Catholics per 100 inhabitants_back` = sigmoid(`Share of Catholics`) * 100,
    `Share of diocesan pastors_back` = sigmoid(`Share of diocesan pastors`),
    `Share of non-vacant parishes entrusted to religious women or laypeople_back` = sigmoid(`Share of non-vacant parishes entrusted to religious women or laypeople`),
    `Yearly ordinations of diocesan priests as share of those incardinated on January 1_back` = sigmoid(`Yearly ordinations of diocesan priests as share of those incardinated on January 1`) * 100,
    `Yearly deaths of diocesan priests as share of those incardinated on January 1_back` = sigmoid(`Yearly deaths of diocesan priests as share of those incardinated on January 1`) * 100,
    `Yearly defections of diocesan priests as share of those incardinated at January 1_back` = sigmoid(`Yearly defections of diocesan priests as share of those incardinated at January 1`) * 100,
    `Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants_back` = sigmoid(`Vocation rate - philosophy+theology candidates for diocesan and religious clergy per inhabitant`) * 100000,
    `Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics_back` = sigmoid(`Vocation rate - philosophy+theology candidates for diocesan and religious clergy per Catholic`) * 100000,
    `Baptisms per inhabitant_back` = sigmoid(`Baptisms per inhabitant`),
    `Share of mixed marriages_back` = sigmoid(`Share of mixed marriages`),
    
    # Log inverses: simple exp(y), with scaling where applicable (no epsilon subtraction)
    `Catholics per km^2_back` = exp(`Catholics per km^2`),
    `Catholics per pastoral centre_back` = exp(`Catholics per pastoral centre`),
    `Non-vacant parishes administered by non-pastor priests per Catholic_back` = exp(`Non-vacant parishes administered by non-pastor priests per Catholic`),
    `Parishes entirely vacant per Catholic_back` = exp(`Parishes entirely vacant per Catholic`),
    `Catholics per priest_back` = exp(`Catholics per priest`),
    `Philosophy+theology candidates for diocesan and religious clergy per 100 priests_back` = exp(`Philosophy+theology candidates for diocesan and religious clergy per priest`) / 100, # Since forward was log(*100)
    `Candidates for diocesan clergy in theology centres per Catholic_back` = exp(`Candidates for diocesan clergy in theology centres per Catholic`),
    `Candidates for religious clergy in theology centres per Catholic_back` = exp(`Candidates for religious clergy in theology centres per Catholic`),
    `Infant baptisms (people up to 7 years old) per Catholic_back` = exp(`Infant baptisms (people up to 7 years old) per Catholic`),
    `Adult baptisms (people over 7 years old) per Catholic_back` = exp(`Adult baptisms (people over 7 years old) per Catholic`),
    `Marriages between Catholics per Catholic_back` = exp(`Marriages between Catholics per Catholic`),
    `Mixed marriages per inhabitant_back` = exp(`Mixed marriages per inhabitant`),
    `Confirmations per Catholic_back` = exp(`Confirmations per Catholic`),
    `First Communions per Catholic_back` = exp(`First Communions per Catholic`)
  ) %>%
  select(Region, ends_with("_back"))

# Print summary of back-transformed data for verification
summary(back_transformed_data %>% select(-Region))

# Assuming same row order, add RowID
analysis_data$RowID <- 1:nrow(analysis_data)
back_transformed_data$RowID <- 1:nrow(back_transformed_data)

# Join by RowID and Region for safety
plot_data <- inner_join(analysis_data, back_transformed_data, by = c("Region", "RowID")) %>%
  select(-RowID)

# List of variable names (original)
var_names <- c(
  "Catholics per 100 inhabitants",
  "Catholics per km^2",
  "Catholics per pastoral centre",
  "Share of diocesan pastors",
  "Non-vacant parishes administered by non-pastor priests per Catholic",
  "Share of non-vacant parishes entrusted to religious women or laypeople",
  "Parishes entirely vacant per Catholic",
  "Catholics per priest",
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

# Corresponding back names
back_names <- paste0(var_names, "_back")

# Create scatterplots
plots <- list()
for (i in seq_along(var_names)) {
  orig_col <- var_names[i]
  back_col <- back_names[i]
  
  # Use complete cases for plotting
  temp_data <- plot_data %>%
    select(all_of(c(orig_col, back_col, "Region"))) %>%
    filter(complete.cases(select(., -Region)))
  
  if (nrow(temp_data) > 0) {
    p <- ggplot(temp_data, aes(x = .data[[orig_col]], y = .data[[back_col]])) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + # y = x line for reference
      labs(title = str_wrap(paste("Original vs Back-Transformed:", orig_col), width = 40),
           x = "Original", y = "Back-Transformed") +
      theme_minimal()
    
    plots[[i]] <- p
  }
}

# Display plots in six different 2x2 grids
grid.arrange(grobs = plots[1:4], ncol = 2, top = "Grid 1")
grid.arrange(grobs = plots[5:8], ncol = 2, top = "Grid 2")
grid.arrange(grobs = plots[9:12], ncol = 2, top = "Grid 3")
grid.arrange(grobs = plots[13:16], ncol = 2, top = "Grid 4")
grid.arrange(grobs = plots[17:20], ncol = 2, top = "Grid 5")
grid.arrange(grobs = plots[21:24], ncol = 2, top = "Grid 6")


# ---- Correlation Analysis and Visualization ----

# Compute correlation matrix (pairwise complete for NAs)
numeric_std <- cluster_data_2022_std %>% select(-Region)
cor_matrix <- cor(numeric_std, use = "pairwise.complete.obs")

# Melt for plotting
melted_cor <- melt(cor_matrix)

# Create correlation heatmap
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "black", mid = "white", high = "black", midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = ifelse(abs(value) > 0.8, round(value, 2), "")), color = "white", size = 3) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_fixed() +
  labs(title = "Correlation Heatmap of Standardized Variables")


# ---- Density and skewness diagnostics [not yet modified] ----

# Define the transformation groups and choose one representative variable from each:
# Group 1: Z-score only (bounded or mild skew)
# Group 2: Log(x+1) then z-score (count data with high skew)
# Group 3: Logit then z-score (percentage data)
# Group 4: Log(x) then z-score (area data, no zeros)

# Function to create before/after comparison with density and skewness
create_comparison_plot <- function(data_before, data_after, col_index, var_name, transformation) {
  
  # Prepare data
  df <- data.frame(
    Before = data_before[, col_index],
    After = as.vector(data_after[, col_index])
  )
  
  # Remove any infinite or NA values
  df <- df[is.finite(df$Before) & is.finite(df$After), ]
  
  # Calculate skewness
  skew_before <- round(skewness(df$Before, na.rm = TRUE), 2)
  skew_after <- round(skewness(df$After, na.rm = TRUE), 2)
  
  # Reshape for ggplot
  df_long <- pivot_longer(df, cols = c(Before, After), 
                          names_to = "Stage", values_to = "Value")
  
  # Set factor levels to control order (Before first)
  df_long$Stage <- factor(df_long$Stage, levels = c("Before", "After"))
  
  # Create density + histogram comparison
  p <- ggplot(df_long, aes(x = Value, fill = Stage)) +
    geom_histogram(aes(y = after_stat(density)), alpha = 0.5, 
                   position = "identity", bins = 30) +
    geom_density(alpha = 0.3, linewidth = 1) +
    facet_wrap(~Stage, scales = "free") +
    scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.3, end = 0.8) +
    scale_x_continuous(labels = label_comma(accuracy = 0.01)) +
    scale_y_continuous(labels = label_comma(accuracy = 0.001)) +
    labs(title = var_name,
         subtitle = paste0("Transformation: ", transformation,
                           "\nSkewness Before: ", skew_before, 
                           "  |  Skewness After: ", skew_after),
         x = "Value", y = "Density") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "bold"),
          plot.subtitle = element_text(size = 8.5, color = "gray40"))
  
  return(p)
}


# ---- Imputation of NaNs ----

zscores <- cluster_data_2022_std %>%
  mutate(across(where(is.numeric), ~ (.-mean(., na.rm=TRUE))/sd(., na.rm=TRUE)))
summary(zscores)

numeric_z <- zscores %>% select(-Region)

# Perform KNN imputation: k=15 nearest neighbors, using Euclidean distance (default in VIM::kNN).
# It computes distances based on available (non-NA) values, scaling them appropriately.
# Set imp_var=FALSE to avoid adding imputation indicator columns.
imputed_numeric <- kNN(numeric_z, k = 15, dist_var = colnames(numeric_z), imp_var = FALSE)

# Combine the imputed numeric data with the original 'Region' column
imputed_z <- data.frame(Region = zscores$Region, imputed_numeric)

# View the first few rows to check
colnames(imputed_z) <- colnames(zscores)
head(imputed_z)


# ---- Cluster analysis: population and territory  ----

# Define list of variables
population_territory <- c("Share of Catholics",
                          "Catholics per km^2",
                          "Catholics per pastoral centre",
                          "Share of diocesan pastors",
                          "Non-vacant parishes administered by non-pastor priests per Catholic",
                          "Share of non-vacant parishes entrusted to religious women or laypeople",
                          "Parishes entirely vacant per Catholic",
                          "Catholics per priest")

# Extract numeric variables
popu_terr <- imputed_z %>% select(all_of(population_territory))

# Convert to matrix for easier computation
pt_data_matrix <- as.matrix(popu_terr)

# Define the Huber loss function
huber_loss <- function(e, delta = 1.345) {  # delta=1.345 is a common choice for 95% efficiency
  ifelse(abs(e) <= delta, 0.5 * e^2, delta * abs(e) - 0.5 * delta^2)
}

# Define the Huber distance function between two vectors
huber_distance <- function(x, y, delta = 1.345) {
  diffs <- x - y
  sqrt(sum(huber_loss(diffs, delta)))
}

# Compute the pairwise Huber distance matrix
# Note: This uses a loop for simplicity; for large n (>1000 rows), consider parallelization or optimization
n <- nrow(pt_data_matrix)
pt_dist_matrix <- matrix(0, n, n)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    pt_dist_matrix[i, j] <- huber_distance(pt_data_matrix[i, ], pt_data_matrix[j, ])
    pt_dist_matrix[j, i] <- pt_dist_matrix[i, j]  # Symmetric
  }
}

# Convert to dist object
pt_huber_dist <- as.dist(pt_dist_matrix)

# Perform hierarchical clustering with Ward's method
# Use "ward.D2" for the unsquared version (recommended for distance matrices)
pt_hc <- hclust(pt_huber_dist, method = "ward.D2")

# Plot the dendrogram
plot(pt_hc, 
     labels = imputed_z$Region, 
     main = "Clustering according to population and territory",
     xlab = "Countries", 
     sub = NULL, 
     horiz = TRUE, 
     cex = 0.4)  # Smaller label size; try 0.3-0.6 based on your display

# Interactive dendrogram
pt_ggdend <- ggdendrogram(pt_hc, rotate = TRUE, size = 2) + 
  theme(axis.text.x = element_text(size = 6, angle = 90))  # Rotate and small text

pt_interactive_dend <- ggplotly(pt_ggdend)
pt_interactive_dend

# CH index (variance ratio criterion) for k choice
pt_nb_res <- NbClust(pt_data_matrix, diss = pt_huber_dist, 
                     distance = NULL, min.nc = 2, max.nc = 15, 
                     method = "ward.D2", index = "ch")  # Or "all" for 30 indices
pt_nb_res$Best.nc  # Optimal k by CH

# Silhouette method for k choice
fviz_nbclust(imputed_z[, population_territory],
             FUNcluster = function(x, k) list(cluster = cutree(pt_hc, k = k)),  # Wrap in list(cluster = ...)
             method = "silhouette", k.max = 15,
             diss = pt_huber_dist) + 
  labs(title = "Silhouette Method for Optimal k")

# k=2 is optimal for both methods

# For validation, compute cophenetic correlation to check how well the dendrogram preserves distances
pt_coph_cor <- cor(pt_huber_dist, cophenetic(pt_hc))
print(paste("Cophenetic Correlation:", round(pt_coph_cor, 2)))  # Closer to 1 is better


# ---- Cluster analysis: clergy and sacraments  ----

# Define list of variables
clergy_sacraments <- c("Yearly ordinations of diocesan priests as share of those incardinated on January 1",
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
                       "First Communions per Catholic")

# Extract numeric variables
numeric_imp <- imputed_z %>% select(all_of(clergy_sacraments))
colnames(numeric_imp) <- clergy_sacraments  # exclude "Region"

# Convert to matrix for easier computation
cs_data_matrix <- as.matrix(numeric_imp)

# Define the Huber loss function
huber_loss <- function(e, delta = 1.345) {  # delta=1.345 is a common choice for 95% efficiency
  ifelse(abs(e) <= delta, 0.5 * e^2, delta * abs(e) - 0.5 * delta^2)
}

# Define the Huber distance function between two vectors
huber_distance <- function(x, y, delta = 1.345) {
  diffs <- x - y
  sqrt(sum(huber_loss(diffs, delta)))
}

# Compute the pairwise Huber distance matrix
# Note: This uses a loop for simplicity; for large n (>1000 rows), consider parallelization or optimization
n <- nrow(cs_data_matrix)
cs_dist_matrix <- matrix(0, n, n)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    cs_dist_matrix[i, j] <- huber_distance(cs_data_matrix[i, ], cs_data_matrix[j, ])
    cs_dist_matrix[j, i] <- cs_dist_matrix[i, j]  # Symmetric
  }
}

# Convert to dist object
cs_huber_dist <- as.dist(cs_dist_matrix)

# Perform hierarchical clustering with Ward's method
# Use "ward.D2" for the unsquared version (recommended for distance matrices)
cs_hc <- hclust(cs_huber_dist, method = "ward.D2")

# Plot the dendrogram
plot(cs_hc, 
     labels = imputed_z$Region,
     main = "Clustering according to clergy and sacraments",
     xlab = "Regions", 
     sub = NULL, 
     hang = -1, 
     cex = 0.4)  # Smaller label size; try 0.3-0.6 based on your display

# Interactive dendrogram
cs_ggdend <- ggdendrogram(cs_hc, rotate = TRUE, size = 2) + 
  theme(axis.text.x = element_text(size = 6, angle = 90))  # Rotate and small text

cs_interactive_dend <- ggplotly(cs_ggdend)
cs_interactive_dend

# CH index (variance ratio criterion) for k choice
cs_nb_res <- NbClust(cs_data_matrix, diss = cs_huber_dist, 
                     distance = NULL, min.nc = 2, max.nc = 15, 
                     method = "ward.D2", index = "ch")  # Or "all" for 30 indices
cs_nb_res$Best.nc  # Optimal k by CH

# Silhouette method for k choice
fviz_nbclust(imputed_z[, clergy_sacraments],
             FUNcluster = function(x, k) list(cluster = cutree(cs_hc, k = k)),  # Wrap in list(cluster = ...)
             method = "silhouette", k.max = 15,
             diss = cs_huber_dist) + 
  labs(title = "Silhouette Method for Optimal k")

# k=2 is optimal for both methods

# For validation, compute cophenetic correlation to check how well the dendrogram preserves distances
cs_coph_cor <- cor(cs_huber_dist, cophenetic(cs_hc))
print(paste("Cophenetic Correlation:", round(cs_coph_cor, 2)))  # Closer to 1 is better

