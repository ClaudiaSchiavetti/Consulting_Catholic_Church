rm(list = ls())

library(reshape2)
library(viridis)
library(ggplot2)
library(psych)
library(moments)  
library(gridExtra)
library(scales)
library(tidyr)

# Local development environment
path_data <- "C:/Users/schia/Documents/GitHub/Consulting_Catholic_Church"
#path_data <- "C:/Users/soffi/Documents/Consulting_Catholic_Church"
setwd(path_data)


#---- Create the dataset ---- 

# Read the dataset, preserving original column names with spaces
final_geo_table <- read.csv("final_geo_table.csv", sep= ";", check.names = FALSE)

# Define the list of columns to keep, using exact names with spaces
columns_to_keep <- c(
  "Region",
  "Year",
  "Region type",
  "Catholics per 100 inhabitants",
  "Catholics in thousands",
  "Area in km^2",
  "Catholics per pastoral centre",
  "Yearly ordinations of diocesan priests as share of those incardinated on January 1",
  "Yearly deaths of diocesan priests as share of those incardinated on January 1",
  "Yearly defections of diocesan priests as share of those incardinated at January 1",
  "Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1",
  "Candidates for diocesan clergy in theology centres",
  "Candidates for religious clergy in theology centres",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics",
  "Philosophy+theology candidates for diocesan and religious clergy per 100 priests",
  "Infant baptisms (people up to 7 years old)",
  "Adult baptisms (people over 7 years old)",
  "Baptisms",
  "Infant baptisms (people up to 7 years old) per 1000 Catholics",
  "Marriages between Catholics",
  "Mixed marriages",
  "Marriages",
  "Marriages per 1000 Catholics",
  "Confirmations per 1000 Catholics",
  "First Communions per 1000 Catholics"
)

# Create the subset dataframe
cluster_data <- final_geo_table[, columns_to_keep]

# Manual corrections

cluster_data[cluster_data$Region == "French Guiana", "Yearly deaths of diocesan priests as share of those incardinated on January 1"] <- 
  cluster_data[cluster_data$Region == "French Guiana", "Yearly deaths of diocesan priests as share of those incardinated on January 1"] / 100

cluster_data[cluster_data$Region == "Finland", "Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1"] <- 
  cluster_data[cluster_data$Region == "Finland", "Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1"] / 100

cluster_data[cluster_data$Region == "Sri Lanka", "Confirmations per 1000 Catholics"] <- 
  cluster_data[cluster_data$Region == "Sri Lanka", "Confirmations per 1000 Catholics"] / 100

cluster_data[cluster_data$Region == "Mali", "Confirmations per 1000 Catholics"] <- 
  8.29

# Create cluster_data_2022 by filtering rows where Year == 2022
cluster_data_2022 <- cluster_data[cluster_data$Year == 2022, ]

# Remove the Year column
cluster_data_2022 <- cluster_data_2022[, colnames(cluster_data_2022) != "Year"]


#---- Descriptive statistics ---- 

# Filter only countries for descriptive statistics
country_data <- cluster_data_2022[cluster_data_2022$`Region type` == "Country", ]

# Select only the numeric variables (exclude Region and Region type)
numeric_vars <- country_data[, !names(country_data) %in% c("Region", "Region type")]

# Summary Statistics Table

# Create comprehensive summary statistics
summary_stats <- data.frame(
  N = sapply(numeric_vars, function(x) sum(!is.na(x))),
  Mean = sapply(numeric_vars, function(x) round(mean(x, na.rm = TRUE), 2)),
  SD = sapply(numeric_vars, function(x) round(sd(x, na.rm = TRUE), 2)),
  Min = sapply(numeric_vars, function(x) round(min(x, na.rm = TRUE), 2)),
  Max = sapply(numeric_vars, function(x) round(max(x, na.rm = TRUE), 2)),
  Median = sapply(numeric_vars, function(x) round(median(x, na.rm = TRUE), 2)),
  Missing_Pct = sapply(numeric_vars, function(x) round(sum(is.na(x))/length(x)*100, 1)),
  Skewness = sapply(numeric_vars, function(x) round(skewness(x, na.rm = TRUE), 2))
)

# Sort by variable type (for better readability)
print(summary_stats)


# Histograms for Representative Variables -- made a unique code after the standardization but we can add something here if needed

#---- Standardization ---- 

# Function to apply the standardization plan
standardize_data <- function(data) {
  
  # Helper function for logit transformation with small epsilon
  logit_transform <- function(p, epsilon = 1e-6) {
    p_adjusted <- pmin(pmax(p, epsilon), 1 - epsilon)
    log(p_adjusted / (1 - p_adjusted))
  }
  
  # Create a copy of the data to avoid modifying the original
  data_std <- data
  
  # Catholics per 100 inhabitants: just z-score
  data_std[, 3] <- scale(data[, 3])
  
  # Catholics in thousands: log(x+1), then z-score
  data_std[, 4] <- scale(log(data[, 4] + 1))
  
  # Area in km²: log(x), then z-score
  data_std[, 5] <- scale(log(data[, 5]))
  
  # Catholics per pastoral centre: log(x+1), then z-score
  data_std[, 6] <- scale(log(data[, 6] + 1))
  
  # Yearly ordinations (%): divide by 100, logit, then z-score
  data_std[, 7] <- scale(logit_transform(data[, 7] / 100))
  
  # Yearly deaths (%): divide by 100, logit, then z-score
  data_std[, 8] <- scale(logit_transform(data[, 8] / 100))
  
  # Yearly defections (%): divide by 100, logit, then z-score
  data_std[, 9] <- scale(logit_transform(data[, 9] / 100))
  
  # Yearly ordinations - deaths - defections (%): only z-score (can be negative)
  data_std[, 10] <- scale(data[, 10])
  
  # Candidates for diocesan clergy: log(x+1), then z-score
  data_std[, 11] <- scale(log(data[, 11] + 1))
  
  # Candidates for religious clergy: log(x+1), then z-score
  data_std[, 12] <- scale(log(data[, 12] + 1))
  
  # Vocation rate per 100k inhabitants: log(x+1), then z-score
  data_std[, 13] <- scale(log(data[, 13] + 1))
  
  # Vocation rate per 100k Catholics: log(x+1), then z-score
  data_std[, 14] <- scale(log(data[, 14] + 1))
  
  # Candidates per 100 priests: z-score only (moderate skew)
  data_std[, 15] <- scale(data[, 15])
  
  # Infant baptisms (absolute): log(x+1), then z-score
  data_std[, 16] <- scale(log(data[, 16] + 1))
  
  # Adult baptisms (absolute): log(x+1), then z-score
  data_std[, 17] <- scale(log(data[, 17] + 1))
  
  # Baptisms (absolute): log(x+1), then z-score
  data_std[, 18] <- scale(log(data[, 18] + 1))
  
  # Infant baptisms per 1000 Catholics: log(x+1), then z-score
  data_std[, 19] <- scale(log(data[, 19] + 1))
  
  # Marriages between Catholics: log(x+1), then z-score
  data_std[, 20] <- scale(log(data[, 20] + 1))
  
  # Mixed marriages: log(x+1), then z-score
  data_std[, 21] <- scale(log(data[, 21] + 1))
  
  # Marriages (absolute): log(x+1), then z-score
  data_std[, 22] <- scale(log(data[, 22] + 1))
  
  # Marriages per 1000 Catholics: z-score only (mild skew)
  data_std[, 23] <- scale(data[, 23])
  
  # Confirmations per 1000 Catholics: log(x+1), then z-score
  data_std[, 24] <- scale(log(data[, 24] + 1))
  
  # First Communions per 1000 Catholics: log(x+1), then z-score
  data_std[, 25] <- scale(log(data[, 25] + 1))
  
  return(data_std)
}

# Apply the standardization
cluster_data_2022_std <- standardize_data(cluster_data_2022)

# Verify the transformation
summary(cluster_data_2022_std[, 3:25])

#Plot

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

# Create plots for the four specified variables

# 1. Catholics per 100 inhabitants (Column 3) - Z-score only
plot1 <- create_comparison_plot(
  cluster_data_2022, cluster_data_2022_std, 3,
  "Catholics per 100 inhabitants", "Z-score only"
)

# 2. Catholics in thousands (Column 4) - log(x+1) → z-score
plot2 <- create_comparison_plot(
  cluster_data_2022, cluster_data_2022_std, 4,
  "Catholics in thousands", "log(x+1) → z-score"
)

# 3. Yearly ordinations of diocesan priests (Column 7) - Proportion → logit → z-score
plot3 <- create_comparison_plot(
  cluster_data_2022, cluster_data_2022_std, 7,
  "Yearly ordinations of diocesan priests (%)", "Proportion → logit → z-score"
)

# 4. Area in km² (Column 5) - log(x) → z-score
plot4 <- create_comparison_plot(
  cluster_data_2022, cluster_data_2022_std, 5,
  "Area in km²", "log(x) → z-score"
)

# Arrange all plots in a grid
grid.arrange(
  plot1, plot2, plot3, plot4,
  ncol = 2, nrow = 2,
  top = "Distribution Comparison: Before and After Standardization"
)


#---- Analysis of the missing values ---- 
# Number of missing values per variable (column)
missing_per_variable <- colSums(is.na(cluster_data_2022))
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

# Number of missing values per each "Region" value
# First, add a temporary column for row-wise missing count
cluster_data_2022$missing_count <- rowSums(is.na(cluster_data_2022))

# Then, aggregate by Region
missing_per_region <- aggregate(missing_count ~ Region, data = cluster_data_2022, FUN = sum)

# Remove the temporary column
cluster_data_2022$missing_count <- NULL

print("Missing values per Region:")
print(missing_per_region)

# Heatmap

# Filter only rows where Region type equals "Country"
country_data <- cluster_data_2022[cluster_data_2022$`Region type` == "Country", ]

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

# Percentage of missing data per country

# Get countries with missing values
rows_with_missing_idx <- rowSums(is.na(country_data)) > 0
missing_summary <- country_data[rows_with_missing_idx, c("Region", "Region type")]
names(missing_summary)[1] <- "Country"  # Rename Region to Country for clarity

# Calculate missing counts
missing_summary$Missing_Count <- rowSums(is.na(country_data[rows_with_missing_idx, ]))
missing_summary$Missing_Percentage <- round(
  (missing_summary$Missing_Count / 
     (ncol(country_data) - 2)) * 100, 1)  # -2 for Region and Region type

print("Countries with missing values:")
print(missing_summary[order(-missing_summary$Missing_Count), ])

# Plot
if(nrow(missing_summary) > 0) {
  p3 <- ggplot(missing_summary, aes(x = reorder(Country, Missing_Percentage), 
                                    y = Missing_Percentage, fill = Missing_Percentage)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis(option = "D") +
    coord_flip() +
    labs(title = "Percentage of Missing Data per Country",
         x = "Country",
         y = "% of Variables Missing",
         fill = "% Missing") +
    theme_minimal() +
    geom_text(aes(label = paste0(Missing_Percentage, "%")), hjust = -0.2, size = 3) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.7) +
    annotate("text", x = nrow(missing_summary), y = 50, 
             label = "50% threshold", vjust = -0.5, color = "red")
  
  print(p3)
}


# Remove 

# Remove countries with high percentage of missing values
countries_to_remove <- c("Dem. Peoples Rep. Of Korea", "China (Mainland)")

# Filter out these countries from cluster_data_2022
cluster_data_2022 <- cluster_data_2022[!cluster_data_2022$Region %in% countries_to_remove, ]

# Also filter from cluster_data_2022_std (the standardized version)
cluster_data_2022_std <- cluster_data_2022_std[!cluster_data_2022_std$Region %in% countries_to_remove, ]




