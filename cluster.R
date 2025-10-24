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

# Create country_data by filtering rows where Year == 2022
country_data <- cluster_data[cluster_data$Year == 2022, ]

# Remove the Year column
country_data <- country_data[, colnames(country_data) != "Year"]

# Filter only countries for descriptive statistics
country_data <- country_data[country_data$`Region type` == "Country", ]


#---- Descriptive statistics ---- 

# Select only the numeric variables (exclude Region type)
analysis_data <- country_data[, !names(country_data) %in% c("Region type")]

# Create comprehensive summary statistics
summary_stats <- data.frame(
  N = sapply(analysis_data[,-1], function(x) sum(!is.na(x))),
  Mean = sapply(analysis_data[,-1], function(x) round(mean(x, na.rm = TRUE), 2)),
  SD = sapply(analysis_data[,-1], function(x) round(sd(x, na.rm = TRUE), 2)),
  Min = sapply(analysis_data[,-1], function(x) round(min(x, na.rm = TRUE), 2)),
  Max = sapply(analysis_data[,-1], function(x) round(max(x, na.rm = TRUE), 2)),
  Median = sapply(analysis_data[,-1], function(x) round(median(x, na.rm = TRUE), 2)),
  Missing_Pct = sapply(analysis_data[,-1], function(x) round(sum(is.na(x))/length(x)*100, 1)),
  Skewness = sapply(analysis_data[,-1], function(x) round(skewness(x, na.rm = TRUE), 2))
)

# Sort by variable type (for better readability)
print(summary_stats)

## Apply the desired variable mutations

# Catholics per km^2: divide col 3 by col 4
analysis_data$`Catholics per km^2` <- analysis_data[, 3] * 1000 / analysis_data[, 4]

# Share of diocesan pastors
# analysis_data[, 6] <- analysis_data[, 6] / (analysis_data[, 6] + analysis_data[, 7])
# colnames(analysis_data)[6] <- "Share of diocesan pastors"
# 
# # Share of parishes administered by priests
# analysis_data[, 8] <- (analysis_data[, 6] + analysis_data[, 7] + 
#                         analysis_data[, 8]) / (analysis_data[, 6] + 
#                         analysis_data[, 7] + analysis_data[, 8] +
#                         analysis_data[, 9] + analysis_data[, 10] +
#                         analysis_data[, 11] + analysis_data[, 12])
# colnames(analysis_data)[8] <- "Share of parishes administered by priests"

# totals
tot_all <- rowSums(analysis_data[, 6:13], na.rm = TRUE)     # includes 'entirely vacant'
tot_with_person <- rowSums(analysis_data[, 6:12], na.rm = TRUE)

# Share of diocesan pastors among parishes that have a pastor
analysis_data$`Share of diocesan pastors` <-
  analysis_data$`Parishes with diocesan pastor` /
  (analysis_data$`Parishes with diocesan pastor` + analysis_data$`Parishes with religious pastor`)

# Share of parishes administered by priests (of ALL parishes)
analysis_data$`Share of parishes administered by priests` <-
  (analysis_data$`Parishes with diocesan pastor` +
     analysis_data$`Parishes with religious pastor` +
     analysis_data$`Parishes without pastor administered by another priest`) / tot_all

# Share of parishes among all pastoral workers (exclude vacant)
analysis_data$`Share priests+deacons among pastoral workers` <-
  (analysis_data$`Parishes with diocesan pastor` +
     analysis_data$`Parishes with religious pastor` +
     analysis_data$`Parishes without pastor administered by another priest` +
     analysis_data$`Parishes without pastor entrusted to permanent deacons`) / tot_with_person

# Adult baptisms shares
analysis_data$`Adult baptisms share of all baptisms` <-
  analysis_data$`Adult baptisms (people over 7 years old)` /
  analysis_data$`Baptisms`

analysis_data$`Adult baptisms share of Catholics` <-
  analysis_data$`Adult baptisms (people over 7 years old)` /
  (analysis_data$`Catholics in thousand` * 1000)

# Wedding proportions
analysis_data$`Share of Catholic-Catholic marriages` <-
  analysis_data$`Marriages between Catholics` /
  analysis_data$`Marriages`

analysis_data$`Share of mixed marriages` <-
  analysis_data$`Mixed marriages` /
  analysis_data$`Marriages`

#TO DROP
clustering_data <- analysis_data[, -c(3, 4, 6:13, 22:26, 28:30, 39)]

## TO NORMALIZE BEFORE COMPUTING Z-SCORE [numbered after dropping the prev. cols!!!]: 
## columns 7,8,17-21,23-25

# Histograms for Representative Variables -- made a unique code after the standardization but we can add something here if needed

#---- Standardization ---- 

# Fixed epsilon transformations
log_transform <- function(x) {
  epsilon <- 1e-6
  log(x + epsilon)
}

logit_transform <- function(p) {
  epsilon <- 1e-6
  p_adjusted <- pmin(pmax(p, epsilon), 1 - epsilon)
  log(p_adjusted / (1 - p_adjusted))
}

standardize_data <- function(data) {
  
  data_std <- data
  
  # ========================================================================
  # GROUP 1: PROPORTIONS/SHARES (0-1) → Logit + Scale
  # ========================================================================
  
  # Column 2: Catholics per 100 inhabitants → rescale, logit, scale
  data_std[, 2] <- scale(logit_transform(data[, 2] / 100))
  
  # Column 5: Yearly ordinations share → rescale, logit, scale
  data_std[, 5] <- scale(logit_transform(data[, 5] / 100))
  
  # Column 6: Yearly deaths share → rescale, logit, scale
  data_std[, 6] <- scale(logit_transform(data[, 6] / 100))
  
  # Column 7: Yearly defections share → rescale, logit, scale
  data_std[, 7] <- scale(logit_transform(data[, 7] / 100))
  
  # Column 17: Share of diocesan pastors → logit, scale
  data_std[, 17] <- scale(logit_transform(data[, 17]))
  
  # Column 18: Share of parishes administered by priests → logit, scale
  data_std[, 18] <- scale(logit_transform(data[, 18]))
  
  # Column 19: Share priests+deacons → logit, scale
  data_std[, 19] <- scale(logit_transform(data[, 19]))
  
  # Column 20: Adult baptisms share → logit, scale
  data_std[, 20] <- scale(logit_transform(data[, 20]))
  
  # Column 21: Share of Catholic-Catholic marriages → logit, scale
  data_std[, 21] <- scale(logit_transform(data[, 21]))
  
  # Column 22: Share of mixed marriages → logit, scale
  data_std[, 22] <- scale(logit_transform(data[, 22]))
  
  # ========================================================================
  # GROUP 2: RATES (very small) → Logit + Scale
  # ========================================================================
  
  # Column 9: Vocation rate per 100k inhabitants → rescale, logit, scale
  data_std[, 9] <- scale(logit_transform(data[, 9] / 100000))
  
  # Column 10: Vocation rate per 100k Catholics → rescale, logit, scale
  data_std[, 10] <- scale(logit_transform(data[, 10] / 100000))
  
  # ========================================================================
  # GROUP 3: RATIOS/DENSITIES (highly skewed) → Log + Scale
  # ========================================================================
  
  # Column 3: Catholics per pastoral centre → log, scale
  data_std[, 3] <- scale(log_transform(data[, 3]))
  
  # Column 4: Catholics per priest → log, scale
  data_std[, 4] <- scale(log_transform(data[, 4]))
  
  # Column 12: Infant baptisms per 1000 Catholics → log, scale
  data_std[, 12] <- scale(log_transform(data[, 12]))
  
  # Column 13: Marriages per 1000 Catholics → log, scale
  data_std[, 13] <- scale(log_transform(data[, 13]))
  
  # Column 14: Confirmations per 1000 Catholics → log, scale
  data_std[, 14] <- scale(log_transform(data[, 14]))
  
  # Column 15: First Communions per 1000 Catholics → log, scale
  data_std[, 15] <- scale(log_transform(data[, 15]))
  
  # Column 16: Catholics per km^2 → log, scale
  data_std[, 16] <- scale(log_transform(data[, 16]))
  
  # ========================================================================
  # GROUP 4: MODERATELY SKEWED → Just Scale
  # ========================================================================
  
  # Column 11: Philosophy+theology candidates per 100 priests → scale only
  data_std[, 11] <- scale(data[, 11])
  
  # ========================================================================
  # GROUP 5: CONTAINS NEGATIVES → Rescale + Scale
  # ========================================================================
  
  # Column 8: Ordinations minus deaths/defections → rescale, scale
  data_std[, 8] <- scale(data[, 8] / 100)
  
  return(data_std)
}

# Apply standardization
clustering_data_std <- standardize_data(clustering_data)
summary(clustering_data_std)

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




