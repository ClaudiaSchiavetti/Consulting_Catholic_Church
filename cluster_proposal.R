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
#path_data <- "C:/Users/schia/Documents/GitHub/Consulting_Catholic_Church"
path_data <- "C:/Users/soffi/Documents/Consulting_Catholic_Church"
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
  "First Communions per 1000 Catholics",
  "Inhabitants in thousands"
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

# Remove countries with high percentage of missing values
countries_to_remove <- c("Dem. Peoples Rep. Of Korea", "China (Mainland)")

# Filter out these countries from cluster_data_2022
country_data <- country_data[!country_data$Region %in% countries_to_remove, ]


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
analysis_data[, 4] <- (analysis_data[, 3] * 1000) / analysis_data[, 4]
colnames(analysis_data)[4] <- "Catholics per km^2"

# Share of diocesan pastors (historical rootedness of the Church?)
analysis_data[, 6] <- analysis_data[, 6] / (analysis_data[, 6] + analysis_data[, 7])
colnames(analysis_data)[6] <- "Share of diocesan pastors"

# Non-vacant parishes administered by priests per Catholic (clergy capabilities?)
analysis_data[, 8] <- (analysis_data[, 6] + analysis_data[, 7] + 
                         analysis_data[, 8]) / (analysis_data[, 3] * 1000)
colnames(analysis_data)[8] <- "Non-vacant parishes administered by priests per Catholic"

# Share of non-vacant parishes without priest entrusted to laypeople (rootedness of democratic values?)
analysis_data[, 12] <- analysis_data[, 12] / (analysis_data[, 9] + analysis_data[, 10] + 
                                                analysis_data[, 11] + analysis_data[, 12])
colnames(analysis_data)[12] <- "Share of non-vacant parishes without priest entrusted to laypeople"

# Parishes entirely vacant per Catholic (decline of religious practice?)
analysis_data[, 13] <- analysis_data[, 13] / (analysis_data[, 3] * 1000)
colnames(analysis_data)[13] <- "Parishes entirely vacant per Catholic"

# Candidates for diocesan clergy in theology centres: normalize per Catholic
analysis_data[, 22] <- analysis_data[, 22] / (analysis_data[, 3] * 1000)
colnames(analysis_data)[22] <- "Candidates for diocesan clergy in theology centres per Catholic"

# Candidates for religious clergy in theology centres: normalize per Catholic
analysis_data[, 23] <- analysis_data[, 23] / (analysis_data[, 3] * 1000)
colnames(analysis_data)[23] <- "Candidates for religious clergy in theology centres per Catholic"

# Infant baptisms (people up to 7 years old): normalize per Catholic (familial transmission of faith?)
analysis_data[, 24] <- analysis_data[, 24] / (analysis_data[, 3] * 1000)
colnames(analysis_data)[24] <- "Infant baptisms (people up to 7 years old) per Catholic"

# Adult baptisms (people over 7 years old): normalize per Catholic (efficacy of evangelization?)
analysis_data[, 25] <- analysis_data[, 25] / (analysis_data[, 3] * 1000)
colnames(analysis_data)[25] <- "Adult baptisms (people over 7 years old) per Catholic"

# Baptisms: normalize per inhabitant (Church growth?)
analysis_data[, 26] <- analysis_data[, 26] / (analysis_data[, 34] * 1000)
colnames(analysis_data)[26] <- "Baptisms per inhabitant"

# Marriages between Catholics: normalize per Catholic (piety of Catholic couples?)
analysis_data[, 28] <- analysis_data[, 28] / (analysis_data[, 3] * 1000)
colnames(analysis_data)[28] <- "Marriages between Catholics per Catholic"

# Mixed marriages: normalize per inhabitant (marital evangelization potential?)
analysis_data[, 29] <- analysis_data[, 29] / (analysis_data[, 34] * 1000)
colnames(analysis_data)[29] <- "Mixed marriages per inhabitant"

# Share of mixed marriages (religious diversity/secularization/Church growth???)
analysis_data[, 30] <- analysis_data[, 29] / (analysis_data[, 28] + analysis_data[, 29])
colnames(analysis_data)[30] <- "Share of mixed marriages"

# Confirmations per Catholic
analysis_data[, 32] <- analysis_data[, 32] / (analysis_data[, 3] * 1000)
colnames(analysis_data)[32] <- "Confirmations per Catholic"

# First Communions per Catholic
analysis_data[, 33] <- analysis_data[, 33] / (analysis_data[, 3] * 1000)
colnames(analysis_data)[33] <- "First Communions per Catholic"

## TO DROP: columns 3,7,9-11,18,27,31,34.
analysis_data <- analysis_data[, -c(3, 7, 9:11, 18, 27, 31, 34)]
names(analysis_data)

#for (i in 2:ncol(analysis_data)) {
#  col_name <- names(analysis_data)[i]
#  min_val <- min(analysis_data[, i], na.rm = TRUE)
#  print(paste(col_name, "- Min:", min_val))
#}

# Histograms for Representative Variables -- made a unique code after the standardization but we can add something here if needed

#---- Standardization ---- 

# Log computation adjusted with ad-hoc epsilon
log_transform <- function(x) {
  # Identify non-zero values excluding exactly 0.007297
  non_zero <- x[x > 0 & x != 0.007297]
  
  # Handle case where there are no valid non-zero values
  if (length(non_zero) == 0) {
    epsilon <- 1e-6  # Fallback to a small constant if no valid min found
  } else {
    min_val <- min(non_zero, na.rm = TRUE)
    epsilon <- 0.5 * min_val
  }
  
  # Adjusted log computation
  log(x + epsilon)
}

# Logit computation adjusted with ad-hoc epsilon
logit_transform <- function(p) {
  # Identify non-zero values excluding exactly 0.007297
  non_zero <- p[p > 0 & p != 0.007297]
  
  # Handle case where there are no valid non-zero values
  if (length(non_zero) == 0) {
    epsilon <- 1e-6  # Fallback to a small constant if no valid min found
  } else {
    min_val <- min(non_zero, na.rm = TRUE)
    epsilon <- 0.5 * min_val
  }
  
  # Adjust p to [epsilon, 1 - epsilon] to avoid log(0) or division by zero
  p_adjusted <- pmin(pmax(p, epsilon), 1 - epsilon)
  
  # Adjusted logit computation
  log(p_adjusted / (1 - p_adjusted))
}

# Function to apply the standardization plan
standardize_data <- function(data) {
  
  # Create a copy of the data to avoid modifying the original
  data_std <- data
  
  # Catholics per 100 inhabitants: rescaling, logit
  data_std[, 2] <- logit_transform(data[, 2]/100)
  
  # Catholics per km^2: log
  data_std[, 3] <- log_transform(data[, 3])
  
  # Catholics per pastoral centre: log
  data_std[, 4] <- log_transform(data[, 4])
  
  # Share of diocesan pastors: logit
  data_std[, 5] <- logit_transform(data[, 5])
  
  # Non-vacant parishes administered by priests per Catholic: log
  data_std[, 6] <- log_transform(data[, 6])
  
  # Share of non-vacant parishes without priest entrusted to laypeople: logit
  data_std[, 7] <- logit_transform(data[, 7])
  
  # Parishes entirely vacant per Catholic: log
  data_std[, 8] <- log_transform(data[, 8])
  
  # Catholics per priest: log
  data_std[, 9] <- log_transform(data[, 9])
  
  # Share of ordinations of diocesan priests: rescaling, logit
  data_std[, 10] <- logit_transform(data[, 10]/100)
  
  # Share of deaths of diocesan priests: rescaling, logit
  data_std[, 11] <- logit_transform(data[, 11]/100)
  
  # Share of defections of diocesan priests: rescaling, logit
  data_std[, 12] <- logit_transform(data[, 12]/100)
  
  # Vocation rate per 100k inhabitants: rescaling, logit
  data_std[, 13] <- logit_transform(data[, 13]/100000)
  
  # Vocation rate per 100k Catholics: rescaling, logit
  data_std[, 14] <- logit_transform(data[, 14]/100000)
  
  # Candidates per 100 priests: rescaling, log
  data_std[, 15] <- log_transform(data[, 15]*100)
  
  # Candidates for diocesan clergy in theology centres per Catholic: log
  data_std[, 16] <- log_transform(data[, 16])
  
  # Candidates for religious clergy in theology centres per Catholic: log
  data_std[, 17] <- log_transform(data[, 17])
  
  # Infant baptisms (people up to 7 years old) per Catholic: log
  data_std[, 18] <- log_transform(data[, 18])
  
  # Adult baptisms (people over 7 years old) per Catholic: log
  data_std[, 19] <- log_transform(data[, 19])
  
  # Baptisms per inhabitant: logit
  data_std[, 20] <- logit_transform(data[, 20])
  
  # Marriages between Catholics per Catholic: log
  data_std[, 21] <- log_transform(data[, 21])
  
  # Mixed marriages per inhabitant: log
  data_std[, 22] <- log_transform(data[, 22])
  
  # Share of mixed marriages: 
  data_std[, 23] <- logit_transform(data[, 23])
  
  # Confirmations per Catholic
  data_std[, 24] <- log_transform(data[, 24])
  
  # First Communions per Catholic
  data_std[, 25] <- log_transform(data[, 25])
  
  return(data_std)
}

# Apply the standardization
cluster_data_2022_std <- standardize_data(analysis_data)

# Verify the transformation
summary(cluster_data_2022_std[, 2:25])

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



