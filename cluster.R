# Local development environment
path_data <- "C:/Users/schia/Documents/GitHub/world-map/church-data-map-world-main"
#path_data <- "C:/Users/soffi/Documents/Consulting_Catholic_Church"
setwd(path_data)

#---- Create the dataset ---- 

# Read the dataset, preserving original column names with spaces
final_geo_table <- read.csv("final_geo_table.csv", check.names = FALSE)

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

# Create cluster_data_2022 by filtering rows where Year == 2022
cluster_data_2022 <- cluster_data[cluster_data$Year == 2022, ]

# Remove the Year column
cluster_data_2022 <- cluster_data_2022[, colnames(cluster_data_2022) != "Year"]

#---- Analysis of the missing values ---- 
# Number of missing values per variable (column)
missing_per_variable <- colSums(is.na(cluster_data_2022))
print("Missing values per variable:")
print(missing_per_variable)

# Bar plot of missing values per variable
library(ggplot2)

missing_df <- data.frame(
  Variable = names(missing_per_variable),
  Missing_Count = as.numeric(missing_per_variable)
)

p1 <- ggplot(missing_df, aes(x = reorder(Variable, Missing_Count), y = Missing_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Missing Values per Variable",
       x = "Variable",
       y = "Number of Missing Values") +
  theme_minimal() +
  geom_text(aes(label = Missing_Count), hjust = -0.2, size = 3.5)

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

# Visualization 1: Bar plot of missing values per variable
library(ggplot2)
library(viridis)  

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
cluster_data_2022$missing_count <- rowSums(is.na(cluster_data_2022))
missing_per_region <- aggregate(missing_count ~ Region, data = cluster_data_2022, FUN = sum)
cluster_data_2022$missing_count <- NULL

print("Missing values per Region:")
print(missing_per_region)

#---- Visualization 3: Heatmap ----
library(reshape2)
library(viridis)

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
  
  p3 <- ggplot(missing_long, aes(x = Variable, y = Country, fill = Missing)) +
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
  
  print(p3)
} else {
  print("No countries with missing values found")
}

#---- Visualization 4: Percentage of missing data per country ----

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
  p4 <- ggplot(missing_summary, aes(x = reorder(Country, Missing_Percentage), 
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
  
  print(p4)
}


#---- Imputation ---- 

