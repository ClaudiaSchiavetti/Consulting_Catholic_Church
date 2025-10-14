# Local development environment
#path_data <- "C:/Users/schia/Documents/GitHub/world-map/church-data-map-world-main"
path_data <- "C:/Users/soffi/Documents/Consulting_Catholic_Church"
setwd(path_data)

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

# Number of missing values per variable (column)
missing_per_variable <- colSums(is.na(cluster_data_2022))
print("Missing values per variable:")
print(missing_per_variable)

# Number of missing values per each "Region" value
# First, add a temporary column for row-wise missing count
cluster_data_2022$missing_count <- rowSums(is.na(cluster_data_2022))

# Then, aggregate by Region
missing_per_region <- aggregate(missing_count ~ Region, data = cluster_data_2022, FUN = sum)

# Remove the temporary column
cluster_data_2022$missing_count <- NULL

print("Missing values per Region:")
print(missing_per_region)
