# ---- Load Required Libraries ----
# This section installs and loads all necessary packages for the Shiny app.
# It checks if each package is installed and installs it if not, then loads them.

required_packages <- c(
  "shiny", "leaflet", "dplyr", "readr", "sf", "DT", "shinythemes",
  "rnaturalearth", "rnaturalearthdata", "RColorBrewer", "webshot",
  "writexl", "plotly", "shinyjs", "viridisLite", "ggplot2", "htmlwidgets"
)
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load the libraries after ensuring they are installed.
lapply(required_packages, library, character.only = TRUE)
useShinyjs()
# Ensure PhantomJS is installed for webshot functionality (used for map downloads).
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}


# ---- Docker Instructions ----
# Set Shiny app to listen on all interfaces and a specific port for Docker compatibility.

options(shiny.host = "0.0.0.0")
options(shiny.port = 3838) # Also 8180 is a valid option



# ---- Load the Data ----

# Read the CSV file containing the geographic data
data <- read.csv("final_geo_table.csv", check.names = FALSE)

# ---- Define Variable Abbreviations ----
# Read variable abbreviations from a CSV file and create a named vector.
# The CSV file should have two columns: 'variable_name' and 'abbreviation'.

abbreviations_file <- file.path(path_outputs, "variable_abbreviations.csv")
if (!file.exists(abbreviations_file)) {
  stop("Variable abbreviations CSV file not found at: ", abbreviations_file)
}
abbreviations_df <- read.csv(abbreviations_file, stringsAsFactors = FALSE, check.names = FALSE)
variable_abbreviations <- setNames(abbreviations_df$abbreviation, abbreviations_df$variable_name)


# ---- Data Filtering Functions ----
# Function to check if a column has non-NA data at the country level.

has_country_data <- function(col_data, region_type) {
  country_data <- col_data[region_type == "Country"]
  return(sum(!is.na(country_data)) > 0)
}


# ---- Identify and Filter Columns with Country Data ----
# Apply the function to identify columns with country-level data.

cols_with_country_data <- sapply(names(data), function(col_name) {
  if (is.numeric(data[[col_name]])) {
    has_country_data(data[[col_name]], data$`Region type`)
  } else {
    TRUE
  }
})

# Filter the dataset to retain only columns with country data.
data_filtered <- data[, cols_with_country_data]


# ---- Separate Data into Macroregions and Countries ----
# Extract macroregion data and rename the region column for consistency.

data_macroregions <- data_filtered %>%
  filter(`Region type` == "Macroregion") %>%
  rename(macroregion = Region)


# ---- Safe Division Helper Function ----
# Helper function to perform division safely, avoiding NA or division by zero.

safe_div <- function(num, den, scale = 1) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, (num / den) * scale)
}


# ---- Process and Merge Macroregions ----
# Standardize names, merge split regions, and filter out aggregates
data_macroregions <- data_macroregions %>%
  mutate(macroregion = case_when(
    macroregion %in% c("Central America (Mainland)", "Central America (Antilles)") ~ "Central America",
    macroregion %in% c("South East and Far East Asia", "South & Far East Asia") ~ "South and Far East Asia",
    macroregion == "Middle East" ~ "Middle East Asia",
    TRUE ~ macroregion
  )) %>%
  filter(!macroregion %in% c("World", "America", "Asia")) %>%
  mutate(Year = suppressWarnings(as.integer(Year))) %>%  # Ensure Year is integer, matching country processing
  group_by(macroregion, Year) %>%
  summarise(
    across(
      where(is.numeric),
      ~ if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)
    ),
    .groups = "drop"
  )

# ---- Recompute Derived Variables for Macroregions ----
# Shorthand for data_macroregions to simplify recomputations.
dm <- data_macroregions
# Recompute density and rates, rounding to 2 decimal places.
if (all(c("Inhabitants per km^2", "Inhabitants in thousands", "Area in km^2") %in% names(dm))) {
  dm[["Inhabitants per km^2"]] <- round(safe_div(dm[["Inhabitants in thousands"]] * 1000, dm[["Area in km^2"]]), 2)
}
if (all(c("Catholics per 100 inhabitants", "Catholics in thousands", "Inhabitants in thousands") %in% names(dm))) {
  dm[["Catholics per 100 inhabitants"]] <- round(safe_div(dm[["Catholics in thousands"]], dm[["Inhabitants in thousands"]], 100), 2)
}
if (all(c("Inhabitants per pastoral centre", "Inhabitants in thousands", "Pastoral centres (total)") %in% names(dm))) {
  dm[["Inhabitants per pastoral centre"]] <- round(safe_div(dm[["Inhabitants in thousands"]] * 1000, dm[["Pastoral centres (total)"]]), 2)
}
if (all(c("Catholics per pastoral centre", "Catholics in thousands", "Pastoral centres (total)") %in% names(dm))) {
  dm[["Catholics per pastoral centre"]] <- round(safe_div(dm[["Catholics in thousands"]] * 1000, dm[["Pastoral centres (total)"]]), 2)
}
if (all(c("Pastoral centres per diocese", "Pastoral centres (total)", "Ecclesiastical territories (total)") %in% names(dm))) {
  dm[["Pastoral centres per diocese"]] <- round(safe_div(dm[["Pastoral centres (total)"]], dm[["Ecclesiastical territories (total)"]]), 2)
}
if (all(c("Parishes as share of total pastoral centres", "Parishes (total)", "Pastoral centres (total)") %in% names(dm))) {
  dm[["Parishes as share of total pastoral centres"]] <- round(safe_div(dm[["Parishes (total)"]], dm[["Pastoral centres (total)"]]), 2)
}
# Recompute mission stations share.
if (all(c("Mission stations with resident priest",
          "Mission stations without resident priest",
          "Pastoral centres (total)") %in% names(dm))) {
  dm[["Mission stations as share of total pastoral centres"]] <-
    round(safe_div(dm[["Mission stations with resident priest"]] +
                     dm[["Mission stations without resident priest"]],
                   dm[["Pastoral centres (total)"]]), 2)
}
if (all(c("Number of other pastoral centres as share of total pastoral centres", "Other pastoral centres", "Pastoral centres (total)") %in% names(dm))) {
  dm[["Number of other pastoral centres as share of total pastoral centres"]] <- round(safe_div(dm[["Other pastoral centres"]], dm[["Pastoral centres (total)"]]), 2)
}
# Compute total priests for burdens.
priests_total <- if ("Priests (diocesan and religious)" %in% names(dm)) {
  dm[["Priests (diocesan and religious)"]]
} else if (all(c("Diocesan priests (total)", "Religious priests") %in% names(dm))) {
  dm[["Diocesan priests (total)"]] + dm[["Religious priests"]]
} else {
  NA_real_
}
if ("Inhabitants per priest" %in% names(dm) && !all(is.na(priests_total))) {
  dm[["Inhabitants per priest"]] <- round(safe_div(dm[["Inhabitants in thousands"]] * 1000, priests_total), 2)
}
if ("Catholics per priest" %in% names(dm) && !all(is.na(priests_total))) {
  dm[["Catholics per priest"]] <- round(safe_div(dm[["Catholics in thousands"]] * 1000, priests_total), 2)
}
# Recompute sacraments per 1000 Catholics.
if (all(c("Infant baptisms (people up to 7 years old) per 1000 Catholics",
          "Infant baptisms (people up to 7 years old)", "Catholics in thousands") %in% names(dm))) {
  dm[["Infant baptisms (people up to 7 years old) per 1000 Catholics"]] <-
    round(safe_div(dm[["Infant baptisms (people up to 7 years old)"]], dm[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("Marriages per 1000 Catholics", "Marriages", "Catholics in thousands") %in% names(dm))) {
  dm[["Marriages per 1000 Catholics"]] <- round(safe_div(dm[["Marriages"]], dm[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("Confirmations per 1000 Catholics", "Confirmations", "Catholics in thousands") %in% names(dm))) {
  dm[["Confirmations per 1000 Catholics"]] <- round(safe_div(dm[["Confirmations"]], dm[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("First Communions per 1000 Catholics", "First Communions", "Catholics in thousands") %in% names(dm))) {
  dm[["First Communions per 1000 Catholics"]] <- round(safe_div(dm[["First Communions"]], dm[["Catholics in thousands"]] * 1000, 1000), 2)
}
# Recompute shares.
if (all(c("Share of adult baptisms (people over 7 years old)", "Adult baptisms (people over 7 years old)", "Baptisms") %in% names(dm))) {
  dm[["Share of adult baptisms (people over 7 years old)"]] <- round(safe_div(dm[["Adult baptisms (people over 7 years old)"]], dm[["Baptisms"]]), 2)
}
if (all(c("Share of mixed marriages (among those celebrated with ecclesiastical rite)", "Mixed marriages", "Marriages") %in% names(dm))) {
  dm[["Share of mixed marriages (among those celebrated with ecclesiastical rite)"]] <- round(safe_div(dm[["Mixed marriages"]], dm[["Marriages"]]), 2)
}
# Recompute vocation/ordination/departure rates.
if (all(c("Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants",
          "Candidates for diocesan and religious clergy in philosophy+theology centres", "Inhabitants in thousands") %in% names(dm))) {
  dm[["Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants"]] <-
    round(safe_div(dm[["Candidates for diocesan and religious clergy in philosophy+theology centres"]],
                   dm[["Inhabitants in thousands"]] * 1000, 100000), 2)
}
if (all(c("Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics",
          "Candidates for diocesan and religious clergy in philosophy+theology centres", "Catholics in thousands") %in% names(dm))) {
  dm[["Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics"]] <-
    round(safe_div(dm[["Candidates for diocesan and religious clergy in philosophy+theology centres"]],
                   dm[["Catholics in thousands"]] * 1000, 100000), 2)
}
if (all(c("Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood",
          "Yearly ordinations of diocesan priests", "Candidates for diocesan clergy in philosophy+theology centres") %in% names(dm))) {
  dm[["Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood"]] <-
    round(safe_div(dm[["Yearly ordinations of diocesan priests"]],
                   dm[["Candidates for diocesan clergy in philosophy+theology centres"]], 100), 2)
}
if (all(c("Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy",
          "Students in philosophy+theology centres for diocesan clergy who left seminary",
          "Candidates for diocesan clergy in philosophy+theology centres") %in% names(dm))) {
  dm[["Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy"]] <-
    round(safe_div(dm[["Students in philosophy+theology centres for diocesan clergy who left seminary"]],
                   dm[["Candidates for diocesan clergy in philosophy+theology centres"]], 100), 2)
}
# Recompute philosophy+theology candidates per 100 priests.
if (all(c("Philosophy+theology candidates for diocesan and religious clergy per 100 priests",
          "Candidates for diocesan and religious clergy in philosophy+theology centres") %in% names(dm)) && !all(is.na(priests_total))) {
  dm[["Philosophy+theology candidates for diocesan and religious clergy per 100 priests"]] <-
    round(safe_div(dm[["Candidates for diocesan and religious clergy in philosophy+theology centres"]], priests_total, 100), 2)
}
# Compute lagged values for diocesan priest shares.
dm <- dm %>%
  arrange(macroregion, Year) %>%
  group_by(macroregion) %>%
  mutate(
    prev_inc_priests = lag(`Incardinated diocesan priests on January 1`, n = 1)
  ) %>%
  ungroup()
if (all(c("Yearly ordinations of diocesan priests as share of those incardinated on January 1",
          "Yearly ordinations of diocesan priests") %in% names(dm))) {
  dm[["Yearly ordinations of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(dm[["Yearly ordinations of diocesan priests"]], dm[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly deaths of diocesan priests as share of those incardinated on January 1",
          "Yearly deaths of diocesan priests") %in% names(dm))) {
  dm[["Yearly deaths of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(dm[["Yearly deaths of diocesan priests"]], dm[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly defections of diocesan priests as share of those incardinated at January 1",
          "Yearly defections of diocesan priests") %in% names(dm))) {
  dm[["Yearly defections of diocesan priests as share of those incardinated at January 1"]] <-
    round(safe_div(dm[["Yearly defections of diocesan priests"]], dm[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1",
          "Yearly ordinations of diocesan priests", "Yearly deaths of diocesan priests", "Yearly defections of diocesan priests") %in% names(dm))) {
  net_ord <- dm[["Yearly ordinations of diocesan priests"]] - dm[["Yearly deaths of diocesan priests"]] - dm[["Yearly defections of diocesan priests"]]
  dm[["Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(net_ord, dm[["prev_inc_priests"]], 100), 2)
}
# Recompute weighted averages.
if (all(c("Average area of ecclesiastical territories in km^2", "Area in km^2", "Ecclesiastical territories (total)") %in% names(dm))) {
  dm[["Average area of ecclesiastical territories in km^2"]] <-
    round(safe_div(dm[["Area in km^2"]], dm[["Ecclesiastical territories (total)"]]), 2)
}
if (all(c("Average diocesan area (in km^2)", "Area in km^2", "Ecclesiastical territories (total)") %in% names(dm))) {
  dm[["Average diocesan area (in km^2)"]] <-
    round(safe_div(dm[["Area in km^2"]], dm[["Ecclesiastical territories (total)"]]), 2)
}
# Recompute apostolic workforce share.
if (all(c("Priests and bishops as share of apostolic workforce",
          "Bishops (total)", "Catechists", "Lay missionaries") %in% names(dm)) &&
    ("Priests (diocesan and religious)" %in% names(dm) ||
     all(c("Diocesan priests (total)", "Religious priests") %in% names(dm)))) {
  priests_total <- if ("Priests (diocesan and religious)" %in% names(dm)) {
    dm[["Priests (diocesan and religious)"]]
  } else {
    dm[["Diocesan priests (total)"]] + dm[["Religious priests"]]
  }
  
  workforce <- priests_total + dm[["Bishops (total)"]] + dm[["Catechists"]] + dm[["Lay missionaries"]]
  dm[["Priests and bishops as share of apostolic workforce"]] <-
    round(safe_div(priests_total + dm[["Bishops (total)"]], workforce), 2)
}
# Commit all recomputed values back to data_macroregions.
data_macroregions <- dm

# Extract country data.
data_countries <- data_filtered %>%
  filter(`Region type` == "Country")


# ---- Load and Prepare World Map ----
# Load world map data from Natural Earth as an sf object.

world <- ne_countries(scale = "medium", returnclass = "sf")

# Unify Somalia by merging Somalia and Somaliland geometries.
somalia_unified <- world %>%
  filter(admin %in% c("Somalia", "Somaliland")) %>%
  summarise(admin = "Somalia", name = "Somalia", geometry = st_union(geometry))

# Update the world map by removing original entries and adding the unified Somalia.
world <- world %>%
  filter(!admin %in% c("Somalia", "Somaliland")) %>%
  bind_rows(somalia_unified)


# ---- Initial Map Data Merge ----
# Merge country data with the world map using country names.

map_data <- left_join(world, data_countries, by = c("name" = "Region"))

# ---- Analyze Unmatched Country Names ----
# Identify countries in data that do not match the world map.

unmatched_in_data <- anti_join(data_countries, world, by = c("Region" = "name"))
#print(unmatched_in_data$Region)


# ---- Manual Country Name Corrections ----
# Define a vector of corrections for mismatched country names.

name_corrections <- c(
  "Antigua and Barbuda" = "Antigua and Barb.",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Bosnia and Herzegovina" = "Bosnia and Herz.",
  "Brunei Darussalam" = "Brunei",
  "Cayman Islands" = "Cayman Is.",
  "Central African Republic" = "Central African Rep.",
  "China (Mainland)" = "China",
  "China (Taiwan)" = "Taiwan",
  "Cook Islands" = "Cook Is.",
  "Cote d'Ivoire" = "Côte d'Ivoire",
  "Dem. Peoples Rep. Of Korea" = "North Korea",
  "Dem. Rep. of the Congo" = "Dem. Rep. Congo",
  "Dominican Republic" = "Dominican Rep.",
  "East Timor" = "Timor-Leste",
  "Equatorial Guinea" = "Eq. Guinea",
  "Eswatini" = "eSwatini",
  "Faeroe Islands" = "Faeroe Is.",
  "Falkland Islands (Malvinas)" = "Falkland Is.",
  "Fed. S. Micronesia" = "Micronesia",
  "French Guiana" = "France",
  "French Polynesia" = "Fr. Polynesia",
  "Gibraltar" = "United Kingdom",
  "Great Britain" = "United Kingdom",
  "Guadeloupe" = "France",
  "Hong Kong SAR" = "Hong Kong",
  "Iran (Islamic Rep. of)" = "Iran",
  "Kazakchstan" = "Kazakhstan",
  "Macao SAR" = "Macao",
  "Macedonia (Rep. of North)" = "North Macedonia",
  "Marshall Islands" = "Marshall Is.",
  "Martinique" = "France",
  "Montenegro (Rep. of)" = "Montenegro",
  "N. Mariana Islands" = "United States of America",
  "Netherlands Antilles" = "Curaçao",
  "Peoples Dem. Rep. Lao" = "Laos",
  "Republic of Korea" = "South Korea",
  "Republic of Moldova" = "Moldova",
  "Reunion" = "France",
  "Russian Fed. (in Europe)" = "Russia",
  "Russian Federation (in Asia)" = "Russia",
  "Saint Kitts and Nevis" = "St. Kitts and Nevis",
  "Sao Tome and Principe" = "São Tomé and Principe",
  "Solomon Islands" = "Solomon Is.",
  "South Sudan" = "S. Sudan",
  "St. Vincent and the Grenadines" = "St. Vin. and Gren.",
  "Svalbard and Jan Mayen Is." = "Norway",
  "Syrian Arab Republic" = "Syria",
  "Tokelau" = "New Zealand",
  "Turkiye" = "Turkey",
  "Turks and Caicos Islands" = "Turks and Caicos Is.",
  "United Republic of Tanzania" = "Tanzania",
  "United States" = "United States of America",
  "Venezuela (The Bolivar Rep. of)" = "Venezuela",
  "Viet Nam" = "Vietnam",
  "Virgin Islands (Great. Brit.)" = "British Virgin Is.",
  "Virgin Islands (U.S.A.)" = "United States of America",
  "Wallis and Futuna Islands" = "Wallis and Futuna Is.",
  "Western Sahara" = "W. Sahara"
)

# Apply corrections by creating a new 'country' column with standardized names.
data_countries$country <- ifelse(
  data_countries$Region %in% names(name_corrections),
  name_corrections[data_countries$Region],
  data_countries$Region
)


# ---- Aggregate Numeric Values by Country and Year ----
# Ensure 'Year' column exists and is converted to integer.

stopifnot("Year" %in% names(data_countries))
data_countries <- data_countries %>%
  mutate(Year = suppressWarnings(as.integer(Year)))

# Identify numeric columns excluding 'Year'.
num_cols <- names(data_countries)[sapply(data_countries, is.numeric)]
num_cols <- setdiff(num_cols, "Year")

# Aggregate by summing numeric values for each country-year pair.
data_countries <- data_countries %>%
  group_by(country, Year) %>%
  summarise(
    across(
      all_of(num_cols),
      ~ if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)
    ),
    .groups = "drop"
  )


# ---- Recompute Derived Variables ----
# Shorthand for data_countries to simplify recomputations.

dc <- data_countries

# Recompute density and rates, rounding to 2 decimal places.
if (all(c("Inhabitants per km^2", "Inhabitants in thousands", "Area in km^2") %in% names(dc))) {
  dc[["Inhabitants per km^2"]] <- round(safe_div(dc[["Inhabitants in thousands"]] * 1000, dc[["Area in km^2"]]), 2)
}
if (all(c("Catholics per 100 inhabitants", "Catholics in thousands", "Inhabitants in thousands") %in% names(dc))) {
  dc[["Catholics per 100 inhabitants"]] <- round(safe_div(dc[["Catholics in thousands"]], dc[["Inhabitants in thousands"]], 100), 2)
}
if (all(c("Inhabitants per pastoral centre", "Inhabitants in thousands", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Inhabitants per pastoral centre"]] <- round(safe_div(dc[["Inhabitants in thousands"]] * 1000, dc[["Pastoral centres (total)"]]), 2)
}
if (all(c("Catholics per pastoral centre", "Catholics in thousands", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Catholics per pastoral centre"]] <- round(safe_div(dc[["Catholics in thousands"]] * 1000, dc[["Pastoral centres (total)"]]), 2)
}
if (all(c("Pastoral centres per diocese", "Pastoral centres (total)", "Ecclesiastical territories (total)") %in% names(dc))) {
  dc[["Pastoral centres per diocese"]] <- round(safe_div(dc[["Pastoral centres (total)"]], dc[["Ecclesiastical territories (total)"]]), 2)
}
if (all(c("Parishes as share of total pastoral centres", "Parishes (total)", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Parishes as share of total pastoral centres"]] <- round(safe_div(dc[["Parishes (total)"]], dc[["Pastoral centres (total)"]]), 2)
}

# Recompute mission stations share.
if (all(c("Mission stations with resident priest",
          "Mission stations without resident priest",
          "Pastoral centres (total)") %in% names(dc))) {
  dc[["Mission stations as share of total pastoral centres"]] <-
    round(safe_div(dc[["Mission stations with resident priest"]] +
                     dc[["Mission stations without resident priest"]],
                   dc[["Pastoral centres (total)"]]), 2)
}
if (all(c("Number of other pastoral centres as share of total pastoral centres", "Other pastoral centres", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Number of other pastoral centres as share of total pastoral centres"]] <- round(safe_div(dc[["Other pastoral centres"]], dc[["Pastoral centres (total)"]]), 2)
}

# Compute total priests for burdens.
priests_total <- if ("Priests (diocesan and religious)" %in% names(dc)) {
  dc[["Priests (diocesan and religious)"]]
} else if (all(c("Diocesan priests (total)", "Religious priests") %in% names(dc))) {
  dc[["Diocesan priests (total)"]] + dc[["Religious priests"]]
} else {
  NA_real_
}
if ("Inhabitants per priest" %in% names(dc) && !all(is.na(priests_total))) {
  dc[["Inhabitants per priest"]] <- round(safe_div(dc[["Inhabitants in thousands"]] * 1000, priests_total), 2)
}
if ("Catholics per priest" %in% names(dc) && !all(is.na(priests_total))) {
  dc[["Catholics per priest"]] <- round(safe_div(dc[["Catholics in thousands"]] * 1000, priests_total), 2)
}

# Recompute sacraments per 1000 Catholics.
if (all(c("Infant baptisms (people up to 7 years old) per 1000 Catholics",
          "Infant baptisms (people up to 7 years old)", "Catholics in thousands") %in% names(dc))) {
  dc[["Infant baptisms (people up to 7 years old) per 1000 Catholics"]] <-
    round(safe_div(dc[["Infant baptisms (people up to 7 years old)"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("Marriages per 1000 Catholics", "Marriages", "Catholics in thousands") %in% names(dc))) {
  dc[["Marriages per 1000 Catholics"]] <- round(safe_div(dc[["Marriages"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("Confirmations per 1000 Catholics", "Confirmations", "Catholics in thousands") %in% names(dc))) {
  dc[["Confirmations per 1000 Catholics"]] <- round(safe_div(dc[["Confirmations"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("First Communions per 1000 Catholics", "First Communions", "Catholics in thousands") %in% names(dc))) {
  dc[["First Communions per 1000 Catholics"]] <- round(safe_div(dc[["First Communions"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}

# Recompute shares.
if (all(c("Share of adult baptisms (people over 7 years old)", "Adult baptisms (people over 7 years old)", "Baptisms") %in% names(dc))) {
  dc[["Share of adult baptisms (people over 7 years old)"]] <- round(safe_div(dc[["Adult baptisms (people over 7 years old)"]], dc[["Baptisms"]]), 2)
}
if (all(c("Share of mixed marriages (among those celebrated with ecclesiastical rite)", "Mixed marriages", "Marriages") %in% names(dc))) {
  dc[["Share of mixed marriages (among those celebrated with ecclesiastical rite)"]] <- round(safe_div(dc[["Mixed marriages"]], dc[["Marriages"]]), 2)
}

# Recompute vocation/ordination/departure rates.
if (all(c("Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants",
          "Candidates for diocesan and religious clergy in philosophy+theology centres", "Inhabitants in thousands") %in% names(dc))) {
  dc[["Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants"]] <-
    round(safe_div(dc[["Candidates for diocesan and religious clergy in philosophy+theology centres"]],
                   dc[["Inhabitants in thousands"]] * 1000, 100000), 2)
}
if (all(c("Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics",
          "Candidates for diocesan and religious clergy in philosophy+theology centres", "Catholics in thousands") %in% names(dc))) {
  dc[["Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics"]] <-
    round(safe_div(dc[["Candidates for diocesan and religious clergy in philosophy+theology centres"]],
                   dc[["Catholics in thousands"]] * 1000, 100000), 2)
}
if (all(c("Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood",
          "Yearly ordinations of diocesan priests", "Candidates for diocesan clergy in philosophy+theology centres") %in% names(dc))) {
  dc[["Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood"]] <-
    round(safe_div(dc[["Yearly ordinations of diocesan priests"]],
                   dc[["Candidates for diocesan clergy in philosophy+theology centres"]], 100), 2)
}
if (all(c("Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy",
          "Students in philosophy+theology centres for diocesan clergy who left seminary",
          "Candidates for diocesan clergy in philosophy+theology centres") %in% names(dc))) {
  dc[["Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy"]] <-
    round(safe_div(dc[["Students in philosophy+theology centres for diocesan clergy who left seminary"]],
                   dc[["Candidates for diocesan clergy in philosophy+theology centres"]], 100), 2)
}

# Recompute philosophy+theology candidates per 100 priests.
if (all(c("Philosophy+theology candidates for diocesan and religious clergy per 100 priests",
          "Candidates for diocesan and religious clergy in philosophy+theology centres") %in% names(dc)) && !all(is.na(priests_total))) {
  dc[["Philosophy+theology candidates for diocesan and religious clergy per 100 priests"]] <-
    round(safe_div(dc[["Candidates for diocesan and religious clergy in philosophy+theology centres"]], priests_total, 100), 2)
}

# Compute lagged values for diocesan priest shares.
dc <- dc %>%
  arrange(country, Year) %>%
  group_by(country) %>%
  mutate(
    prev_inc_priests = lag(`Incardinated diocesan priests on January 1`, n = 1)
  ) %>%
  ungroup()
if (all(c("Yearly ordinations of diocesan priests as share of those incardinated on January 1",
          "Yearly ordinations of diocesan priests") %in% names(dc))) {
  dc[["Yearly ordinations of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(dc[["Yearly ordinations of diocesan priests"]], dc[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly deaths of diocesan priests as share of those incardinated on January 1",
          "Yearly deaths of diocesan priests") %in% names(dc))) {
  dc[["Yearly deaths of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(dc[["Yearly deaths of diocesan priests"]], dc[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly defections of diocesan priests as share of those incardinated at January 1",
          "Yearly defections of diocesan priests") %in% names(dc))) {
  dc[["Yearly defections of diocesan priests as share of those incardinated at January 1"]] <-
    round(safe_div(dc[["Yearly defections of diocesan priests"]], dc[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1",
          "Yearly ordinations of diocesan priests", "Yearly deaths of diocesan priests", "Yearly defections of diocesan priests") %in% names(dc))) {
  net_ord <- dc[["Yearly ordinations of diocesan priests"]] - dc[["Yearly deaths of diocesan priests"]] - dc[["Yearly defections of diocesan priests"]]
  dc[["Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(net_ord, dc[["prev_inc_priests"]], 100), 2)
}

# Recompute weighted averages.
if (all(c("Average area of ecclesiastical territories in km^2", "Area in km^2", "Ecclesiastical territories (total)") %in% names(dc))) {
  dc[["Average area of ecclesiastical territories in km^2"]] <-
    round(safe_div(dc[["Area in km^2"]], dc[["Ecclesiastical territories (total)"]]), 2)
}
if (all(c("Average diocesan area (in km^2)", "Area in km^2", "Ecclesiastical territories (total)") %in% names(dc))) {
  dc[["Average diocesan area (in km^2)"]] <-
    round(safe_div(dc[["Area in km^2"]], dc[["Ecclesiastical territories (total)"]]), 2)
}

# Recompute apostolic workforce share.
if (all(c("Priests and bishops as share of apostolic workforce",
          "Bishops (total)", "Catechists", "Lay missionaries") %in% names(dc)) &&
    ("Priests (diocesan and religious)" %in% names(dc) ||
     all(c("Diocesan priests (total)", "Religious priests") %in% names(dc)))) {
  priests_total <- if ("Priests (diocesan and religious)" %in% names(dc)) {
    dc[["Priests (diocesan and religious)"]]
  } else {
    dc[["Diocesan priests (total)"]] + dc[["Religious priests"]]
  }
  
  workforce <- priests_total + dc[["Bishops (total)"]] + dc[["Catechists"]] + dc[["Lay missionaries"]]
  dc[["Priests and bishops as share of apostolic workforce"]] <-
    round(safe_div(priests_total + dc[["Bishops (total)"]], workforce), 2)
}

# Commit all recomputed values back to data_countries.
data_countries <- dc


# ---- Define Excluded Variables for Map Tab ----
# List variables to exclude from the "Map" tab's variable selection menu.

excluded_vars <- c(
  "Area in km^2",
  "prev_inc_priests",
  "Yearly ordinations of diocesan priests as share of those incardinated on January 1",
  "Yearly deaths of diocesan priests as share of those incardinated on January 1",
  "Yearly defections of diocesan priests as share of those incardinated at January 1",
  "Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1",
  "Ecclesiastical territories (total) - index numbers (base 2013 = 100)",
  "Pastoral centres (total) - index numbers (base 2013 = 100)",
  "Parishes (total) - index numbers (base 2013 = 100)",
  "Priests (diocesan and religious) - index numbers (base 2013 = 100)",
  "Diocesan priests (total) - index numbers (base 2013 = 100)",
  "Incardinated diocesan priests - index numbers (base 2013 = 100)",
  "Religious priests - index numbers (base 2013 = 100)",
  "Permanent deacons (diocesan and religious) - index numbers (base 2013 = 100)",
  "Non-priest religious men (with temporary or perpetual vows) - index numbers (base 2013 = 100)",
  "Religious women (with temporary or perpetual vows) - index numbers (base 2013 = 100)",
  "Candidates for diocesan and religious clergy in philosophy+theology centres - index numbers (base 2013 = 100)",
  "Students for diocesan and religious clergy in secondary schools - index numbers (base 2013 = 100)"
)

# ---- Define Allowed Variables for Map Tab ----
# Select numeric variables excluding 'Year' and those in excluded_vars.

allowed_variables <- setdiff(
  names(data_countries)[sapply(data_countries, is.numeric) & names(data_countries) != "Year"],
  excluded_vars
)


# ---- Final Map Data Rematching ----
# Re-merge the world map with corrected country data.

map_data <- left_join(world, data_countries, by = c("name" = "country"))

# Check for any remaining unmatched countries and print them.
unmatched_after_fix <- anti_join(data_countries, world, by = c("country" = "name"))
# print(unique(unmatched_after_fix$country))

# ---- Identify Time Series Variables ----
# Filter variables that have data for more than one year for time series use.

time_series_vars <- allowed_variables[
  sapply(allowed_variables, function(var) {
    years <- data_countries %>%
      filter(!is.na(.data[[var]])) %>%
      pull(Year) %>%
      unique()
    length(years) > 1
  })
]

