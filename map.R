# ---- Load the required libraries ---- 

required_packages <- c("shiny", "leaflet", "dplyr", "readr", "sf", "DT", "shinythemes", "rnaturalearth", "rnaturalearthdata")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(DT)
library(shinythemes)
library(rnaturalearth)
library(rnaturalearthdata)

# ---- Docker instructions ---- 

options(shiny.host = "0.0.0.0") 
# tells Shiny to listen on all network interfaces

options(shiny.port = 3838) # Also 8180 is a valid option 
# tells Shiny which port to use

# ---- Set the Working Directory ---- 

setwd("C:/Users/schia/Documents/LMU/Consulting/App")

# ---- Load the data ---- 

data <- read.csv("final_geo_table.csv")

# Select only Countries
data_countries <- data %>%
  filter(Region.type == "Country")

# Load world map from Natural Earth (returns an 'sf' object for mapping)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge the data with the world map using country names
map_data <- left_join(world, data_countries, by = c("name" = "Region"))

# ---- Analysis of unmatched names ---- 

# Find unmatched names
unmatched_in_data <- anti_join(data_countries, world, by = c("Region" = "name"))
print(unmatched_in_data$Region)

# Manual corrections 
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
  "Kazakchstan" = "Kazakhstan",  # spelling fix
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

#Create a new column with the corrected country names 

data_countries$country <- ifelse(
  data_countries$Region %in% names(name_corrections),
  name_corrections[data_countries$Region],
  data_countries$Region
)

#Rematching
map_data <- left_join(world, data_countries, by = c("name" = "country"))

#Verification step 
unmatched_after_fix <- anti_join(data_countries, world, by = c("country" = "name"))
print(unique(unmatched_after_fix$country))


# ---- UI layout ----

ui <- navbarPage("World Stats Explorer", theme = shinytheme("flatly"),
                 
                 # First tab: Map
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # Drop-down to choose the variable  and the year to display
                              
                              selectInput("variable", "Select variable to display:",
                                          choices = names(data_countries)[sapply(data_countries, is.numeric) & names(data_countries) != "Year"]),
                              selectInput("year", "Select year:",
                                          choices = sort(unique(data_countries$Year)), selected = max(data_countries$Year))
                            ),
                            mainPanel(
                              # Output map
                              leafletOutput("map", height = 600)
                            )
                          )
                 ),
                 
                 # Second tab: Raw Data Table
                 tabPanel("Data Table",
                          DTOutput("table")
                 )
)


# ---- Server logic ---- 

server <- function(input, output, session) {
  
  # Dynamically update year choices when variable changes
  observeEvent(input$variable, {
    available_years <- sort(unique(data_countries %>%
                                     filter(!is.na(.data[[input$variable]])) %>%
                                     pull(Year)))
    
    updateSelectInput(session, "year", 
                      choices = available_years, 
                      selected = max(available_years))
  })
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    req(input$variable, input$year)  # Ensure both inputs are available
    
    # Filter data for selected year
    filtered_data <- map_data %>%
      filter(Year == input$year)
    
    # Define color palette
    pal <- colorNumeric("YlGnBu", domain = filtered_data[[input$variable]], na.color = "transparent")
    
    # Build map
    leaflet(filtered_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(filtered_data[[input$variable]]),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = ~paste(name, "<br>", input$variable, ":", filtered_data[[input$variable]]),
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = filtered_data[[input$variable]],
        title = paste(input$variable, "(", input$year, ")"),
        position = "bottomright"
      )
  })
  
  # Render the raw data table
  output$table <- renderDT({
    req(input$year)
    datatable(filter(data_countries, Year == input$year))
  })
  
}



# ---- Launch the app ----
shinyApp(ui, server)
