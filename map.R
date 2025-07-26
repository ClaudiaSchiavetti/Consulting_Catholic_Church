# ---- Load the required libraries ---- 

required_packages <- c("shiny", "leaflet", "dplyr", "readr", "sf", "DT", "shinythemes", "rnaturalearth", "rnaturalearthdata", "RColorBrewer")

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
library(viridisLite)
library(ggplot2)
library(RColorBrewer)

# ---- Docker instructions ---- 

options(shiny.host = "0.0.0.0") 
# tells Shiny to listen on all network interfaces

options(shiny.port = 3838) # Also 8180 is a valid option 
# tells Shiny which port to use

# ---- Set the Working Directory ---- 
#path_outputs <- "C:/Users/schia/Documents/LMU/Consulting/App"
path_outputs <- "C:\\Users\\soffi\\Desktop\\CONSULTING"
setwd(path_outputs)

# ---- Load the data ---- 

data <- read.csv("final_geo_table.csv", check.names = FALSE)

# Function to check if a column has data only in macroregions
has_country_data <- function(col_data, region_type) {
  # Check if there's any non-NA data for countries
  country_data <- col_data[region_type == "Country"]
  return(sum(!is.na(country_data)) > 0)
}

# Identify columns that have data for countries
cols_with_country_data <- sapply(names(data), function(col_name) {
  if(is.numeric(data[[col_name]])) {
    has_country_data(data[[col_name]], data$`Region type`)
  } else {
    TRUE  # Keep non-numeric columns
  }
})

#To verify the debug
#print(paste("Total columns:", length(cols_with_country_data)))
#print(paste("Columns with country data:", sum(cols_with_country_data)))
#print(paste("Columns with only macroregion data:", sum(!cols_with_country_data)))

# Filter your dataset to keep only columns with country data
data_filtered <- data[, cols_with_country_data]

# Select only Countries
data_countries <- data %>%
  filter(`Region type` == "Country")

# Load world map from Natural Earth (returns an 'sf' object for mapping)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Fix Somalia shape by merging Somalia and Somaliland
somalia_unified <- world %>%
  filter(admin %in% c("Somalia", "Somaliland")) %>%
  summarise(admin = "Somalia", name = "Somalia", geometry = st_union(geometry))

# Remove original entries and add unified Somalia
world <- world %>%
  filter(!admin %in% c("Somalia", "Somaliland")) %>%
  bind_rows(somalia_unified)

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

# Aggregate numeric values for countries that have been merged but keeping NA when necessary
data_countries <- data_countries %>%
  group_by(country, Year) %>%
  summarise(across(
    all_of(setdiff(names(.)[sapply(., is.numeric)], "Year")),
    ~ if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)
  ), .groups = "drop")




#Rematching
map_data <- left_join(world, data_countries, by = c("name" = "country"))

#Verification step 
unmatched_after_fix <- anti_join(data_countries, world, by = c("country" = "name"))
print(unique(unmatched_after_fix$country))


# ---- UI layout ----
# 1. Get the list of unique, non-missing, non-empty country names from the data
all_countries <- sort(unique(data_countries$country))

# Remove any NA values or purely empty strings that might be in the country list
all_countries <- all_countries[!is.na(all_countries) & all_countries != ""]

# 2. Create a named list for selectizeInput choices
# The 'names' of this list are what the user sees in the dropdown.
# The 'values' of this list are what Shiny passes to the server logic.
# For countries, it's common for them to be the same.
country_choices_list <- as.list(all_countries)
names(country_choices_list) <- all_countries # Assign each country name as its own list element's name

# 3. Add the initial blank/placeholder option to the beginning of the list
final_country_dropdown_choices <- c("Type to search..." = "", country_choices_list)

ui <- tagList(
  tags$head(
    #css
    tags$style(HTML("
  html, body {
    height: 100%;
    margin: 0;
    padding: 0;
    overflow: hidden; /* Keep for Map tab */
  }
  #map {
    height: 100vh !important;
    width: 100vw !important;
    position: absolute;
    top: 0;
    left: 0;
    z-index: 1;
  }
  .leaflet-container { 
    background: #ececec !important; 
    height: 100% !important;
    width: 100% !important;
  }
  .leaflet-tile-pane { filter: grayscale(10%) brightness(1.15); }
  .leaflet-control { font-size: 14px; }
  .panel-default {
    box-shadow: 0 2px 6px rgba(0,0,0,0.25);
    border-radius: 10px;
    z-index: 1000 !important;
  }
  .panel-default > .panel-heading {
    background-color: #4e73df;
    color: white;
    font-weight: bold;
  }
  .navbar {
    z-index: 1001 !important;
  }
  body {
    font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  }
  .shiny-plot-output {
    margin-top: 10px;
  }
  .panel-default .form-group {
    margin-bottom: 15px;
  }
  /* Add scrollbar to Data Explorer tab's main panel */
  div.tab-pane[data-value='Data Explorer'] .data-explorer-main {
  overflow-y: auto !important;
  max-height: 80vh !important;
  padding: 15px;
}
  }
"))
  ),
  
  navbarPage("Annuarium Statisticum Ecclesiae", theme = shinytheme("flatly"),
             
             # MAP TAB
             tabPanel("Map",
                      div(
                        leafletOutput("map", height = "100vh", width = "100%"),
                        absolutePanel(
                          id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                          width = 300, height = "auto",
                          style = "background-color: rgba(255,255,255,0.8); padding: 10px; border-radius: 10px; overflow-y: auto; max-height: 90vh;",
                          
                          h4("World Stats Explorer"),
                          selectInput("variable", "Select variable to display:",
                                      choices = setdiff(names(data_countries)[sapply(data_countries, is.numeric) & names(data_countries) != "Year"], "Area.in.km.2")),
                          selectInput("year", "Select year:",
                                      choices = sort(unique(data_countries$Year)), selected = max(data_countries$Year)),
                          
                          
                          selectizeInput("country_search", "Search for a country:",
                                         choices = final_country_dropdown_choices, # <-- USE THE NEW VARIABLE HERE
                                         selected = "", multiple = FALSE,
                                         options = list(placeholder = 'Type to search...')),
                          
                          
                          plotOutput("varPlot", height = 150),
                          hr(),
                          htmlOutput("country_info"),
                          actionButton("reset_map", "Reset Map View", icon = icon("undo"))
                          
                        )
                      )
             ),
             
             # DATA TABLE TAB
             tabPanel("Data Explorer",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          tags$div(
                            style = "background-color: #f8f9fa; border-radius: 8px; padding: 15px; border: 1px solid #dee2e6; font-size: 14px;",
                            selectInput("explorer_variable", "Select variable:",
                                        choices = c("Select a variable..." = "", 
                                                    setdiff(names(data_countries)[sapply(data_countries, is.numeric)], "Year"))
                            ),
                            selectInput("explorer_year", "Select year:",
                                        choices = sort(unique(data_countries$Year))
                            ),
                            actionButton("reset_table", "Reset Filters", icon = icon("redo"))
                          )
                        ),
                        mainPanel(
                          class = "data-explorer-main",
                          width = 9,
                          DTOutput("table")
                        )
                      )
             )
             
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
    library(viridisLite)  # at top if not yet loaded
    pal <- colorNumeric(palette = RColorBrewer::brewer.pal(7, "Purples"), 
                        domain = filtered_data[[input$variable]], na.color = "transparent")
    
    # Build map
    leaflet(filtered_data, options = leafletOptions(maxBounds = list(c(-120, -240), c(120, 240)), maxBoundsViscosity = 1)) %>%
      addProviderTiles("CartoDB.Voyager", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 0, lat = 30, zoom = 3) %>%
      addPolygons(
        fillColor = ~pal(filtered_data[[input$variable]]),
        weight = 1,
        opacity = 0.45,  # Reduced to make borders less opaque
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,  # Reduced to allow labels to show through
        layerId = ~name,
        label = ~lapply(paste0("<strong>", name, "</strong><br/>", 
                               input$variable, ": ", 
                               formatC(filtered_data[[input$variable]], format = "f", digits = 0, big.mark = ",")), htmltools::HTML),
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.8,  # Adjusted highlight opacity
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = filtered_data[[input$variable]],
        title = paste("Number in", input$year),
        position = "bottomright"
      )
  })
  
  # Create a reactive value to store the selected country
  selected_country <- reactiveVal(NULL)
  
  # Store selections for syncing between tabs
  selections <- reactiveValues(variable = NULL, year = NULL)
  
  # Keep track of current selections in Map tab
  observe({
    selections$variable <- input$variable
    selections$year <- input$year
  })
  
  # Sync Data Explorer inputs with Map tab
  observeEvent(input$variable, {
    updateSelectInput(session, "explorer_variable", selected = input$variable)
  })
  
  observeEvent(input$year, {
    updateSelectInput(session, "explorer_year", selected = input$year)
  })
  
  # When a country is clicked on the map
  observeEvent(input$map_shape_click, {
    selected_country(input$map_shape_click$id)
    
    # Update the dropdown with the clicked country
    updateSelectInput(session, "country_search", selected = input$map_shape_click$id)
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = map_data %>% filter(name == input$map_shape_click$id, Year == input$year),
        fill = FALSE,
        color = "red",
        weight = 3,
        opacity = 1,
        group = "highlight"
      ) %>%
      setView(
        lng = st_coordinates(st_centroid(st_union(map_data %>%
                                                    filter(name == input$map_shape_click$id))))[1],
        lat = st_coordinates(st_centroid(st_union(map_data %>%
                                                    filter(name == input$map_shape_click$id))))[2],
        zoom = 4
      )
  })
  
  # When a country is selected from the dropdown
  observeEvent(input$country_search, {
    req(input$country_search, input$year)
    
    selected_country(input$country_search)
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = map_data %>% filter(name == input$country_search, Year == input$year),
        fill = FALSE,
        color = "red",
        weight = 3,
        opacity = 1,
        group = "highlight"
      ) %>%
      setView(
        lng = st_coordinates(st_centroid(st_union(map_data %>%
                                                    filter(name == input$country_search))))[1],
        lat = st_coordinates(st_centroid(st_union(map_data %>%
                                                    filter(name == input$country_search))))[2],
        zoom = 4
      )
  })
  
  
  
  observeEvent(input$reset_map, {
    selected_country(NULL)
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      setView(lng = 0, lat = 30, zoom = 3)
    
    updateSelectInput(session, "country_search", selected = "")
  })
  
  
  
  
  # Create an output that shows info about the selected country and variable
  output$country_info <- renderUI({
    req(selected_country())
    
    info <- map_data %>%
      filter(name == selected_country(), Year == input$year) %>%
      select(name, all_of(input$variable))
    
    if (nrow(info) == 0 || is.na(info[[input$variable]][1])) {
      HTML(paste0("<strong>", selected_country(), "</strong><br/>No data available"))
    } else {
      value <- info[[input$variable]][1]
      formatted_value <- format(round(as.numeric(value), 0), big.mark = ",", scientific = FALSE)
      HTML(paste0("<strong>", selected_country(), "</strong><br/>", input$variable, ": ", formatted_value))
    }
  })
  
  
  
  #Data Explorer
  output$table <- renderDT({
    if (is.null(input$explorer_variable) || input$explorer_variable == "") {
      return(datatable(data.frame(Message = "Please select a variable to explore.")))
    }
    
    req(input$explorer_year)
    
    filtered <- data_countries %>%
      filter(Year == input$explorer_year) %>%
      select(country, Year, all_of(input$explorer_variable))
    
    if (!is.null(selected_country()) && selected_country() %in% filtered$country) {
      filtered <- filtered %>% filter(country == selected_country())
    }
    
    datatable(filtered, options = list(pageLength = 20))
  })
  
  
  #To display only the available years for each variable
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    
    available_years <- sort(unique(data_countries %>%
                                     filter(!is.na(.data[[input$explorer_variable]])) %>%
                                     pull(Year)))
    
    updateSelectInput(session, "explorer_year",
                      choices = available_years,
                      selected = max(available_years))
  })
  
  
  observeEvent(input$reset_table, {
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
    selected_country(NULL)
  })
  
  #Histogram of the variable displayed
  output$varPlot <- renderPlot({
    req(input$variable, input$year)
    
    filtered <- data_countries %>% filter(Year == input$year)
    values <- filtered[[input$variable]]
    
    # Check if values are all NA
    if (all(is.na(values))) {
      plot.new()
      text(0.5, 0.5, "No data available for this variable in selected year", cex = 1.2)
      return()
    }
    
    country_selected <- selected_country()
    selected_value <- NA
    
    if (!is.null(country_selected) && country_selected %in% filtered$country) {
      selected_value <- filtered %>%
        filter(country == country_selected) %>%
        pull(input$variable)
    }
    
    library(stringr)
    x_label <- str_wrap(input$variable, width = 30)
    plot_title <- str_wrap(paste("Distribution of", input$variable), width = 40)
    
    p <- ggplot(data.frame(values), aes(x = values)) +
      geom_density(fill = "#440154", alpha = 0.5, na.rm = TRUE) +
      labs(x = x_label, y = "Density", title = plot_title) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(size = 11),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 9)
      )
    
    if (!is.na(selected_value)) {
      p <- p + geom_vline(xintercept = selected_value, color = "red", size = 1.2)
    }
    
    p
  })
  
  
  
}


# ---- Launch the app ----
shinyApp(ui, server)
