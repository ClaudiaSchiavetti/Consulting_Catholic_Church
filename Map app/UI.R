# ---- UI Layout ----
# Prepare country choices for the search input.

all_countries <- sort(unique(data_countries$country))
all_countries <- all_countries[!is.na(all_countries) & all_countries != ""]
country_choices_list <- as.list(all_countries)
names(country_choices_list) <- all_countries
final_country_dropdown_choices <- c("Type to search..." = "", country_choices_list)

# ---- Helper Functions for UI ----
# Helper function to create select inputs for variables, years, or countries.
create_select_input <- function(id, label, choices, selected = NULL, multiple = FALSE, placeholder = NULL) {
  if (!is.null(placeholder)) {
    selectizeInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple,
      options = list(placeholder = placeholder)
    )
  } else {
    selectInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple
    )
  }
}

# Helper function to create download buttons for CSV and Excel.
create_download_buttons <- function() {
  div(
    style = "margin-top: 10px;",
    downloadButton("download_csv", "CSV", class = "btn btn-sm btn-success"),
    downloadButton("download_excel", "Excel", class = "btn btn-sm btn-info")
  )
}

# ---- Helper Functions for Server Logic ----
# Helper function to format values for display (e.g., map hover labels, country info).
format_value <- function(value, mode) {
  ifelse(mode != "absolute" & value == 0,
         "<0.01",
         formatC(round(as.numeric(value), ifelse(mode == "absolute", 0, 2)), format = "f", digits = ifelse(mode == "absolute", 0, 2), big.mark = ","))
}

# Helper function to create color palette for map visualizations.
create_pal <- function(values) {
  valid_values <- values[!is.na(values)]
  if (length(valid_values) > 0) {
    colorNumeric(palette = viridisLite::plasma(256), domain = valid_values, na.color = "transparent")
  } else {
    colorNumeric(palette = viridisLite::plasma(256), domain = c(0, 1), na.color = "transparent")
  }
}

# Helper function to create download data for CSV/Excel handlers.
create_download_data <- function(data, year, variable, selected_country) {
  filtered <- data %>%
    filter(Year == year) %>%
    select(country, Year, all_of(variable))
  if (!is.null(selected_country) && selected_country %in% filtered$country) {
    filtered <- filtered %>% filter(country = selected_country)
  }
  filtered
}

# Define the Shiny UI with custom styles and layout.
ui <- tagList(
  tags$head(
    includeCSS("styles.css"),
    useShinyjs()
  ),
  
  navbarPage("Annuarium Statisticum Ecclesiae", id = "navbar", theme = shinytheme("flatly"),
             
             tabPanel("Map",
                      div(
                        leafletOutput("map", height = "100vh", width = "100%"),
                        absolutePanel(
                          id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                          width = 300, height = "auto",
                          style = "background-color: rgba(255,255,255,0.8); padding: 10px; border-radius: 10px; overflow-y: auto; max-height: 90vh;",
                          create_select_input("variable", "Select variable to display:", allowed_variables),
                          create_select_input("year", "Select year:", sort(unique(data_countries$Year)), selected = max(data_countries$Year)),
                          radioButtons("display_mode", "Display mode:",
                                       choices = list("Absolute values" = "absolute",
                                                      "Per thousand inhabitants" = "per_capita",
                                                      "Per thousand Catholics" = "per_catholic"),
                                       selected = "absolute"),
                          create_select_input("country_search", "Search for a country:", final_country_dropdown_choices, selected = "", multiple = FALSE, placeholder = "Type to search..."),
                          plotOutput("varPlot", height = 150),
                          hr(),
                          div(style = "margin-bottom: 15px;", htmlOutput("country_info")),
                          actionButton("reset_map", "Reset View", icon = icon("undo")),
                          div(style = "margin-top: 15px;",
                              downloadButton("download_map", "Download Map", class = "btn btn-primary")
                          )
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
                            create_select_input("explorer_variable", "Select variable:", c("Select a variable..." = "", allowed_variables)),
                            create_select_input("explorer_year", "Select year:", sort(unique(data_countries$Year))),
                            create_download_buttons(),
                            br(),
                            actionButton("reset_table", "Reset Filters", icon = icon("redo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        mainPanel(
                          class = "data-explorer-main",
                          width = 9,
                          DTOutput("table"),
                          br()
                        )
                      )
             ),
             
             # Time Series TAB
             tabPanel(
               "Time Series",
               tags$style(HTML("
    .ts-container {
      min-height: calc(100vh - 50px); /* Adjust 50px based on navbar height */
      display: flex;
      align-items: stretch;
      margin: 0;
      padding: 0;
    }
    .ts-sidebar {
      height: 100%;
      overflow-y: auto;
      background-color: #f8f9fa;
      padding: 15px;
      border-radius: 8px;
      border: 1px solid #dee2e6;
    }
    .ts-main {
      height: 100%;
      display: flex;
      flex-direction: column;
      overflow: hidden;
    }
    .ts-plot {
      flex: 1 1 auto;
      height: 100%;
    }
  ")),
               fluidRow(
                 class = "ts-container",
                 column(
                   width = 3,
                   class = "ts-sidebar",
                   create_select_input("ts_variable", "Select variable:", time_series_vars, selected = time_series_vars[1]),
                   radioButtons("ts_level", "Region level:",
                                choices = c("Continent" = "Macroregion", "Country" = "Country"),
                                selected = "Macroregion"),
                   uiOutput("ts_region_selector"),
                   div(
                     style = "margin-top: 10px;",
                     downloadButton("download_ts_plot", "Download Plot", class = "btn btn-sm btn-primary"),
                     actionButton("reset_ts", "Reset", icon = icon("undo"), class = "btn btn-sm btn-secondary")
                   )
                 ),
                 column(
                   width = 9,
                   class = "ts-main",
                   div(
                     class = "ts-plot",
                     plotlyOutput("ts_plot", height = "100%")
                   )
                 )
               )
             )
  )
)

