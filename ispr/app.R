# ---- Load Required Libraries ----
# This section installs and loads all necessary packages for the Shiny app.
# It checks if each package is installed and installs it if not, then loads them.

required_packages <- c(
  "shiny", "dplyr", "readr", "DT", "shinythemes",
  "RColorBrewer", "writexl", "plotly", "shinyjs", "ggplot2"
)
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load the libraries after ensuring they are installed.
lapply(required_packages, library, character.only = TRUE)
useShinyjs()


# ---- Docker Instructions ----
# Set Shiny app to listen on all interfaces and a specific port for Docker compatibility.

options(shiny.host = "0.0.0.0")
options(shiny.port = 3838) # Also 8180 is a valid option


# ---- Load the Data ----
# Set your working directory and read the data file.

# Define the data file path and set it as your working directory.
path_outputs <- "C:/Users/schia/Documents/LMU/Consulting/App"
#path_outputs <- "C:\\Users\\soffi\\Desktop\\CONSULTING"
setwd(path_outputs)

# Read the CSV file containing the data
data <- read.csv("final_ispr_men_table.csv", check.names = FALSE)


# ---- Define Variable Abbreviations ----
# Read variable abbreviations from a CSV file and create a named vector.
# The CSV file should have two columns: 'variable_name' and 'abbreviation'.

abbreviations_file <- file.path(path_outputs, "variable_abbreviations.csv")
if (!file.exists(abbreviations_file)) {
  stop("Variable abbreviations CSV file not found at: ", abbreviations_file)
}
abbreviations_df <- read.csv(abbreviations_file, stringsAsFactors = FALSE, check.names = FALSE)
variable_abbreviations <- setNames(abbreviations_df$abbreviation, abbreviations_df$variable_name)


# ---- Data Processing ----
# Identify numeric columns excluding 'Year' and 'Categories of Institutes'.
num_cols <- names(data)[sapply(data, is.numeric)]
num_cols <- setdiff(num_cols, "Year")

# Aggregate or clean data if needed, but since no countries, use as is.
# Ensure 'Year' is integer.
data <- data %>%
  mutate(Year = as.integer(Year))


# ---- Identify Time Series Variables ----
# Filter variables that have data for more than one year for time series use.

time_series_vars <- num_cols[
  sapply(num_cols, function(var) {
    years <- data %>%
      filter(!is.na(.data[[var]])) %>%
      pull(Year) %>%
      unique()
    length(years) > 1
  })
]


# ---- UI Layout ----
# Prepare category choices for the selector.
all_categories <- sort(unique(data$`Categories of Institutes`))
category_choices_list <- as.list(all_categories)
names(category_choices_list) <- all_categories
final_category_dropdown_choices <- category_choices_list

# Helper function to create select inputs for variables, years, or categories.
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

# Helper function to create download data for CSV/Excel handlers.
create_download_data <- function(data, year, variable, selected_category) {
  filtered <- data %>%
    filter(Year == year) %>%
    select(`Categories of Institutes`, Year, all_of(variable))
  if (!is.null(selected_category) && selected_category %in% filtered$`Categories of Institutes`) {
    filtered <- filtered %>% filter(`Categories of Institutes` == selected_category)
  }
  filtered
}

# Define the Shiny UI with custom styles and layout.
ui <- tagList(
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden;
      }
      .navbar {
        z-index: 1001 !important;
      }
      body {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      .plotly, .js-plotly-plot, .plotly text {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      .shiny-plot-output {
        margin-top: 10px;
      }
      div.tab-pane[data-value='Time Series'] .ts-wrap {
        height: calc(100vh - 150px);
      }
      div.tab-pane[data-value='Yearly Snapshot'] .ts-wrap {
        height: calc(100vh - 150px);
      }
      .ts-sidebar {
        max-height: calc(100vh - 100px);
        overflow-y: auto;
        background-color: #f8f9fa;
        border-radius: 8px;
        border: 1px solid #dee2e6;
        padding: 15px;
      }
      div.tab-pane[data-value='Data Explorer'] .data-explorer-main {
        overflow-y: auto !important;
        max-height: 80vh !important;
        padding: 15px;
      }
      #ts_variable, #ys_variable {
        size: 15; /* Show more options in dropdown */
        max-height: 400px;
        overflow-y: auto;
      }
    ")), useShinyjs()
  ),
  
  navbarPage("ISPR Men Statistics", id = "navbar", theme = shinytheme("flatly"),
             
             tabPanel("Time Series",
                      fluidRow(
                        column(
                          width = 3,
                          class = "ts-sidebar",
                          style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                          create_select_input("ts_variable", "Select variable:", time_series_vars, selected = time_series_vars[1]),
                          div(style = "margin-top: 10px;",
                              downloadButton("download_ts_plot", "Download Plot", class = "btn btn-sm btn-primary"),
                              actionButton("reset_ts", "Reset", icon = icon("undo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        column(
                          width = 9,
                          div(class = "ts-wrap",
                              plotlyOutput("ts_plot", height = "100%")
                          )
                        )
                      )
             ),
             
             # Data Explorer Tab
             tabPanel("Data Explorer",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          tags$div(
                            style = "background-color: #f8f9fa; border-radius: 8px; padding: 15px; border: 1px solid #dee2e6; font-size: 14px;",
                            create_select_input("explorer_variable", "Select variable:", c("Select a variable..." = "", time_series_vars)),
                            create_select_input("explorer_year", "Select year:", sort(unique(data$Year))),
                            create_select_input("explorer_category", "Search for a category:", c("Type to search..." = "", final_category_dropdown_choices), selected = "", multiple = FALSE, placeholder = "Type to search..."),
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
             
             # Yearly Snapshot Tab
             tabPanel("Yearly Snapshot",
                      fluidRow(
                        column(
                          width = 3,
                          class = "ts-sidebar",
                          style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                          create_select_input("ys_variable", "Select variable:", time_series_vars, selected = time_series_vars[1]),
                          div(style = "margin-top: 10px;",
                              downloadButton("download_ys_plot", "Download Plot", class = "btn btn-sm btn-primary"),
                              actionButton("reset_ys", "Reset", icon = icon("undo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        column(
                          width = 9,
                          div(class = "ts-wrap",
                              plotlyOutput("ys_plot", height = "100%")
                          )
                        )
                      )
             )
  )
)


# ---- Server Logic ----
# Define the server function for the Shiny app.
server <- function(input, output, session) {
  
  # ---- Initialize Reactive Values ----
  # Reactive value for selected category.
  selected_category <- reactiveVal(NULL)
  # Reactive values for synchronizing selections across tabs.
  selections <- reactiveValues(
    variable = NULL,
    year = NULL,
    from_tab = NULL # Track which tab triggered the change
  )
  
  # ---- Time Series Plot Data ----
  plot_data_reactive <- reactive({
    req(input$ts_variable)
    
    filtered_data <- data %>%
      select(`Categories of Institutes`, Year, !!sym(input$ts_variable)) %>%
      rename(category = `Categories of Institutes`, value = !!sym(input$ts_variable)) %>%
      filter(!is.na(value))
    
    filtered_data
  })
  
  # ---- Render Time Series Plot ----
  # Generate Plotly line chart for time series data.
  output$ts_plot <- renderPlotly({
    plot_data <- plot_data_reactive()
    
    if (nrow(plot_data) == 0) {
      return(
        plot_ly() %>%
          add_trace(x = numeric(0), y = numeric(0),
                    type = "scatter", mode = "lines+markers",
                    showlegend = FALSE, hoverinfo = "skip") %>%
          layout(title = "No data available for the selected variable")
      )
    }
    
    plot_ly(
      data = plot_data,
      x = ~Year,
      y = ~value,
      color = ~category,
      colors = RColorBrewer::brewer.pal(max(3, length(unique(plot_data$category))), "Set2"),
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "<b>", category, "</b><br>",
        "Year: ", Year, "<br>",
        "Value: ", round(value, 2)
      ),
      line = list(width = 2),
      marker = list(size = 6, opacity = 0.8)
    ) %>%
      layout(
        title = paste("Time Series of", input$ts_variable),
        hovermode = "closest",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Absolute Value"),
        legend = list(title = list(text = "Categories"))
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ---- Time Series Download Button ----
  # Create a static ggplot for downloading the time series plot.
  plot_ts_static <- reactive({
    plot_data <- plot_data_reactive()
    
    if (nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes(x = Year, y = value, color = category)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(title = paste("Time Series of", input$ts_variable),
           x = "Year", y = "Absolute Value", color = "Categories") +
      theme_minimal(base_size = 13) +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            panel.background = element_rect(fill = "white", colour = "white"),
            legend.position = "bottom")
  })
  
  output$download_ts_plot <- downloadHandler(
    filename = function() {
      paste0("time_series_", input$ts_variable, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_ts_static(), width = 10, height = 6, dpi = 300)
    }
  )
  
  # ---- Yearly Snapshot Plot Data ----
  ys_plot_data_reactive <- reactive({
    req(input$ys_variable)
    
    filtered_data <- data %>%
      filter(Year == 2022) %>%
      select(`Categories of Institutes`, Year, !!sym(input$ys_variable)) %>%
      rename(category = `Categories of Institutes`, value = !!sym(input$ys_variable)) %>%
      filter(!is.na(value))
    
    filtered_data
  })
  
  # ---- Render Yearly Snapshot Histogram ----
  # Generate Plotly bar chart for 2022 data.
  output$ys_plot <- renderPlotly({
    plot_data <- ys_plot_data_reactive()
    
    if (nrow(plot_data) == 0) {
      return(
        plot_ly() %>%
          add_bars(x = character(0), y = numeric(0),
                   showlegend = FALSE, hoverinfo = "skip") %>%
          layout(title = "No data available for the selected variable in 2022")
      )
    }
    
    plot_ly(
      data = plot_data,
      x = ~category,
      y = ~value,
      type = "bar",
      color = ~category,
      colors = RColorBrewer::brewer.pal(max(3, length(unique(plot_data$category))), "Set2"),
      hoverinfo = "text",
      text = ~paste0(
        "<b>", category, "</b><br>",
        "Year: 2022<br>",
        "Value: ", round(value, 2)
      ),
      textposition = "none",
      showlegend = TRUE
    ) %>%
      layout(
        title = paste("Yearly Snapshot of", input$ys_variable, "in 2022"),
        hovermode = "closest",
        xaxis = list(title = "Categories of Institutes"),
        yaxis = list(title = "Absolute Value"),
        legend = list(title = list(text = "Categories"), x = 1.02, y = 1, xanchor = "left", yanchor = "top"),
        margin = list(r = 150)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ---- Yearly Snapshot Download Button ----
  # Create a static ggplot for downloading the yearly snapshot histogram.
  ys_plot_static <- reactive({
    plot_data <- ys_plot_data_reactive()
    
    if (nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes(x = category, y = value, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set2") +
      labs(title = paste("Yearly Snapshot of", input$ys_variable, "in 2022"),
           x = "Categories of Institutes", y = "Absolute Value") +
      theme_minimal(base_size = 13) +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            panel.background = element_rect(fill = "white", colour = "white"),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$download_ys_plot <- downloadHandler(
    filename = function() {
      paste0("yearly_snapshot_", input$ys_variable, "_2022_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = ys_plot_static(), width = 10, height = 6, dpi = 300)
    }
  )
  
  # ---- Render Data Table for Explorer Tab ----
  # Display data table for selected variable, year, and optional category.
  output$table <- renderDT({
    if (is.null(input$explorer_variable) || input$explorer_variable == "") {
      return(datatable(data.frame(Message = "Please select a variable to explore.")))
    }
    req(input$explorer_year)
    
    filtered <- data %>%
      filter(Year == input$explorer_year) %>%
      select(`Categories of Institutes`, Year, !!input$explorer_variable)
    
    if (!is.null(input$explorer_category) && input$explorer_category != "" && input$explorer_category %in% filtered$`Categories of Institutes`) {
      filtered <- filtered %>% filter(`Categories of Institutes` == input$explorer_category)
    }
    
    datatable(filtered, options = list(pageLength = 20))
  })
  
  # ---- Synchronize Variable and Year Selections ----
  # Update from Time Series tab.
  observeEvent(input$ts_variable, {
    if (is.null(selections$from_tab) || selections$from_tab != "time_series") {
      selections$variable <- input$ts_variable
      selections$from_tab <- "time_series"
      updateSelectInput(session, "explorer_variable", selected = input$ts_variable)
      updateSelectInput(session, "ys_variable", selected = input$ts_variable)
    }
  })
  
  # Update from Data Explorer tab.
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    if (input$explorer_variable != "" && (is.null(selections$from_tab) || selections$from_tab != "explorer")) {
      selections$variable <- input$explorer_variable
      selections$from_tab <- "explorer"
      updateSelectInput(session, "ts_variable", selected = input$explorer_variable)
      updateSelectInput(session, "ys_variable", selected = input$explorer_variable)
    }
  })
  
  # Update from Yearly Snapshot tab.
  observeEvent(input$ys_variable, {
    if (is.null(selections$from_tab) || selections$from_tab != "yearly_snapshot") {
      selections$variable <- input$ys_variable
      selections$from_tab <- "yearly_snapshot"
      updateSelectInput(session, "explorer_variable", selected = input$ys_variable)
      updateSelectInput(session, "ts_variable", selected = input$ys_variable)
    }
  })
  
  # ---- Sync Selections on Tab Switch ----
  # Ensure tabs reflect the latest variable selection when activated.
  observeEvent(input$navbar, {
    valid_variable <- if (!is.null(selections$variable) && selections$variable %in% time_series_vars) {
      selections$variable
    } else {
      time_series_vars[1]
    }
    if (input$navbar == "Time Series") {
      updateSelectInput(session, "ts_variable", selected = valid_variable)
    } else if (input$navbar == "Yearly Snapshot") {
      updateSelectInput(session, "ys_variable", selected = valid_variable)
    } else if (input$navbar == "Data Explorer") {
      updateSelectInput(session, "explorer_variable", selected = valid_variable)
    }
  })
  
  # ---- Update Available Years for Explorer Tab ----
  # Dynamically update years based on variable data availability.
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    available_years <- sort(unique(data %>%
                                     filter(!is.na(.data[[input$explorer_variable]])) %>%
                                     pull(Year)))
    updateSelectInput(session, "explorer_year", choices = available_years, selected = max(available_years))
  })
  
  # ---- Handle Category Search Selection ----
  observeEvent(input$explorer_category, {
    req(input$explorer_category)
    selected_category(input$explorer_category)
  })
  
  # ---- Reset Table Filters ----
  observeEvent(input$reset_table, {
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data$Year))
    updateSelectInput(session, "explorer_category", selected = "")
    selected_category(NULL)
  })
  
  # ---- Time Series Reset Button ----
  # Reset time series selections to defaults.
  observeEvent(input$reset_ts, {
    updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
    shiny::invalidateLater(100, session)
  })
  
  # ---- Yearly Snapshot Reset Button ----
  # Reset yearly snapshot selections to defaults.
  observeEvent(input$reset_ys, {
    updateSelectInput(session, "ys_variable", selected = time_series_vars[1])
    shiny::invalidateLater(100, session)
  })
  
  # ---- Download Data as CSV ----
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("data_explorer_", input$explorer_variable, "_", input$explorer_year, ".csv")
    },
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      write.csv(create_download_data(data, input$explorer_year, input$explorer_variable, selected_category()), file, row.names = FALSE)
    }
  )
  
  # ---- Download Data as Excel ----
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("data_explorer_", input$explorer_variable, "_", input$explorer_year, ".xlsx")
    },
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      writexl::write_xlsx(create_download_data(data, input$explorer_year, input$explorer_variable, selected_category()), path = file)
    }
  )
}


# ---- Launch the Shiny App ----
shinyApp(ui, server)
