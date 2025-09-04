# ---- Load Required Libraries ----
required_packages <- c(
  "shiny", "dplyr", "readr", "DT", "shinythemes",
  "RColorBrewer", "writexl", "plotly", "shinyjs",
  "ggplot2", "viridis", "tibble", "tidyr"
)
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
lapply(required_packages, library, character.only = TRUE)
useShinyjs()

# ---- Docker Instructions ----
options(shiny.host = "0.0.0.0")
options(shiny.port = 3838)

# ---- Load the Data ----
#path_outputs <- "C:/Users/schia/Documents/GitHub/Consulting_Catholic_Church"
path_outputs <- "C:/Users/soffi/Documents/Consulting_Catholic_Church"
setwd(path_outputs)
data <- read.csv("final_ispr_women_table.csv", check.names = FALSE)

# ---- Define Variable Abbreviations ----
abbreviations_file <- file.path(path_outputs, "variable_abbreviations.csv")
if (!file.exists(abbreviations_file)) {
  stop("Variable abbreviations CSV file not found at: ", abbreviations_file)
}
abbreviations_df <- read.csv(abbreviations_file, stringsAsFactors = FALSE, check.names = FALSE)
variable_abbreviations <- setNames(abbreviations_df$abbreviation, abbreviations_df$variable_name)

# ---- Data Processing ----
num_cols <- names(data)[sapply(data, is.numeric)]
num_cols <- setdiff(num_cols, "Year")
data <- data %>% mutate(Year = as.integer(Year))

# ---- Identify All Variables and Time Series Variables ----
all_vars <- num_cols
time_series_vars <- num_cols[
  sapply(num_cols, function(var) {
    years <- data %>% filter(!is.na(.data[[var]])) %>% pull(Year) %>% unique()
    length(years) > 1
  })
]

# ---- WOMEN TAXONOMY (flat, 3 groups) ----
WOMEN_TOP_CATS <- c("Autonomous Houses", "Centralized Institutes", "Secular Institutes")

# ---- UI Helpers ----
all_categories <- sort(unique(data$`Categories of Institutes`))
category_choices_list <- as.list(all_categories); names(category_choices_list) <- all_categories
final_category_dropdown_choices <- category_choices_list
create_select_input <- function(id, label, choices, selected = NULL, multiple = FALSE, placeholder = NULL) {
  if (!is.null(placeholder)) {
    selectizeInput(id, label, choices = choices, selected = selected, multiple = multiple,
                   options = list(placeholder = placeholder))
  } else {
    selectInput(id, label, choices = choices, selected = selected, multiple = multiple)
  }
}
create_download_buttons <- function() {
  div(
    style = "margin-top: 10px;",
    downloadButton("download_csv", "CSV", class = "btn btn-sm btn-success"),
    downloadButton("download_excel", "Excel", class = "btn btn-sm btn-info")
  )
}
# keep the same signature; the last arg is ignored now (no congregation view)
create_download_data <- function(data, year, variable, selected_category, view_by_congregation_unused) {
  filtered <- data %>%
    filter(Year == year, `Categories of Institutes` %in% WOMEN_TOP_CATS) %>%
    select(`Categories of Institutes`, Year, all_of(variable))
  if (!is.null(selected_category) && selected_category %in% filtered$`Categories of Institutes`) {
    filtered <- filtered %>% filter(`Categories of Institutes` == selected_category)
  }
  filtered
}

# ---- UI ----
ui <- tagList(
  tags$head(
    tags$style(HTML("
      html, body { height: 100%; margin: 0; padding: 0; overflow: hidden; }
      .navbar { z-index: 1001 !important; }
      body, .plotly, .js-plotly-plot, .plotly text { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
      .shiny-plot-output { margin-top: 10px; }
      div.tab-pane[data-value='Time Series'] .ts-wrap,
      div.tab-pane[data-value='Yearly Snapshot'] .ts-wrap { height: calc(100vh - 150px); }
      .ts-sidebar {
        max-height: calc(100vh - 100px); overflow-y: auto; background-color: #f8f9fa;
        border-radius: 8px; border: 1px solid #dee2e6; padding: 15px;
      }
      div.tab-pane[data-value='Data Explorer'] .data-explorer-main {
        overflow-y: auto !important; max-height: 80vh !important; padding: 15px;
      }
      #ts_variable, #ys_variable { size: 15; max-height: 400px; overflow-y: auto; }
    ")),
    useShinyjs()
  ),
  
  navbarPage("ISPR Women Statistics", id = "navbar", theme = shinytheme("flatly"),
             
             tabPanel("Time Series",
                      fluidRow(
                        column(
                          width = 3, class = "ts-sidebar",
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
                              fluidRow(
                                column(12, htmlOutput("ts_breadcrumb"))
                              ),
                              plotlyOutput("ts_plot", height = "100%")
                          )
                        )
                      )
             ),
             
             tabPanel("Data Explorer",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          tags$div(
                            style = "background-color: #f8f9fa; border-radius: 8px; padding: 15px; border: 1px solid #dee2e6; font-size: 14px;",
                            create_select_input("explorer_variable", "Select variable:", c("Select a variable..." = "", all_vars)),
                            create_select_input("explorer_year", "Select year:", sort(unique(data$Year))),
                            create_select_input("explorer_category", "Search for a category:",
                                                c("Type to search..." = "", final_category_dropdown_choices),
                                                selected = "", multiple = FALSE, placeholder = "Type to search..."),
                            create_download_buttons(),
                            br(),
                            actionButton("reset_table", "Reset Filters", icon = icon("redo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        mainPanel(class = "data-explorer-main", width = 9, DTOutput("table"), br())
                      )
             ),
             
             tabPanel("Yearly Snapshot",
                      fluidRow(
                        column(
                          width = 3, class = "ts-sidebar",
                          style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                          create_select_input("ys_variable", "Select variable:", all_vars, selected = all_vars[1]),
                          create_select_input("ys_year", "Select year:", sort(unique(data$Year))),
                          div(style = "margin-top: 10px;",
                              downloadButton("download_ys_plot", "Download Plot", class = "btn btn-sm btn-primary"),
                              actionButton("reset_ys", "Reset", icon = icon("undo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        column(
                          width = 9,
                          div(class = "ts-wrap",
                              fluidRow(
                                column(12, htmlOutput("ys_breadcrumb"))
                              ),
                              plotlyOutput("ys_plot", height = "100%")
                          )
                        )
                      )
             )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Helper function to wrap text for titles
  wrap_title <- function(text, width = 50, sep = "<br>") {
    paste(strwrap(text, width = width), collapse = sep)
  }
  
  # Simple breadcrumbs (no drill-down now)
  output$ys_breadcrumb <- renderUI(HTML("<b>Path:</b> Yearly Snapshot"))
  output$ts_breadcrumb <- renderUI(HTML("<b>Path:</b> Time Series"))
  
  # ---- Time Series data (flat by 3 categories) ----
  ts_drill_data <- reactive({
    req(input$ts_variable)
    data %>%
      filter(`Categories of Institutes` %in% WOMEN_TOP_CATS) %>%
      select(`Categories of Institutes`, Year, !!sym(input$ts_variable)) %>%
      rename(category = `Categories of Institutes`, value = !!sym(input$ts_variable)) %>%
      filter(!is.na(value)) %>%
      group_by(Year, category) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  })
  
  plot_data_reactive <- reactive({
    ts_drill_data() %>% rename(label = category) %>% rename(category = label)
  })
  
  output$ts_plot <- renderPlotly({
    plot_data <- ts_drill_data()
    if (nrow(plot_data) == 0) {
      return(plot_ly() %>% layout(title = "No data available for the selected variable")
             %>% config(displayModeBar = FALSE, responsive = TRUE))
    }
    
    main_title <- paste("Time Series of", input$ts_variable)
    wrapped_title <- wrap_title(main_title, width = 50, sep = "<br>")
    num_br <- length(unlist(gregexpr("<br>", wrapped_title)))
    num_lines <- num_br + 1
    t_margin <- 30 + 20 * num_lines
    
    plot_ly(
      data = plot_data,
      x = ~Year, y = ~value, color = ~category,
      colors = viridis::viridis(n = length(unique(plot_data$category))),
      type = "scatter", mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0("<b>", category, "</b><br>Year: ", Year, "<br>Value: ", round(value, 2))
    ) %>%
      layout(
        title = list(text = wrapped_title),
        hovermode = "closest",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Absolute Value"),
        legend = list(title = list(text = "Categories"), x = 1.02, y = 1, xanchor = "left", yanchor = "top"),
        margin = list(r = 150, t = t_margin)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # Static TS download
  plot_ts_static <- reactive({
    plot_data <- plot_data_reactive()
    if (nrow(plot_data) == 0) return(NULL)
    main_title <- paste("Time Series of", input$ts_variable)
    wrapped_title <- wrap_title(main_title, width = 50, sep = "\n")
    ggplot(plot_data, aes(x = Year, y = value, color = category)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_viridis_d() +
      labs(title = wrapped_title,
           x = "Year", y = "Absolute Value", color = "Categories") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.position = "bottom"
      )
  })
  output$download_ts_plot <- downloadHandler(
    filename = function() paste0("time_series_", input$ts_variable, "_", Sys.Date(), ".png"),
    content = function(file) ggsave(file, plot = plot_ts_static(), width = 10, height = 6, dpi = 300)
  )
  
  # ---- Yearly Snapshot (flat by 3 categories) ----
  ys_year_data <- function(year, variable) {
    data %>%
      filter(Year == year, `Categories of Institutes` %in% WOMEN_TOP_CATS) %>%
      select(`Categories of Institutes`, !!sym(variable)) %>%
      rename(label = `Categories of Institutes`, value = !!sym(variable)) %>%
      filter(!is.na(value)) %>%
      group_by(label) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  }
  
  output$ys_plot <- renderPlotly({
    req(input$ys_variable, input$ys_year)
    dd <- ys_year_data(input$ys_year, input$ys_variable)
    if (nrow(dd) == 0) {
      return(plot_ly() %>% layout(title = "No data available for the selected variable and year")
             %>% config(displayModeBar = FALSE, responsive = TRUE))
    }
    
    main_title <- paste("Yearly Snapshot of", input$ys_variable, "in", input$ys_year)
    wrapped_title <- wrap_title(main_title, width = 50, sep = "<br>")
    num_br <- length(unlist(gregexpr("<br>", wrapped_title)))
    num_lines <- num_br + 1
    t_margin <- 30 + 20 * num_lines
    
    plot_ly(
      data = dd, x = ~label, y = ~value, type = "bar",
      color = ~label, colors = viridis::viridis(length(unique(dd$label))),
      hovertemplate = "<b>%{x}</b><br>Value: %{y:,}<extra></extra>"
    ) %>%
      layout(
        title = list(text = wrapped_title),
        hovermode = "closest",
        showlegend = FALSE,
        xaxis = list(title = "Categories of Institutes", tickangle = -15),
        yaxis = list(title = "Absolute Value"),
        margin = list(r = 20, t = t_margin, b = 60, l = 60)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  ys_plot_static <- reactive({
    req(input$ys_variable, input$ys_year)
    dd <- ys_year_data(input$ys_year, input$ys_variable)
    if (nrow(dd) == 0) return(NULL)
    dd$label <- factor(dd$label, levels = dd$label)
    main_title <- paste("Yearly Snapshot of", input$ys_variable, "in", input$ys_year)
    wrapped_title <- wrap_title(main_title, width = 50, sep = "\n")
    ggplot(dd, aes(x = label, y = value, fill = label)) +
      geom_col() +
      scale_fill_viridis_d(guide = "none") +
      labs(title = wrapped_title,
           x = "Categories of Institutes", y = "Absolute Value") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.text.x = element_text(angle = 15, hjust = 1)
      )
  })
  output$download_ys_plot <- downloadHandler(
    filename = function() paste0("yearly_snapshot_", input$ys_variable, "_", input$ys_year, "_", Sys.Date(), ".png"),
    content = function(file) ggsave(file, plot = ys_plot_static(), width = 10, height = 6, dpi = 300)
  )
  
  # ---- Data Explorer table ----
  selected_category <- reactiveVal(NULL)
  output$table <- renderDT({
    if (is.null(input$explorer_variable) || input$explorer_variable == "") {
      return(datatable(data.frame(Message = "Please select a variable to explore.")))
    }
    req(input$explorer_year)
    filtered <- data %>%
      filter(Year == input$explorer_year, `Categories of Institutes` %in% WOMEN_TOP_CATS) %>%
      select(`Categories of Institutes`, Year, !!input$explorer_variable)
    if (!is.null(input$explorer_category) && input$explorer_category != "" &&
        input$explorer_category %in% filtered$`Categories of Institutes`) {
      filtered <- filtered %>% filter(`Categories of Institutes` == input$explorer_category)
    }
    datatable(filtered, options = list(pageLength = 20))
  })
  
  # ---- Available variables for Yearly Snapshot by year ----
  ys_available_vars <- reactive({
    req(input$ys_year)
    num_cols[
      sapply(num_cols, function(var) {
        any(!is.na(
          data %>%
            filter(Year == input$ys_year, `Categories of Institutes` %in% WOMEN_TOP_CATS) %>%
            pull(.data[[var]])
        ))
      })
    ]
  })
  
  # ---- Synchronize Variable Selections ----
  selections <- reactiveValues(variable = NULL, year = NULL, category = NULL, from_tab_var = NULL, from_tab_year = NULL)
  
  observeEvent(list(input$ys_year), {
    available_vars <- ys_available_vars()
    selected_var <- if (!is.null(selections$variable) && selections$variable %in% available_vars) {
      selections$variable
    } else {
      available_vars[1]
    }
    updateSelectInput(session, "ys_variable", choices = available_vars, selected = selected_var)
  })
  
  observeEvent(input$ts_variable, {
    if (is.null(selections$from_tab_var) || selections$from_tab_var != "time_series") {
      selections$variable <- input$ts_variable
      selections$from_tab_var <- "time_series"
      updateSelectInput(session, "explorer_variable", selected = input$ts_variable)
      if (input$ts_variable %in% ys_available_vars()) {
        updateSelectInput(session, "ys_variable", selected = input$ts_variable)
      }
    }
  })
  
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    if (input$explorer_variable != "" && (is.null(selections$from_tab_var) || selections$from_tab_var != "explorer")) {
      selections$variable <- input$explorer_variable
      selections$from_tab_var <- "explorer"
      if (input$explorer_variable %in% ys_available_vars()) {
        updateSelectInput(session, "ys_variable", selected = input$explorer_variable)
      }
      if (input$explorer_variable %in% time_series_vars) {
        updateSelectInput(session, "ts_variable", selected = input$explorer_variable)
      }
    }
  })
  
  observeEvent(input$ys_variable, {
    if (is.null(selections$from_tab_var) || selections$from_tab_var != "yearly_snapshot") {
      selections$variable <- input$ys_variable
      selections$from_tab_var <- "yearly_snapshot"
      updateSelectInput(session, "explorer_variable", selected = input$ys_variable)
      if (input$ys_variable %in% time_series_vars) {
        updateSelectInput(session, "ts_variable", selected = input$ys_variable)
      }
    }
  })
  
  observeEvent(input$explorer_year, {
    if (is.null(selections$from_tab_year) || selections$from_tab_year != "explorer") {
      selections$year <- input$explorer_year
      selections$from_tab_year <- "explorer"
      req(input$ys_variable)
      available_ys <- sort(unique(data %>% filter(!is.na(.data[[input$ys_variable]])) %>% pull(Year)))
      if (input$explorer_year %in% available_ys) {
        updateSelectInput(session, "ys_year", selected = input$explorer_year)
      }
    }
  })
  
  observeEvent(input$ys_year, {
    if (is.null(selections$from_tab_year) || selections$from_tab_year != "yearly_snapshot") {
      selections$year <- input$ys_year
      selections$from_tab_year <- "yearly_snapshot"
      req(input$explorer_variable); req(input$explorer_variable != "")
      available_explorer <- sort(unique(data %>% filter(!is.na(.data[[input$explorer_variable]])) %>% pull(Year)))
      if (input$ys_year %in% available_explorer) {
        updateSelectInput(session, "explorer_year", selected = input$ys_year)
      }
    }
  })
  
  # ---- Category search selection ----
  observeEvent(input$explorer_category, {
    req(input$explorer_category)
    selected_category(input$explorer_category)
    selections$category <- input$explorer_category
  })
  
  # ---- Resets ----
  observeEvent(input$reset_table, {
    updateSelectInput(session, "explorer_variable", selected = "", choices = c("Select a variable..." = "", all_vars))
    updateSelectInput(session, "explorer_year", selected = max(data$Year))
    updateSelectInput(session, "explorer_category", selected = "")
    selected_category(NULL)
  })
  observeEvent(input$reset_ys, {
    updateSelectInput(session, "ys_variable", selected = ys_available_vars()[1])
    updateSelectInput(session, "ys_year", selected = max(data$Year))
    shiny::invalidateLater(100, session)
  })
  observeEvent(input$reset_ts, {
    updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
    shiny::invalidateLater(100, session)
  })
  
  # ---- Downloads ----
  output$download_csv <- downloadHandler(
    filename = function() paste0("data_explorer_", input$explorer_variable, "_", input$explorer_year, ".csv"),
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      write.csv(create_download_data(data, input$explorer_year, input$explorer_variable, selected_category(), FALSE),
                file, row.names = FALSE)
    }
  )
  output$download_excel <- downloadHandler(
    filename = function() paste0("data_explorer_", input$explorer_variable, "_", input$explorer_year, ".xlsx"),
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      writexl::write_xlsx(create_download_data(data, input$explorer_year, input$explorer_variable, selected_category(), FALSE),
                          path = file)
    }
  )
}

# ---- Launch ----
shinyApp(ui, server)