
# ---- Server Logic ----
# Helper function to standardize macroregion names for consistency in plotting.

TARGET_REGIONS <- c(
  "South and Far East Asia",
  "Africa",
  "Europe",
  "South America",
  "North America",
  "Middle East Asia",
  "Central America",
  "Oceania"
)
standardize_macro <- function(df, col = "macroregion") {
  df %>%
    filter(!.data[[col]] %in% c("World", "America", "Asia")) %>% # drop aggregates
    mutate(
      macro_simplified = dplyr::case_when(
        .data[[col]] %in% c("Central America (Mainland)", "Central America (Antilles)") ~ "Central America",
        .data[[col]] %in% c("South East and Far East Asia", "South & Far East Asia") ~ "South and Far East Asia",
        .data[[col]] == "Middle East" ~ "Middle East Asia",
        TRUE ~ .data[[col]]
      )
    ) %>%
    filter(macro_simplified %in% TARGET_REGIONS) %>%
    mutate(macro_simplified = factor(macro_simplified, levels = TARGET_REGIONS))
}

# Define the server function for the Shiny app.
server <- function(input, output, session) {
  
  # ---- Initialize Reactive Values ----
  # Reactive value for selected country.
  selected_country <- reactiveVal(NULL)
  # Reactive values for synchronizing selections across tabs.
  selections <- reactiveValues(
    variable = NULL,
    year = NULL,
    from_tab = NULL # Track which tab triggered the change
  )
  
  # Reactive data based on geographic level
  current_data <- reactive({
    if (input$geographic_level == "countries") {
      data_countries
    } else {
      data_macroregions
    }
  })
  
  # Reactive map data based on geographic level  
  current_map_data <- reactive({
    if (input$geographic_level == "countries") {
      map_data
    } else {
      map_data_macroregions
    }
  })
  
  # Reactive allowed variables based on geographic level
  current_allowed_variables <- reactive({
    if (input$geographic_level == "countries") {
      allowed_variables
    } else {
      allowed_variables_macroregions
    }
  })
  
  # Reactive for display mode label.
  mode_label <- reactive({
    if (as.integer(input$year) == 2022) input$display_mode else "absolute"
  })
  
  # Reactive for filtered map data
  filtered_map_data <- reactive({
    req(input$variable, input$year, input$geographic_level)
    data <- current_map_data() %>% filter(Year == input$year)
    
    if (as.integer(input$year) == 2022) {
      if (input$display_mode == "per_capita") {
        data[[input$variable]] <- ifelse(
          !is.na(data[["Inhabitants in thousands"]]) & data[["Inhabitants in thousands"]] > 0,
          data[[input$variable]] / data[["Inhabitants in thousands"]],
          NA_real_
        )
      } else if (input$display_mode == "per_catholic") {
        data[[input$variable]] <- ifelse(
          !is.na(data[["Catholics in thousands"]]) & data[["Catholics in thousands"]] > 0,
          data[[input$variable]] / data[["Catholics in thousands"]],
          NA_real_
        )
      }
    }
    data
  })
  
  # Helper function to update time series for a selected country.
  update_time_series_for_country <- function(country) {
    req(country)
    if (country %in% data_countries$country) {
      updateRadioButtons(session, "ts_level", selected = "Country")
      shinyjs::delay(500, {
        updateSelectInput(session, "ts_regions", selected = country)
      })
    }
  }
  
  # ---- Synchronize Variable and Year Selections ----
  # Update from Map tab.
  observeEvent(input$variable, {
    if (is.null(selections$from_tab) || selections$from_tab != "explorer") {
      selections$variable <- input$variable
      selections$from_tab <- "map"
      updateSelectInput(session, "explorer_variable", selected = input$variable)
      if (input$variable %in% time_series_vars) {
        updateSelectInput(session, "ts_variable", selected = input$variable)
      } else {
        updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
      }
    }
  })
  
  # Update variable choices when geographic level changes
  observe({
    updateSelectInput(
      session,
      "variable",
      choices = current_allowed_variables(),
      selected = if (input$variable %in% current_allowed_variables()) input$variable else current_allowed_variables()[1]
    )
  })
  
  # Update from Data Explorer tab.
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    if (input$explorer_variable != "" && (is.null(selections$from_tab) || selections$from_tab != "map")) {
      selections$variable <- input$explorer_variable
      selections$from_tab <- "explorer"
      updateSelectInput(session, "variable", selected = input$explorer_variable)
      if (input$explorer_variable %in% time_series_vars) {
        updateSelectInput(session, "ts_variable", selected = input$explorer_variable)
      } else {
        updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
      }
    }
  })
  
  # Update from Time Series tab.
  observeEvent(input$ts_variable, {
    if (is.null(selections$from_tab) || selections$from_tab != "map") {
      selections$variable <- input$ts_variable
      selections$from_tab <- "time_series"
      updateSelectInput(session, "variable", selected = input$ts_variable)
      updateSelectInput(session, "explorer_variable", selected = input$ts_variable)
    }
  })
  
  # Synchronize years from Map tab.
  observeEvent(input$year, {
    if (is.null(selections$from_tab) || selections$from_tab != "explorer") {
      selections$year <- input$year
      updateSelectInput(session, "explorer_year", selected = input$year)
    }
  })
  
  # Synchronize years from Data Explorer tab.
  observeEvent(input$explorer_year, {
    if (is.null(selections$from_tab) || selections$from_tab != "map") {
      selections$year <- input$explorer_year
      updateSelectInput(session, "year", selected = input$explorer_year)
    }
  })
  
  # ---- Update Available Years Based on Variable ----
  # Dynamically update year choices based on data availability for the selected variable.
  observeEvent(input$variable, {
    available_years <- sort(unique(current_data() %>%
                                     filter(!is.na(.data[[input$variable]])) %>%
                                     pull(Year)))
    updateSelectInput(session, "year", choices = available_years, selected = max(available_years))
  })
  
  # ---- Limit Display Modes to 2022 ----
  # Restrict per capita/per Catholic modes to 2022 data only.
  observeEvent(input$year, {
    if (as.integer(input$year) == 2022) {
      updateRadioButtons(
        session, "display_mode",
        choices = list("Absolute values" = "absolute",
                       "Per thousand inhabitants" = "per_capita",
                       "Per thousand Catholics" = "per_catholic"),
        selected = if (input$display_mode %in% c("absolute", "per_capita", "per_catholic")) input$display_mode else "absolute"
      )
    } else {
      updateRadioButtons(
        session, "display_mode",
        choices = list("Absolute values" = "absolute"),
        selected = "absolute"
      )
    }
  })
  
  
  # Handle geographic level changes
  observeEvent(input$geographic_level, {
    if (input$geographic_level == "macroregions") {
      # Clear country selection and disable country search
      selected_country(NULL)
      updateSelectInput(session, "country_search", selected = "")
      leafletProxy("map") %>% clearGroup("highlight")
    }
  })
  
  # ---- Render Interactive World Map ----
  # Create the Leaflet map with selected variable data.
  output$map <- renderLeaflet({
    req(input$variable, input$year, input$geographic_level)
    ml <- mode_label()
    filtered_data <- filtered_map_data()
    
    # Ensure valid values for color palette
    pal <- create_pal(filtered_data[[input$variable]])
    
    # Create labels based on geographic level
    if (input$geographic_level == "countries") {
      region_name <- filtered_data$name
    } else {
      region_name <- filtered_data$macroregion
    }
    
    leaflet(filtered_data, options = leafletOptions(
      maxBounds = list(c(-120, -240), c(120, 240)),
      maxBoundsViscosity = 1,
      zoomControl = FALSE
    )) %>%
      addProviderTiles("CartoDB.Voyager", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 0, lat = 30, zoom = 3) %>%
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        L.control.zoom({ position: 'topright' }).addTo(map);
      }
    ") %>%
      addPolygons(
        fillColor = ~pal(filtered_data[[input$variable]]),
        weight = 1,
        opacity = 0.45,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.6,
        layerId = ~region_name,
        label = ~lapply(paste0("<strong>", region_name, "</strong><br/>",
                               switch(ml,
                                      "absolute" = variable_abbreviations[input$variable],
                                      "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."),
                                      "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                               ": ",
                               format_value(filtered_data[[input$variable]], ml)), htmltools::HTML),
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.8, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = filtered_data[[input$variable]],
                title = paste(switch(ml,
                                     "absolute" = variable_abbreviations[input$variable],
                                     "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."),
                                     "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                              "in", input$year),
                position = "bottomright")
  })
  
  # ---- Handle Map Download ----
  # Generate filename and content for downloading the map as PNG.
  output$download_map <- downloadHandler(
    filename = function() {
      ml <- mode_label()
      paste0("map_export_", input$variable, "_", input$year,
             switch(ml,
                    "absolute" = "",
                    "per_capita" = "_per_capita",
                    "per_catholic" = "_per_catholic"), ".png")
    },
    content = function(file) {
      ml <- mode_label()
      filtered_data <- filtered_map_data()
      pal <- create_pal(filtered_data[[input$variable]])
      leaflet_obj <- leaflet(filtered_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        fitBounds(-110, -40, 120, 65) %>%
        addPolygons(
          fillColor = ~pal(filtered_data[[input$variable]]),
          color = "white", weight = 1, opacity = 0.45, fillOpacity = 0.6,
          label = ~name
        ) %>%
        addLegend(pal = pal, values = filtered_data[[input$variable]],
                  title = paste(switch(ml,
                                       "absolute" = variable_abbreviations[input$variable],
                                       "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."),
                                       "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                                "in", input$year),
                  position = "bottomright") %>%
        addControl(
          html = paste0("<div style='font-size:20px; font-weight:bold; background-color:rgba(255,255,255,0.7);
                  padding:6px 12px; border-radius:6px;'>",
                        switch(ml,
                               "absolute" = input$variable,
                               "per_capita" = paste(input$variable, "per thousand inhabitants"),
                               "per_catholic" = paste(input$variable, "per thousand Catholics")),
                        " - ", input$year, "</div>"),
          position = "topright"
        ) %>%
        addControl(
          html = "<div style='font-size:13px; background-color:rgba(255,255,255,0.6); padding:4px 10px;
            border-radius:5px;'>Source: Annuarium Statisticum Ecclesiae</div>",
          position = "bottomleft"
        )
      temp_html <- tempfile(fileext = ".html")
      saveWidget(leaflet_obj, temp_html, selfcontained = TRUE)
      webshot::webshot(temp_html, file = file, vwidth = 1600, vheight = 1000)
    }
  )
  
  # ---- Time Series Region Selector UI ----
  # Dynamically render region selector based on level (Country or Macroregion).
  output$ts_region_selector <- renderUI({
    if (input$ts_level == "Country") {
      create_select_input("ts_regions", "Select country/countries:", sort(unique(data_countries$country)), multiple = TRUE)
    } else {
      create_select_input("ts_regions", "Displayed continents (remove any to filter):", TARGET_REGIONS, selected = TARGET_REGIONS, multiple = TRUE)
    }
  })
  
  # ---- Handle Map Click Events ----
  # Highlight clicked country and update selections.
  # ---- Handle Map Click Events ----
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id, input$year)
    
    if (identical(input$geographic_level, "macroregions")) {
      showNotification(
        paste("Selected macroregion:", input$map_shape_click$id),
        type = "message", duration = 3
      )
      return()
    }
    
    # Countries path
    clicked_country <- input$map_shape_click$id
    
    md <- current_map_data()                       # should return the sf for the current year/scope
    filtered_data <- md %>%
      dplyr::filter(name == clicked_country, Year == input$year)
    
    if (nrow(filtered_data) == 0 || !"geometry" %in% names(filtered_data)) {
      showNotification("No geometry/data for selection.", type = "error", duration = 3)
      return()
    }
    
    # update selections
    selected_country(clicked_country)  # your reactiveVal setter
    updateSelectInput(session, "country_search", selected = clicked_country)
    
    # update time series if applicable
    if (isTRUE(input$variable %in% time_series_vars)) {
      update_time_series_for_country(clicked_country)
      showNotification(paste("Time series updated to show", clicked_country),
                       type = "message", duration = 3)
    }
    
    # highlight + center
    geom_union <- tryCatch(sf::st_union(filtered_data$geometry), error = function(e) NULL)
    if (!is.null(geom_union)) {
      ctr <- tryCatch(
        sf::st_coordinates(sf::st_centroid(geom_union))[1:2],
        error = function(e) c(0, 30)
      )
      leafletProxy("map") %>%
        clearGroup("highlight") %>%
        addPolygons(
          data = filtered_data,
          fill = FALSE, color = "red", weight = 3, opacity = 1,
          group = "highlight"
        ) %>%
        setView(lng = ctr[1], lat = ctr[2], zoom = 4)
    } else {
      leafletProxy("map") %>% clearGroup("highlight")
    }
  })
  
  # ---- Handle Country Search Selection ----
  # Highlight selected country from search and update view.
  observeEvent(input$country_search, {
    req(input$country_search, input$year)
    selected_country(input$country_search)
    
    # Update time series only if variable is valid.
    if (input$variable %in% time_series_vars) {
      update_time_series_for_country(input$country_search)
    }
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = map_data %>% filter(name = input$country_search, Year = input$year),
        fill = FALSE, color = "red", weight = 3, opacity = 1, group = "highlight") %>%
      setView(
        lng = tryCatch(
          st_coordinates(st_centroid(st_union(map_data %>% filter(name = input$country_search))))[1],
          error = function(e) 0
        ),
        lat = tryCatch(
          st_coordinates(st_centroid(st_union(map_data %>% filter(name = input$country_search))))[2],
          error = function(e) 30
        ),
        zoom = 4
      )
  })
  
  # ---- Sync Selections on Tab Switch to Time Series ----
  # Ensure time series tab reflects current selections when activated.
  observeEvent(input$navbar, {
    if (input$navbar == "Time Series") {
      # Ensure variable is valid for time series.
      valid_variable <- if (!is.null(input$variable) && input$variable %in% time_series_vars) {
        input$variable
      } else {
        time_series_vars[1] # Fallback to first valid time series variable
      }
      
      # Update variable and level.
      updateSelectInput(session, "ts_variable", selected = valid_variable)
      if (!is.null(selected_country()) && selected_country() %in% data_countries$country) {
        updateRadioButtons(session, "ts_level", selected = "Country")
        # Use a longer delay to ensure UI is ready.
        shinyjs::delay(500, {
          updateSelectInput(session, "ts_regions", selected = selected_country())
        })
      } else {
        updateRadioButtons(session, "ts_level", selected = "Macroregion")
        shinyjs::delay(500, {
          updateSelectInput(session, "ts_regions", selected = TARGET_REGIONS)
        })
      }
    }
  })
  
  # ---- Reset Map View and Selections ----
  # Clear highlights and reset view on reset button click.
  observeEvent(input$reset_map, {
    selected_country(NULL)
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      setView(lng = 0, lat = 30, zoom = 3)
    updateSelectInput(session, "country_search", selected = "")
    updateRadioButtons(session, "display_mode", selected = "absolute")
    
    # Reset time series to default.
    updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
    updateRadioButtons(session, "ts_level", selected = "Macroregion")
    updateSelectInput(session, "ts_regions", selected = TARGET_REGIONS)
    
    # Reset data explorer.
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
  })
  
  # ---- Display Country-Specific Information ----
  # Render HTML with country info based on selection.
  output$country_info <- renderUI({
    ml <- mode_label()
    req(selected_country())
    info <- filtered_map_data() %>% filter(name == selected_country())
    if (nrow(info) == 0 || is.na(info[[input$variable]][1])) {
      HTML(paste0("<strong>", selected_country(), "</strong><br/>No data available"))
    } else {
      HTML(paste0("<strong>", selected_country(), "</strong><br/>",
                  switch(ml,
                         "absolute" = input$variable,
                         "per_capita" = paste(input$variable, "per thousand inhabitants"),
                         "per_catholic" = paste(input$variable, "per thousand Catholics")),
                  " in ", input$year, ": ", format_value(info[[input$variable]][1], ml)))
    }
  })
  
  # ---- Render Macroregion Histogram ----
  # Generate bar plot for continent-level distribution.
  output$varPlot <- renderPlot({
    
    ml <- mode_label()
    req(input$variable, input$year)
    
    filtered_macro <- data_macroregions %>%
      filter(Year == input$year) %>%
      mutate(macroregion = case_when(
        macroregion %in% c("Central America (Mainland)", "Central America (Antilles)") ~ "Central America",
        macroregion == "South East and Far East Asia" ~ "South & Far East Asia",
        TRUE ~ macroregion
      )) %>%
      filter(!macroregion %in% c("World", "America", "Asia")) %>%
      group_by(macroregion) %>%
      summarise(
        value = switch(ml,
                       "absolute" = sum(.data[[input$variable]], na.rm = TRUE),
                       "per_capita" = ifelse(
                         sum(.data[["Inhabitants in thousands"]], na.rm = TRUE) > 0,
                         sum(.data[[input$variable]], na.rm = TRUE) / sum(.data[["Inhabitants in thousands"]], na.rm = TRUE),
                         NA_real_
                       ),
                       "per_catholic" = ifelse(
                         sum(.data[["Catholics in thousands"]], na.rm = TRUE) > 0,
                         sum(.data[[input$variable]], na.rm = TRUE) / sum(.data[["Catholics in thousands"]], na.rm = TRUE),
                         NA_real_
                       )),
        .groups = "drop"
      )
    
    if (all(is.na(filtered_macro$value))) {
      plot.new()
      text(0.5, 0.5, "No data available at macroregion level", cex = 1.2)
      return()
    }
    
    ggplot(filtered_macro, aes(x = reorder(macroregion, value), y = value)) +
      geom_col(fill = "#f7f7f7", color = "gray80", linewidth = 0.3, alpha = 1) +
      geom_text(
        aes(label = ifelse(ml != "absolute" & value == 0, "<0.01",
                           scales::comma(value, accuracy = ifelse(ml == "absolute", 1, 0.01)))),
        hjust = -0.05, size = 2.9
      ) +
      coord_flip(clip = "off") +
      labs(
        x = "Continents",
        y = NULL,
        title = paste("Continent-level distribution", "in", input$year),
        caption = switch(ml,
                         "absolute" = variable_abbreviations[input$variable],
                         "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."),
                         "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath."))
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold", margin = margin(b = 10)),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#CCCCCC1A"),
        panel.grid.minor = element_line(colour = "#CCCCCC1A"),
        axis.text.y = element_text(size = 8)
      )
  })
  
  # ---- Render Time Series Plot ----
  # Generate Plotly line chart for time series data.
  output$ts_plot <- renderPlotly({
    req(input$ts_variable, input$ts_regions, input$ts_level)
    
    data_source <- if (input$ts_level == "Country") data_countries else data_macroregions
    region_col <- if (input$ts_level == "Country") "country" else "macroregion"
    
    plot_data <- data_source %>%
      filter(.data[[region_col]] %in% input$ts_regions) %>%
      select(Year, !!sym(region_col), !!sym(input$ts_variable)) %>%
      rename(region = !!sym(region_col), value = !!sym(input$ts_variable)) %>%
      filter(!is.na(value))
    if (input$ts_level == "Macroregion") {
      plot_data <- data_macroregions %>%
        standardize_macro("macroregion") %>%
        select(Year, macro_simplified, !!sym(input$ts_variable)) %>%
        rename(region = macro_simplified, value = !!sym(input$ts_variable)) %>%
        filter(region %in% input$ts_regions) %>%
        group_by(Year, region) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(value))
    }
    
    if (nrow(plot_data) == 0) {
      return(
        plot_ly() %>%
          add_trace(x = numeric(0), y = numeric(0),
                    type = "scatter", mode = "lines+markers",
                    showlegend = FALSE, hoverinfo = "skip") %>%
          layout(title = "No data available for the selected variable and region(s)")
      )
    }
    
    plot_ly(
      data = plot_data,
      x = ~Year,
      y = ~value,
      color = ~region,
      colors = RColorBrewer::brewer.pal(max(3, length(unique(plot_data$region))), "Set2"),
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "<b>", region, "</b><br>",
        "Year: ", Year, "<br>",
        "Value: ", round(value, 2)
      ),
      line = list(width = 2),
      marker = list(size = 6, opacity = 0.8)
    ) %>%
      layout(
        title = paste("Time Series of", variable_abbreviations[input$ts_variable]),
        hovermode = "closest",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Absolute Value"),
        legend = list(title = list(text = ifelse(input$ts_level == "Country", "Countries", "Continents")))
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ---- Time Series Reset Button ----
  # Reset time series selections to defaults.
  observeEvent(input$reset_ts, {
    updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
    updateRadioButtons(session, "ts_level", selected = "Macroregion")
    updateSelectInput(session, "ts_regions", selected = TARGET_REGIONS)
    
    # Clear country selection.
    selected_country(NULL)
    leafletProxy("map") %>% clearGroup("highlight")
    updateSelectInput(session, "country_search", selected = "")
    # Reset data explorer.
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
  })
  
  # ---- Time Series Download Button ----
  # Create a static ggplot for downloading the time series plot.
  plot_ts_static <- reactive({
    req(input$ts_variable, input$ts_regions, input$ts_level)
    
    data_source <- if (input$ts_level == "Country") data_countries else data_macroregions
    region_col <- if (input$ts_level == "Country") "country" else "macroregion"
    
    plot_data <- data_source %>%
      filter(.data[[region_col]] %in% input$ts_regions) %>%
      select(Year, !!sym(region_col), !!sym(input$ts_variable)) %>%
      rename(region = !!sym(region_col), value = !!sym(input$ts_variable))
    
    if (input$ts_level == "Macroregion") {
      plot_data <- data_macroregions %>%
        standardize_macro("macroregion") %>%
        select(Year, macro_simplified, !!sym(input$ts_variable)) %>%
        rename(region = macro_simplified, value = !!sym(input$ts_variable)) %>%
        filter(region %in% input$ts_regions) %>%
        group_by(Year, region) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    }
    
    ggplot(plot_data, aes(x = Year, y = value, color = region)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(title = paste("Time Series of", input$ts_variable),
           x = "Year", y = "Absolute Value", color = ifelse(input$ts_level == "Country", "Countries", "Continents")) +
      theme_minimal(base_size = 13) +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            panel.background = element_rect(fill = "white", colour = "white")) +
      theme(legend.position = "bottom")
  })
  
  output$download_ts_plot <- downloadHandler(
    filename = function() {
      paste0("time_series_", input$ts_variable, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_ts_static(), width = 10, height = 6, dpi = 300)
    }
  )
  
  
  # ---- Render Data Table for Explorer Tab ----
  # Display data table with optional per capita calculations for 2022.
  output$table <- renderDT({
    if (is.null(input$explorer_variable) || input$explorer_variable == "") {
      return(datatable(data.frame(Message = "Please select a variable to explore.")))
    }
    req(input$explorer_year)
    
    base_cols <- data_countries %>%
      filter(Year == input$explorer_year) %>%
      select(country, Year, !!input$explorer_variable,
             `Inhabitants in thousands`, `Catholics in thousands`)
    
    if (as.integer(input$explorer_year) == 2022) {
      filtered <- base_cols %>%
        mutate(
          `Per 1000 Inhabitants` = ifelse(
            !is.na(`Inhabitants in thousands`) & `Inhabitants in thousands` > 0,
            round(.data[[input$explorer_variable]] / `Inhabitants in thousands`, 3),
            NA_real_
          ),
          `Per 1000 Catholics` = ifelse(
            !is.na(`Catholics in thousands`) & `Catholics in thousands` > 0,
            round(.data[[input$explorer_variable]] / `Catholics in thousands`, 3),
            NA_real_
          )
        ) %>%
        select(-`Inhabitants in thousands`, -`Catholics in thousands`)
    } else {
      filtered <- base_cols %>%
        select(-`Inhabitants in thousands`, -`Catholics in thousands`)
    }
    
    if (!is.null(selected_country()) && selected_country() %in% filtered$country) {
      filtered <- filtered %>% filter(country = selected_country())
    }
    datatable(filtered, options = list(pageLength = 20))
  })
  
  # ---- Update Available Years for Explorer Tab ----
  # Dynamically update years based on variable data availability.
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    available_years <- sort(unique(data_countries %>%
                                     filter(!is.na(.data[[input$explorer_variable]])) %>%
                                     pull(Year)))
    updateSelectInput(session, "explorer_year", choices = available_years, selected = max(available_years))
  })
  
  # ---- Reset Table Filters ----
  # Clear selections in data explorer tab.
  observeEvent(input$reset_table, {
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
    selected_country(NULL)
    
    # Clear map highlight.
    leafletProxy("map") %>% clearGroup("highlight")
    updateSelectInput(session, "country_search", selected = "")
  })
  
  # ---- Download Data as CSV ----
  # Handler for CSV download from data explorer.
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("data_explorer_", input$explorer_variable, "_", input$explorer_year, ".csv")
    },
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      write.csv(create_download_data(data_countries, input$explorer_year, input$explorer_variable, selected_country()), file, row.names = FALSE)
    }
  )
  
  # ---- Download Data as Excel ----
  # Handler for Excel download from data explorer.
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("data_explorer_", input$explorer_variable, "_", input$explorer_year, ".xlsx")
    },
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      writexl::write_xlsx(create_download_data(data_countries, input$explorer_year, input$explorer_variable, selected_country()), path = file)
    }
  )
}

