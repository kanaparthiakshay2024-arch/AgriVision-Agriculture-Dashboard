# ============================================================
# AgriVision: India Agriculture Crop Production Dashboard
# R Shiny dashboard for CSI3005 Advanced Data Visualization
# ============================================================

# Install packages once if needed:


library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(viridis)
library(scales)

# ---- 1. Load the dataset ----

# Change this filename to the actual downloaded CSV filename if different.
DATA_FILE <- "crop_production.csv"

if (!file.exists(DATA_FILE)) {
  stop(
    paste0(
      "Cannot find '", DATA_FILE, "'. Download the Kaggle CSV, ",
      "put it beside app.R, and update DATA_FILE to its exact filename."
    )
  )
}

raw_data <- readr::read_csv(DATA_FILE, show_col_types = FALSE)

# ---- 2. Normalize column names and map common column variants ----
names(raw_data) <- names(raw_data) |>
  stringr::str_to_lower() |>
  stringr::str_replace_all("[^a-z0-9]+", "_") |>
  stringr::str_replace_all("^_|_$", "")

find_col <- function(candidates, data_names) {
  found <- candidates[candidates %in% data_names]
  if (length(found) == 0) return(NA_character_)
  found[[1]]
}

state_col <- find_col(c("state", "state_name"), names(raw_data))
district_col <- find_col(c("district", "district_name"), names(raw_data))
year_col <- find_col(c("crop_year", "year", "year_of_production"), names(raw_data))
season_col <- find_col(c("season", "crop_season"), names(raw_data))
crop_col <- find_col(c("crop", "crop_name"), names(raw_data))
production_col <- find_col(c("production", "crop_production", "production_tonnes"), names(raw_data))
area_col <- find_col(c("area", "area_hectares", "cultivated_area"), names(raw_data))
yield_col <- find_col(c("yield", "crop_yield", "yield_per_hectare"), names(raw_data))

required <- c(
  State = state_col, Year = year_col, Season = season_col,
  Crop = crop_col, Production = production_col
)
if (any(is.na(required))) {
  stop(
    paste0(
      "Some required columns were not found. Columns in your file are: ",
      paste(names(raw_data), collapse = ", "),
      ". Update the candidate names in the column-mapping section."
    )
  )
}

# Convert values carefully; retain rows even if optional fields are absent.
to_num <- function(x) suppressWarnings(readr::parse_number(as.character(x)))

agri <- tibble(
  state = as.character(raw_data[[state_col]]),
  year = to_num(raw_data[[year_col]]),
  season = as.character(raw_data[[season_col]]),
  crop = as.character(raw_data[[crop_col]]),
  production = to_num(raw_data[[production_col]]),
  district = if (!is.na(district_col)) as.character(raw_data[[district_col]]) else NA_character_,
  area = if (!is.na(area_col)) to_num(raw_data[[area_col]]) else NA_real_,
  yield_value = if (!is.na(yield_col)) to_num(raw_data[[yield_col]]) else NA_real_
) |>
  mutate(
    across(c(state, season, crop), ~ stringr::str_squish(.x)),
    state = na_if(state, ""),
    season = na_if(season, ""),
    crop = na_if(crop, ""),
    production = if_else(production < 0, NA_real_, production),
    area = if_else(area < 0, NA_real_, area),
    yield_value = if_else(yield_value < 0, NA_real_, yield_value)
  ) |>
  filter(!is.na(state), !is.na(year), !is.na(season), !is.na(crop)) |>
  mutate(
    # Calculate yield only if area and production are available and area > 0.
    yield_value = if_else(
      is.na(yield_value) & !is.na(area) & area > 0 & !is.na(production),
      production / area,
      yield_value
    )
  )

if (nrow(agri) == 0) stop("No usable rows remain after cleaning. Check the CSV columns and values.")

# ---- 3. UI ----
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "AgriVision | India"),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Geographic Analysis", tabName = "geography", icon = icon("map")),
      menuItem("Time & Season", tabName = "time", icon = icon("chart-line")),
      menuItem("Crop Relationships", tabName = "relationships", icon = icon("chart-scatter")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    ),
    hr(),
    h4("Dashboard Filters", style = "padding-left: 15px;"),
    selectInput(
      "states", "State(s)",
      choices = sort(unique(agri$state)),
      selected = sort(unique(agri$state)),
      multiple = TRUE
    ),
    selectInput(
      "crops", "Crop(s)",
      choices = sort(unique(agri$crop)),
      selected = head(sort(unique(agri$crop)), min(10, length(unique(agri$crop)))),
      multiple = TRUE
    ),
    selectInput(
      "seasons", "Season(s)",
      choices = sort(unique(agri$season)),
      selected = sort(unique(agri$season)),
      multiple = TRUE
    ),
    sliderInput(
      "years", "Year range",
      min = floor(min(agri$year, na.rm = TRUE)),
      max = ceiling(max(agri$year, na.rm = TRUE)),
      value = c(floor(min(agri$year, na.rm = TRUE)),
                ceiling(max(agri$year, na.rm = TRUE))),
      sep = ""
    ),
    actionButton("reset", "Reset filters", icon = icon("rotate-left"))
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f7f5; }
      .main-header .logo { font-weight: 700; }
      .box { border-radius: 8px; }
      .small-box { border-radius: 8px; }
    "))),
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("records_box", width = 3),
          valueBoxOutput("production_box", width = 3),
          valueBoxOutput("states_box", width = 3),
          valueBoxOutput("crops_box", width = 3)
        ),
        fluidRow(
          box(title = "Annual Production Trend", status = "success", solidHeader = TRUE,
              width = 8, plotlyOutput("overview_trend", height = 320)),
          box(title = "Production by Season", status = "success", solidHeader = TRUE,
              width = 4, plotlyOutput("season_bar", height = 320))
        ),
        fluidRow(
          box(title = "Top Crops by Total Production", status = "success", solidHeader = TRUE,
              width = 6, plotlyOutput("top_crops", height = 360)),
          box(title = "Production Distribution", status = "success", solidHeader = TRUE,
              width = 6, plotlyOutput("production_hist", height = 360))
        )
      ),
      tabItem(
        tabName = "geography",
        fluidRow(
          box(title = "State-Level Production Comparison", status = "success", solidHeader = TRUE,
              width = 12, plotlyOutput("state_bar", height = 480))
        ),
        fluidRow(
          box(title = "District Production (where available)", status = "success", solidHeader = TRUE,
              width = 12, plotlyOutput("district_bar", height = 400))
        ),
        helpText("A true choropleth requires a state/district boundary file and matching geographic names. This dashboard includes a state comparison chart; see notes below for adding a map.")
      ),
      tabItem(
        tabName = "time",
        fluidRow(
          box(title = "Crop Production Over Time", status = "success", solidHeader = TRUE,
              width = 8, plotlyOutput("year_line", height = 360)),
          box(title = "Seasonal Production Heatmap", status = "success", solidHeader = TRUE,
              width = 4, plotlyOutput("season_heatmap", height = 360))
        ),
        fluidRow(
          box(title = "Yield by Crop", status = "success", solidHeader = TRUE,
              width = 12, plotlyOutput("yield_box", height = 420))
        )
      ),
      tabItem(
        tabName = "relationships",
        fluidRow(
          box(title = "Production vs Area", status = "success", solidHeader = TRUE,
              width = 6, plotlyOutput("production_area", height = 380)),
          box(title = "Correlation Heatmap", status = "success", solidHeader = TRUE,
              width = 6, plotlyOutput("correlation_plot", height = 380))
        ),
        fluidRow(
          box(title = "Parallel Coordinates: Production, Area and Yield",
              status = "success", solidHeader = TRUE, width = 12,
              plotlyOutput("parallel_plot", height = 430))
        ),
        helpText("Area/yield charts appear only when those fields exist and contain usable values.")
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          box(title = "Filtered Dataset", status = "success", solidHeader = TRUE,
              width = 12, downloadButton("download_csv", "Download filtered CSV"),
              br(), br(), DTOutput("data_table"))
        )
      )
    )
  )
)

# ---- 4. Server ----
server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSelectInput(session, "states", selected = sort(unique(agri$state)))
    updateSelectInput(session, "crops",
                      selected = head(sort(unique(agri$crop)), min(10, length(unique(agri$crop)))))
    updateSelectInput(session, "seasons", selected = sort(unique(agri$season)))
    updateSliderInput(session, "years",
                      value = c(floor(min(agri$year, na.rm = TRUE)),
                                ceiling(max(agri$year, na.rm = TRUE))))
  })
  
  filtered <- reactive({
    d <- agri
    if (!is.null(input$states) && length(input$states) > 0)
      d <- d |> filter(state %in% input$states) else return(d[0, ])
    if (!is.null(input$crops) && length(input$crops) > 0)
      d <- d |> filter(crop %in% input$crops) else return(d[0, ])
    if (!is.null(input$seasons) && length(input$seasons) > 0)
      d <- d |> filter(season %in% input$seasons) else return(d[0, ])
    if (!is.null(input$years))
      d <- d |> filter(year >= input$years[1], year <= input$years[2])
    d
  })
  
  output$records_box <- renderValueBox({
    valueBox(comma(nrow(filtered())), "Filtered records", icon = icon("database"), color = "green")
  })
  output$production_box <- renderValueBox({
    total <- sum(filtered()$production, na.rm = TRUE)
    valueBox(comma(round(total, 2)), "Total production (dataset units)", icon = icon("wheat-awn"), color = "olive")
  })
  output$states_box <- renderValueBox({
    valueBox(n_distinct(filtered()$state), "States / regions", icon = icon("map"), color = "teal")
  })
  output$crops_box <- renderValueBox({
    valueBox(n_distinct(filtered()$crop), "Crop types", icon = icon("seedling"), color = "purple")
  })
  
  output$overview_trend <- renderPlotly({
    d <- filtered() |> filter(!is.na(production)) |>
      group_by(year) |> summarise(production = sum(production, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(d) > 0, "No data for the selected filters."))
    p <- ggplot(d, aes(year, production)) +
      geom_line(color = "#16834A", linewidth = 1) +
      geom_point(color = "#16834A", size = 2) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(x = "Year", y = "Total production", title = NULL) +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$season_bar <- renderPlotly({
    d <- filtered() |>
      filter(!is.na(production), is.finite(production), production >= 0) |>
      group_by(season) |>
      summarise(production = sum(production, na.rm = TRUE), .groups = "drop") |>
      filter(production > 0) |>
      arrange(production)
    
    validate(need(nrow(d) > 0, "No positive production values found for these filters."))
    
    p <- ggplot(d, aes(
      x = production,
      y = reorder(season, production),
      text = paste0("Season: ", season,
                    "<br>Total production: ", scales::comma(production))
    )) +
      geom_col(fill = "#16834A", width = 0.65) +
      scale_x_log10(labels = scales::label_number(
        scale_cut = scales::cut_short_scale()
      )) +
      labs(
        title = "Total Crop Production by Season",
        x = "Total production (log scale)",
        y = "Agricultural season"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$top_crops <- renderPlotly({
    d <- filtered() |>
      filter(!is.na(production), is.finite(production), production > 0) |>
      group_by(crop) |>
      summarise(production = sum(production, na.rm = TRUE), .groups = "drop") |>
      slice_max(production, n = 10, with_ties = FALSE) |>
      arrange(production)
    
    validate(need(nrow(d) > 0, "No positive production values found."))
    
    p <- ggplot(d, aes(
      x = production,
      y = reorder(crop, production),
      text = paste0("Crop: ", crop,
                    "<br>Total production: ", scales::comma(production))
    )) +
      geom_col(fill = "#16834A", width = 0.7) +
      scale_x_log10(labels = scales::label_number(
        scale_cut = scales::cut_short_scale()
      )) +
      labs(
        title = "Top 10 Crops by Total Production",
        x = "Total production (log scale)",
        y = "Crop"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$production_hist <- renderPlotly({
    d <- filtered() |>
      filter(!is.na(production), is.finite(production), production > 0)
    
    validate(need(nrow(d) > 0, "No positive production values available."))
    
    # Log-transform values so a few very large records do not hide the distribution.
    p <- ggplot(d, aes(x = log10(production))) +
      geom_histogram(bins = 35, fill = "#5AAE7A", color = "white") +
      scale_x_continuous(
        breaks = pretty(log10(d$production), n = 6),
        labels = function(x) scales::label_number(
          scale_cut = scales::cut_short_scale()
        )(10^x)
      ) +
      labs(
        title = "Distribution of Positive Production Values",
        x = "Production (logarithmic scale)",
        y = "Number of records"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p)
  })
  
  output$state_bar <- renderPlotly({
    d <- filtered() |> group_by(state) |>
      summarise(production = sum(production, na.rm = TRUE), .groups = "drop") |>
      slice_max(production, n = 30, with_ties = FALSE)
    validate(need(nrow(d) > 0, "No data for the selected filters."))
    p <- ggplot(d, aes(reorder(state, production), production, fill = production)) +
      geom_col() + coord_flip() +
      scale_fill_viridis_c(option = "C") +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(x = NULL, y = "Total production", fill = "Production") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$district_bar <- renderPlotly({
    d <- filtered() |> filter(!is.na(district), district != "") |>
      group_by(district) |>
      summarise(production = sum(production, na.rm = TRUE), .groups = "drop") |>
      slice_max(production, n = 25, with_ties = FALSE)
    validate(need(nrow(d) > 0, "District data is not available in this dataset or filter."))
    p <- ggplot(d, aes(reorder(district, production), production)) +
      geom_col(fill = "#3B8D69") + coord_flip() +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(x = NULL, y = "Total production") + theme_minimal(base_size = 12)
    ggplotly(p)
  })
  
  output$year_line <- renderPlotly({
    d <- filtered() |> group_by(year, crop) |>
      summarise(production = sum(production, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(d) > 0, "No data for the selected filters."))
    p <- ggplot(d, aes(year, production, color = crop, group = crop)) +
      geom_line(linewidth = 0.8) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(x = "Year", y = "Total production", color = "Crop") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })
  
  output$season_heatmap <- renderPlotly({
    d <- filtered() |> group_by(crop, season) |>
      summarise(production = sum(production, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(d) > 0, "No data for the selected filters."))
    # Keep the top 20 crops so labels remain readable.
    top <- d |> group_by(crop) |> summarise(total = sum(production), .groups = "drop") |>
      slice_max(total, n = 20, with_ties = FALSE) |> pull(crop)
    d <- d |> filter(crop %in% top)
    p <- ggplot(d, aes(season, crop, fill = production)) +
      geom_tile(color = "white", linewidth = 0.2) +
      scale_fill_viridis_c(option = "C", labels = label_number(scale_cut = cut_short_scale())) +
      labs(x = "Season", y = "Crop", fill = "Production") +
      theme_minimal(base_size = 10)
    ggplotly(p)
  })
  
  output$yield_box <- renderPlotly({
    d <- filtered() |> filter(!is.na(yield_value), is.finite(yield_value))
    validate(need(nrow(d) > 0, "No usable yield column found. Add a yield column or calculate yield from production / area."))
    top <- d |> group_by(crop) |> summarise(n = n(), .groups = "drop") |>
      slice_max(n, n = 12, with_ties = FALSE) |> pull(crop)
    d <- d |> filter(crop %in% top)
    p <- ggplot(d, aes(crop, yield_value, fill = crop)) +
      geom_boxplot(show.legend = FALSE, outlier.alpha = 0.25) +
      coord_flip() +
      labs(x = NULL, y = "Yield (dataset units)") +
      theme_minimal(base_size = 11)
    ggplotly(p)
  })
  
  output$production_area <- renderPlotly({
    d <- filtered() |> filter(!is.na(area), !is.na(production), area > 0)
    validate(need(nrow(d) > 0, "Area values are not available in this dataset."))
    p <- ggplot(d, aes(area, production, color = season, text = paste(
      "Crop:", crop, "<br>State:", state, "<br>Year:", year,
      "<br>Area:", area, "<br>Production:", production
    ))) +
      geom_point(alpha = 0.6) +
      scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(x = "Area", y = "Production", color = "Season") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })
  
  output$correlation_plot <- renderPlotly({
    d <- filtered() |> select(any_of(c("production", "area", "yield_value", "year"))) |>
      select(where(is.numeric)) |> drop_na()
    validate(need(ncol(d) >= 2 && nrow(d) >= 3, "Need at least two numeric variables with complete values for correlation."))
    # Remove columns with no variation before calculating correlation.
    d <- d |> select(where(~ sd(.x, na.rm = TRUE) > 0))
    validate(need(ncol(d) >= 2, "Not enough varying numeric columns for a correlation heatmap."))
    corr <- cor(d, use = "pairwise.complete.obs")
    corr_df <- as.data.frame(as.table(corr))
    names(corr_df) <- c("Variable1", "Variable2", "Correlation")
    p <- ggplot(corr_df, aes(Variable1, Variable2, fill = Correlation,
                             text = paste("Variables:", Variable1, "and", Variable2,
                                          "<br>Correlation:", round(Correlation, 3)))) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", Correlation)), size = 4) +
      scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                           midpoint = 0, limits = c(-1, 1)) +
      labs(x = NULL, y = NULL, fill = "Correlation") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
    ggplotly(p, tooltip = "text")
  })
  
  output$parallel_plot <- renderPlotly({
    d <- filtered() |> select(any_of(c("production", "area", "yield_value", "year"))) |>
      select(where(is.numeric)) |> drop_na()
    d <- d |> select(where(~ sd(.x, na.rm = TRUE) > 0))
    validate(need(ncol(d) >= 2 && nrow(d) >= 2,
                  "Need at least two numeric variables with complete values for parallel coordinates."))
    # Limit to 500 rows to keep the interactive plot responsive.
    if (nrow(d) > 500) d <- d |> slice_sample(n = 500)
    plot_ly(
      data = d,
      type = "parcoords",
      line = list(color = d[[1]], colorscale = "Viridis", showscale = TRUE),
      dimensions = lapply(names(d), function(nm) {
        list(label = nm, values = d[[nm]])
      })
    )
  })
  
  output$data_table <- renderDT({
    datatable(
      filtered(),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("agrivision_filtered_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(filtered(), file)
  )
}

shinyApp(ui, server)
