# app.R -----------------------------------------------------------------
# Conservation & Biodiversity Explorer: Cape Fynbos
# Run from the project root with:  shiny::runApp()

library(shiny)
library(tidyverse)
library(leaflet)
library(vegan)
library(DT)

source("R/helpers.R")

# ---- Load data --------------------------------------------------------------

data_file <- "data/clean_occurrences.csv"
if (!file.exists(data_file)) {
  stop("data/clean_occurrences.csv not found. ",
       "Run source('data-raw/build_data.R') from the project root first.",
       call. = FALSE)
}

occurrences <- read_csv(data_file, show_col_types = FALSE)
if (!"region" %in% names(occurrences)) {
  occurrences$region <- assign_region(
    occurrences$decimalLatitude, occurrences$decimalLongitude
  )
}
if (!"genus" %in% names(occurrences)) {
  occurrences$genus <- stringr::word(occurrences$species, 1)
}
if (!"data_source" %in% names(occurrences)) occurrences$data_source <- "mock"
occurrences <- occurrences %>%
  mutate(iucn_status = factor(iucn_status, levels = IUCN_LEVELS))

cleaning_log <- if (file.exists("data/cleaning_log.csv")) {
  read_csv("data/cleaning_log.csv", show_col_types = FALSE)
} else {
  NULL
}

present_status <- IUCN_LEVELS[IUCN_LEVELS %in% occurrences$iucn_status]
all_species    <- sort(unique(occurrences$species))
all_genera     <- sort(unique(occurrences$genus))
all_regions    <- sort(unique(occurrences$region))
year_min       <- min(occurrences$year, na.rm = TRUE)
year_max       <- max(occurrences$year, na.rm = TRUE)
is_mock        <- "data_source" %in% names(occurrences) &&
  any(occurrences$data_source == "mock")

fmt <- function(x) if (is.na(x)) "–" else formatC(x, digits = 2, format = "f")

plot_theme <- theme_minimal(base_size = 13) +
  theme(plot.subtitle = element_text(colour = "#7f8c8d"))

hint <- function(text) {
  tags$span(
    "?", class = "hint", title = text, tabindex = "0", role = "img",
    `aria-label` = paste("Help:", text)
  )
}

# ---- UI ---------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; background-color: #f4f7f6; }
    .well { background-color: #ffffff; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    h2 { color: #2c3e50; font-weight: bold; }
    .stat-number { font-size: 28px; font-weight: bold; color: #2c3e50; margin: 0; }
    .stat-label { color: #7f8c8d; margin-bottom: 4px; }
    .note { color: #7f8c8d; font-size: 13px; }
    .hint { display: inline-block; width: 16px; height: 16px; margin-left: 3px;
      border: 1px solid #7f8c8d; border-radius: 50%; color: #52616b;
      font-size: 11px; font-weight: bold; line-height: 14px; text-align: center;
      cursor: help; vertical-align: text-top; }
    .hint:focus { outline: 2px solid #3498db; outline-offset: 2px; }
  "))),

  titlePanel(
    div(h2("Conservation & Biodiversity Explorer: Cape Fynbos"),
        h5("Explore Proteaceae and Ericaceae occurrence records, threat status and diversity across the Cape",
           style = "color: #7f8c8d;"))
  ),

  uiOutput("data_banner"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters", style = "color: #3498db; font-weight: bold;"),

      sliderInput("year_range", tagList(
                    "Year range",
                    hint("Keep records whose observation year falls in this inclusive range.")
                  ),
                  min = year_min, max = year_max,
                  value = c(year_min, year_max), sep = "", step = 1),

      checkboxGroupInput("iucn_filter", tagList(
                           "IUCN status",
                           hint("Keep records in the selected status categories. Demo-data statuses are illustrative placeholders, not real assessments.")
                         ),
                         choices = present_status, selected = present_status),

      checkboxGroupInput("genus_filter", tagList(
                           "Genus",
                           hint("Keep records belonging to the selected plant genera.")
                         ),
                         choices = all_genera, selected = all_genera),

      checkboxGroupInput("region_filter", tagList(
                           "Area",
                           hint("Filters the map and diversity tab by the nearest Cape reference region; comparison areas are chosen separately.")
                         ),
                         choices = all_regions, selected = all_regions),

      selectizeInput("species_filter", tagList(
                       "Species (leave empty for all)",
                       hint("Optionally keep only selected species. This filter works together with genus and the other sidebar filters.")
                     ),
                     choices = all_species, multiple = TRUE,
                     options = list(placeholder = "All species")),

      hr(),
      h4("Analysis", style = "color: #3498db; font-weight: bold;"),
      selectInput("cell_size", tagList(
                    "Grid cell size",
                    hint("Sets the latitude/longitude grid used to count distinct species and define survey sites for the accumulation curve.")
                  ),
                  choices = c("0.1° (~10 km)" = 0.1,
                              "0.25° (~25 km)" = 0.25,
                              "0.5° (~50 km)" = 0.5),
                  selected = 0.25),
      helpText("Smaller cells show finer spatial detail; larger cells combine more records."),

      hr(),
      downloadButton("download_data", "Download filtered data"),
      helpText("Downloads the filtered occurrence rows currently shown in the data table.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",

        # -- Map
        tabPanel("Map",
          br(),
          radioButtons("map_mode", tagList(
                         "Map display",
                         hint("Choose individual occurrence records or a grid summary of distinct species.")
                       ), inline = TRUE,
                       choices = c("Records by IUCN status" = "records",
                                   "Species richness per grid cell" = "richness")),
          p(class = "note",
            "Each dot is an occurrence record, not an individual plant. In richness mode, each square is shaded by the number of distinct species recorded in that grid cell. The map uses OpenStreetMap tiles and requires an internet connection."),
          leafletOutput("map", height = "600px")
        ),

        # -- Diversity
        tabPanel("Diversity",
          br(),
          fluidRow(
            column(3, div(class = "well", div(class = "stat-label", "Records",
                                              hint("Number of occurrence records matching the active filters; this is not a count of individual plants.")),
                          p(class = "stat-number", textOutput("total_records", inline = TRUE)))),
            column(3, div(class = "well", div(class = "stat-label", "Species",
                                              hint("Number of distinct species represented in the filtered occurrence records.")),
                          p(class = "stat-number", textOutput("unique_species", inline = TRUE)))),
            column(3, div(class = "well", div(class = "stat-label", "Shannon H'",
                                              hint("Shannon diversity summarizes both the number of species and how evenly records are distributed among them. Higher values mean greater diversity in this sample.")),
                          p(class = "stat-number", textOutput("shannon_index", inline = TRUE)))),
            column(3, div(class = "well", div(class = "stat-label", "Simpson 1-D",
                                              hint("The probability that two randomly selected records represent different species. Higher values mean greater diversity in this sample.")),
                          p(class = "stat-number", textOutput("simpson_index", inline = TRUE))))
          ),
          fluidRow(
            column(6, div(class = "well",
                          h4("Survey effort",
                             hint("Counts occurrence records by year. Peaks can reflect more observation activity, rather than a true increase in plant abundance.")),
                          plotOutput("year_chart", height = "300px"),
                          p(class = "note", "Records per year. Spikes usually reflect recording effort, not more plants."))),
            column(6, div(class = "well",
                          h4("Species accumulation",
                             hint("Randomized accumulation of distinct species across occupied grid cells. A curve that continues rising suggests more sampling may find additional species.")),
                          plotOutput("accumulation_curve", height = "300px"),
                          p(class = "note", "If the curve is still climbing, the area is under-surveyed.")))
          ),
          fluidRow(
            column(12, div(class = "well",
                           h4("Records per species",
                              hint("The 15 most-recorded species under the current filters. Record counts reflect sampling activity and are not population sizes.")),
                           uiOutput("species_plot_ui")))
          )
        ),

        # -- Compare areas
        tabPanel("Compare areas",
          br(),
          fluidRow(
            column(3, selectInput("area_a", tagList(
                                    "Area A",
                                    hint("First Cape reference region to compare. Comparison ignores the sidebar Area filter.")
                                  ), choices = all_regions,
                                  selected = all_regions[1])),
            column(3, selectInput("area_b", tagList(
                                    "Area B",
                                    hint("Second Cape reference region to compare. Comparison ignores the sidebar Area filter.")
                                  ), choices = all_regions,
                                  selected = all_regions[min(2, length(all_regions))]))
          ),
          p(class = "note",
            "The table and chart compare records, observed species and diversity. Rarefied richness estimates expected species at equal record counts; it helps account for unequal sampling effort, but does not correct every survey bias."),
          div(class = "well", tableOutput("compare_table"),
              uiOutput("compare_note")),
          div(class = "well", plotOutput("compare_chart", height = "320px")),
          p(class = "note",
            "Uses year, IUCN status, genus and species filters from the sidebar. The Area filter is ignored here.")
        ),

        # -- Data
        tabPanel("Data table", br(),
                 p(class = "note", "One row per cleaned occurrence record, limited to the active filters."),
                 DTOutput("data_table")),

        # -- Data quality
        tabPanel("Data quality",
          br(),
          p("Records removed at each sequential cleaning step before the data reached this app. A record rejected at an earlier step is not counted again later."),
          DTOutput("cleaning_table")
        )
      )
    )
  )
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  # Year, IUCN and species filters (used by the compare tab)
  base_filtered <- reactive({
    d <- occurrences %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2],
        iucn_status %in% input$iucn_filter
      )
    if (length(input$species_filter) > 0) {
      d <- d %>% filter(species %in% input$species_filter)
    }
    d <- d %>% filter(genus %in% input$genus_filter)
    d
  })

  # ...plus the Area filter (used everywhere else)
  filtered_data <- reactive({
    base_filtered() %>% filter(region %in% input$region_filter)
  })

  cell_size     <- reactive(as.numeric(input$cell_size))
  summary_stats <- reactive(diversity_summary(filtered_data()))

  no_data_msg <- "No records match the current filters."

  # ---- Banner
  output$data_banner <- renderUI({
    if (is_mock) {
      div(class = "alert alert-warning",
          strong("Demo data. "),
          "These records are simulated and the IUCN statuses are placeholders, ",
          "not real assessments.")
    }
  })

  # ---- Summary cards
  output$total_records  <- renderText(format(nrow(filtered_data()), big.mark = ","))
  output$unique_species <- renderText(summary_stats()$richness)
  output$shannon_index  <- renderText(fmt(summary_stats()$shannon))
  output$simpson_index  <- renderText(fmt(summary_stats()$simpson))

  # ---- Map (base map drawn once; layers updated through a proxy)
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(min(occurrences$decimalLongitude), min(occurrences$decimalLatitude),
                max(occurrences$decimalLongitude), max(occurrences$decimalLatitude))
  })
  outputOptions(output, "map", suspendWhenHidden = FALSE)

  observe({
    d <- filtered_data()
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes() %>% clearControls()
    if (nrow(d) == 0) return()

    if (input$map_mode == "records") {
      shown <- present_status[present_status %in% d$iucn_status]
      pal <- colorFactor(unname(IUCN_COLOURS[shown]), levels = shown)
      legend_title <- if (any(d$data_source == "mock", na.rm = TRUE)) {
        "IUCN status (illustrative)"
      } else {
        "IUCN status"
      }
      d <- d %>% arrange(iucn_status)

      proxy %>%
        addCircleMarkers(
          data = d,
          lng = ~decimalLongitude, lat = ~decimalLatitude,
          fillColor = ~pal(as.character(iucn_status)),
          radius = 7, stroke = TRUE, color = "#ffffff", weight = 1.5,
          fillOpacity = 0.9,
          popup = ~paste0("<b>", species, "</b><br>Genus: ", genus,
                          "<br>Year: ", year,
                          "<br>Status: ", iucn_status, "<br>Area: ", region)
        ) %>%
        addLegend("bottomright", pal = pal, values = shown, title = legend_title)
    } else {
      cells <- richness_by_cell(d, cell_size())
      pal <- colorNumeric("YlGnBu", domain = c(0, max(cells$richness, 1)))

      proxy %>%
        addRectangles(
          data = cells,
          lng1 = ~lon0, lat1 = ~lat0, lng2 = ~lon1, lat2 = ~lat1,
          fillColor = ~pal(richness), fillOpacity = 0.7,
          color = "#555555", weight = 1,
          popup = ~paste0("Species: ", richness, "<br>Records: ", records)
        ) %>%
        addLegend("bottomright", pal = pal, values = cells$richness,
                  title = "Species richness")
    }
  })

  # ---- Charts
  output$year_chart <- renderPlot({
    d <- filtered_data()
    validate(need(nrow(d) > 0, no_data_msg))

    per_year <- d %>%
      count(year) %>%
      complete(year = full_seq(year, 1), fill = list(n = 0L))

    ggplot(per_year, aes(year, n)) +
      geom_col(fill = "#3498db") +
      labs(x = "Year", y = "Records") +
      plot_theme
  })

  output$accumulation_curve <- renderPlot({
    d <- filtered_data()
    validate(need(nrow(d) > 0, no_data_msg))

    acc <- species_accumulation(d, cell_size())
    validate(need(!is.null(acc),
                  "Need records from at least 2 grid cells. Try a smaller cell size or wider filters."))

    ggplot(acc, aes(sites, richness)) +
      geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd),
                  fill = "#3498db", alpha = 0.2) +
      geom_line(colour = "#2c3e50", linewidth = 1) +
      labs(x = "Grid cells sampled", y = "Cumulative species") +
      plot_theme
  })

  output$species_chart <- renderPlot({
    d <- filtered_data()
    validate(need(nrow(d) > 0, no_data_msg))

    top_species <- d %>%
      count(species, sort = TRUE) %>%
      slice_head(n = 15) %>%
      pull(species)

    d %>%
      filter(species %in% top_species) %>%
      count(species, iucn_status) %>%
      ggplot(aes(x = reorder(species, n), y = n, fill = iucn_status)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = IUCN_COLOURS, name = "IUCN status", drop = TRUE) +
      labs(x = NULL, y = "Records") +
      plot_theme
  })

  output$species_plot_ui <- renderUI({
    n_species <- min(15, dplyr::n_distinct(filtered_data()$species))
    plotOutput("species_chart", height = paste0(max(240, n_species * 28), "px"))
  })

  # ---- Compare areas
  comparison <- reactive({
    req(input$area_a, input$area_b)
    compare_areas(base_filtered(), input$area_a, input$area_b)
  })

  output$compare_table <- renderTable({
    comparison() %>%
      transmute(
        Area = area,
        Records = records,
        Species = richness,
        `Shannon H'` = shannon,
        `Simpson 1-D` = simpson,
        Evenness = evenness,
        `Rarefied richness` = rarefied_richness
      )
  }, digits = 2, na = "–")

  output$compare_note <- renderUI({
    n <- comparison()$rarefied_n[1]
    if (is.na(n) || n < 2) {
      p(class = "note", "Rarefied richness needs at least 2 records in each area.")
    } else {
      p(class = "note",
        paste0("Rarefied richness = expected species if both areas were sampled at the same effort (",
               n, " records each), so the better-surveyed area doesn't look richer just because of effort."))
    }
  })

  output$compare_chart <- renderPlot({
    cmp <- comparison()
    validate(need(all(cmp$records > 0), "One of the selected areas has no records under the current filters."))

    cmp %>%
      select(area, `Shannon H'` = shannon, `Simpson 1-D` = simpson,
             `Rarefied richness` = rarefied_richness) %>%
      pivot_longer(-area, names_to = "metric", values_to = "value") %>%
      ggplot(aes(x = area, y = value, fill = area)) +
      geom_col(width = 0.6) +
      facet_wrap(~metric, scales = "free_y") +
      scale_fill_manual(values = c("#3498db", "#e67e22")) +
      labs(x = NULL, y = NULL) +
      plot_theme +
      theme(legend.position = "none")
  })

  # ---- Data table + download
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("cape_fynbos_occurrences_filtered_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(filtered_data(), file)
  )

  # ---- Data quality
  output$cleaning_table <- renderDT({
    validate(need(!is.null(cleaning_log), "No cleaning log found (data/cleaning_log.csv)."))
    datatable(cleaning_log, options = list(dom = "t", pageLength = 20), rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
