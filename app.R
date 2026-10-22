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

threatened_statuses <- c("Vulnerable", "Endangered", "Critically Endangered")

status_descriptions <- c(
  "Least Concern" = "Not currently considered at risk of extinction.",
  "Near Threatened" = "Close to qualifying for a threatened category.",
  "Vulnerable" = "At high risk of extinction in the wild.",
  "Endangered" = "At very high risk of extinction in the wild.",
  "Critically Endangered" = "At extremely high risk of extinction in the wild.",
  "Data Deficient" = "There is not enough information to assess extinction risk.",
  "Not Evaluated" = "No extinction-risk assessment is available."
)

# ---- UI ---------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; background-color: #f4f7f6; }
    .well { background-color: #ffffff; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    h2 { color: #2c3e50; font-weight: bold; }
    .stat-number { font-size: 28px; font-weight: bold; color: #2c3e50; margin: 0; }
    .stat-label { color: #7f8c8d; margin-bottom: 4px; }
    .note { color: #7f8c8d; font-size: 13px; }
    .status-key { margin: 0; padding: 0; list-style: none; }
    .status-key li { padding: 6px 0; border-bottom: 1px solid #ecf0f1; }
    .status-swatch { display: inline-block; width: 12px; height: 12px;
      margin-right: 8px; border-radius: 50%; }
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

      sliderInput("year_range", "Year range",
                  min = year_min, max = year_max,
                  value = c(year_min, year_max), sep = "", step = 1),

      checkboxGroupInput("iucn_filter", "IUCN status",
                         choices = present_status, selected = present_status),

      actionButton("threatened_only", "Threatened only",
                   class = "btn-warning btn-sm"),
      actionButton("all_statuses", "All statuses",
                   class = "btn-default btn-sm"),
      helpText("Threatened only selects Vulnerable, Endangered and Critically Endangered."),

      checkboxGroupInput("genus_filter", "Genus",
                         choices = all_genera, selected = all_genera),

      checkboxGroupInput("region_filter", "Area",
                         choices = all_regions, selected = all_regions),

      selectizeInput("species_filter", "Species (leave empty for all)",
                     choices = all_species, multiple = TRUE,
                     options = list(placeholder = "All species")),

      hr(),
      h4("Analysis", style = "color: #3498db; font-weight: bold;"),
      selectInput("cell_size", "Grid cell size",
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

        tabPanel("Start here",
          br(),
          h3("Explore Cape Fynbos occurrence records"),
          p("This dashboard maps recorded sightings of Proteaceae and Ericaceae and summarizes which species appear in the sample. Use the filters to explore different years, plant groups and areas."),
          p("An occurrence record documents an observation at a place and time. It is not a count of individual plants, and more records can mean more people were recording rather than more plants growing there."),
          p("Use these patterns to explore where observations have been made and what questions might be worth investigating; this sample is not a census or a conservation-priority map."),
          if (is_mock) {
            div(class = "alert alert-warning",
                strong("About this demonstration: "),
                "locations, species ranges and extinction-risk categories are simulated or illustrative. Use the dashboard to learn how the tools work, not to draw conclusions about real populations.")
          },
          h4("What the current filters show"),
          fluidRow(
            column(3, div(class = "well", div(class = "stat-label", "Sightings"),
                          p(class = "stat-number", textOutput("start_records", inline = TRUE)))),
            column(3, div(class = "well", div(class = "stat-label", "Species recorded"),
                          p(class = "stat-number", textOutput("start_species", inline = TRUE)))),
            column(3, div(class = "well", div(class = "stat-label", "Records in threatened categories"),
                          p(class = "stat-number", textOutput("threatened_share", inline = TRUE)))),
            column(3, div(class = "well", div(class = "stat-label", "Most species recorded"),
                          p(class = "stat-number", textOutput("richest_region", inline = TRUE))))
          ),
          p(class = "note",
            "The threatened share is the percentage of records labelled Vulnerable, Endangered or Critically Endangered. In demo data, these labels are placeholders."),
          h4("Extinction-risk categories"),
          p("These categories describe assessed extinction risk, not how many plants were observed. Demo-data assignments are illustrative placeholders."),
          uiOutput("status_guide"),
          h4("Where to go next"),
          tags$ul(
            tags$li("Open Map to see recorded sightings or the number of distinct species in each grid cell."),
            tags$li("Open Diversity to explore the sampled mix of species and how records are distributed over time."),
            tags$li("Open Compare areas to compare observed species counts at equal record effort.")
          )
        ),

        # -- Map
        tabPanel("Map",
          br(),
          radioButtons("map_mode", "Map display", inline = TRUE,
                       choices = c("Species richness per grid cell" = "richness",
                                   "Records by IUCN status" = "records"),
                       selected = "richness"),
          p(class = "note",
            "Each dot is an occurrence record, not an individual plant. In richness mode, each square is shaded by the number of distinct species recorded in that grid cell. The map uses OpenStreetMap tiles and requires an internet connection."),
          leafletOutput("map", height = "600px")
        ),

        # -- Diversity
        tabPanel("Diversity",
          br(),
          fluidRow(
            column(3, div(class = "well", div(class = "stat-label", "Sightings",
                                              tags$small("occurrence records")),
                          p(class = "stat-number", textOutput("total_records", inline = TRUE)))),
            column(3, div(class = "well", div(class = "stat-label", "Species recorded"),
                          p(class = "stat-number", textOutput("unique_species", inline = TRUE)))),
            column(3, div(class = "well", div(class = "stat-label", "Variety score"),
                          p(class = "stat-number", textOutput("effective_species", inline = TRUE)),
                          tags$small("equally common species (exp Shannon H')"))),
            column(3, div(class = "well", div(class = "stat-label", "Different-species chance"),
                          p(class = "stat-number", textOutput("simpson_index", inline = TRUE)),
                          tags$small("Simpson 1-D")))
          ),
          fluidRow(
            column(6, div(class = "well",
                          h4("Recording through time",
                             tags$small("(sightings per year)")),
                          plotOutput("year_chart", height = "300px"),
                          uiOutput("year_caption"))),
            column(6, div(class = "well",
                          h4("Have we found them all?",
                             tags$small("(species accumulation)")),
                          plotOutput("accumulation_curve", height = "300px"),
                          uiOutput("accumulation_caption")))
          ),
          fluidRow(
            column(12, div(class = "well",
                           h4("Most-recorded species",
                              tags$small("(top 15 by sightings)")),
                           uiOutput("species_plot_ui"),
                           p(class = "note",
                             "Record counts describe observation activity, not how many plants occur in nature.")))
          )
        ),

        # -- Compare areas
        tabPanel("Compare areas",
          br(),
          fluidRow(
            column(3, selectInput("area_a", "Area A", choices = all_regions,
                                  selected = all_regions[1])),
            column(3, selectInput("area_b", "Area B", choices = all_regions,
                                  selected = all_regions[min(2, length(all_regions))]))
          ),
          p(class = "note",
            "Rarefied richness means expected species at equal effort: both areas are compared using the same number of occurrence records. This helps with unequal sample sizes, but does not correct every survey bias."),
          div(class = "well", tableOutput("compare_table"),
              uiOutput("compare_note")),
          div(class = "well", plotOutput("compare_chart", height = "320px")),
          p(class = "note",
            "Uses year, status, genus and species filters from the sidebar. The Area filter is ignored here.")
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

  observeEvent(input$threatened_only, {
    updateCheckboxGroupInput(
      session, "iucn_filter",
      selected = intersect(threatened_statuses, present_status)
    )
  })

  observeEvent(input$all_statuses, {
    updateCheckboxGroupInput(session, "iucn_filter", selected = present_status)
  })

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
  output$effective_species <- renderText(
    paste0("≈ ", fmt(exp(summary_stats()$shannon)))
  )
  output$simpson_index <- renderText({
    value <- summary_stats()$simpson
    if (is.na(value)) "–" else sprintf("%.0f%%", value * 100)
  })

  output$start_records <- renderText(format(nrow(filtered_data()), big.mark = ","))
  output$start_species <- renderText(summary_stats()$richness)
  output$threatened_share <- renderText({
    d <- filtered_data()
    if (!nrow(d)) return("–")
    sprintf("%.1f%%", 100 * mean(d$iucn_status %in% threatened_statuses))
  })
  output$richest_region <- renderText({
    d <- filtered_data()
    if (!nrow(d)) return("–")
    ranked <- d %>%
      group_by(region) %>%
      summarise(species = n_distinct(species), .groups = "drop")
    leaders <- ranked %>% filter(species == max(species)) %>% pull(region)
    paste(leaders, collapse = " / ")
  })

  output$status_guide <- renderUI({
    tags$ul(
      class = "status-key",
      lapply(present_status, function(status) {
        tags$li(
          tags$span(
            class = "status-swatch",
            style = paste0("background-color: ", IUCN_COLOURS[[status]], ";")
          ),
          tags$strong(status), " — ", status_descriptions[[status]]
        )
      })
    )
  })

  # ---- Map (base map drawn once; layers updated through a proxy)
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
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
          clusterOptions = markerClusterOptions(),
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
                  title = "Species per grid cell")
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
      labs(x = "Year", y = "Sightings") +
      plot_theme
  })

  output$year_caption <- renderUI({
    d <- filtered_data()
    if (!nrow(d)) return(p(class = "note", no_data_msg))

    year_limits <- input$year_range
    before_years <- if (year_limits[1] < 2015) {
      seq.int(year_limits[1], min(year_limits[2], 2014))
    } else {
      integer()
    }
    after_years <- if (year_limits[2] >= 2015) {
      seq.int(max(year_limits[1], 2015), year_limits[2])
    } else {
      integer()
    }

    per_year <- d %>% count(year)
    before_avg <- if (length(before_years)) {
      sum(per_year$n[per_year$year %in% before_years]) / length(before_years)
    } else {
      NA_real_
    }
    after_avg <- if (length(after_years)) {
      sum(per_year$n[per_year$year %in% after_years]) / length(after_years)
    } else {
      NA_real_
    }

    caption <- if (!is.na(before_avg) && !is.na(after_avg) && before_avg > 0) {
      paste0(
        "Since 2015, the filtered data averages ",
        format(round(after_avg / before_avg, 1), nsmall = 1),
        " times as many records per year as before 2015. This reflects recording activity, not plant abundance."
      )
    } else {
      "Yearly record counts describe observation activity, not plant abundance. Select years on both sides of 2015 to compare the averages."
    }
    p(class = "note", caption)
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

  output$accumulation_caption <- renderUI({
    d <- filtered_data()
    if (!nrow(d)) return(p(class = "note", no_data_msg))
    acc <- species_accumulation(d, cell_size())
    if (is.null(acc)) {
      return(p(class = "note", "At least two occupied grid cells are needed to estimate this curve."))
    }

    final_richness <- tail(acc$richness, 1)
    comparison_index <- max(1, floor(length(acc$richness) * 0.75))
    still_rising <- final_richness > acc$richness[comparison_index]
    trend <- if (still_rising) {
      "The line is still rising near the end, so additional sampled cells are still adding species."
    } else {
      "The line is flattening near the end, so additional sampled cells are adding fewer new species."
    }
    p(class = "note", paste0(
      "The estimate reaches about ", round(final_richness), " species across ",
      max(acc$sites), " occupied grid cells. ", trend,
      " This is not proof that every species has been found."
    ))
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
      labs(x = NULL, y = "Sightings") +
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
        Sightings = records,
        `Species recorded` = richness,
        `Effective species` = exp(shannon),
        `Chance two sightings differ (%)` = 100 * simpson,
        `Species at equal effort` = rarefied_richness
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
      transmute(
        area,
        `Effective species` = exp(shannon),
        `Chance two sightings differ (%)` = 100 * simpson,
        `Species at equal effort` = rarefied_richness
      ) %>%
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
