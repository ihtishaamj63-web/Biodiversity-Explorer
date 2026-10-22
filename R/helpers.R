IUCN_LEVELS <- c(
  "Least Concern", "Near Threatened", "Vulnerable", "Endangered",
  "Critically Endangered", "Data Deficient", "Not Evaluated"
)

IUCN_COLOURS <- c(
  "Least Concern" = "#27ae60",
  "Near Threatened" = "#f1c40f",
  "Vulnerable" = "#f39c12",
  "Endangered" = "#e74c3c",
  "Critically Endangered" = "#8e44ad",
  "Data Deficient" = "#7f8c8d",
  "Not Evaluated" = "#34495e"
)

CAPE_REGIONS <- tibble::tribble(
  ~region, ~lat, ~lon,
  "Cape Peninsula", -34.1, 18.4,
  "Boland", -33.8, 19.2,
  "Overberg", -34.3, 20.0,
  "Cederberg", -32.5, 19.2,
  "Little Karoo", -33.5, 21.5,
  "Garden Route", -33.9, 22.8
)

assign_region <- function(latitude, longitude) {
  if (length(latitude) != length(longitude)) {
    stop("latitude and longitude must have equal lengths.", call. = FALSE)
  }

  vapply(seq_along(latitude), function(i) {
    if (is.na(latitude[i]) || is.na(longitude[i])) return("Other")
    distances <- (CAPE_REGIONS$lat - latitude[i])^2 +
      (CAPE_REGIONS$lon - longitude[i])^2
    closest <- which.min(distances)
    if (sqrt(distances[closest]) <= 1.25) CAPE_REGIONS$region[closest] else "Other"
  }, character(1), USE.NAMES = FALSE)
}

clean_occurrences <- function(raw) {
  required <- c("species", "decimalLatitude", "decimalLongitude", "year")
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop("Missing required columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  data <- raw |>
    dplyr::mutate(
      species = stringr::str_to_sentence(
        stringr::str_squish(as.character(species))
      ),
      decimalLatitude = suppressWarnings(as.numeric(decimalLatitude)),
      decimalLongitude = suppressWarnings(as.numeric(decimalLongitude)),
      year = suppressWarnings(as.integer(year))
    )
  if (!"coordinateUncertaintyInMeters" %in% names(data)) {
    data$coordinateUncertaintyInMeters <- NA_real_
  } else {
    data$coordinateUncertaintyInMeters <-
      suppressWarnings(as.numeric(data$coordinateUncertaintyInMeters))
  }
  if (!"data_source" %in% names(data)) data$data_source <- "GBIF"

  log <- tibble::tibble(
    step = "Input",
    removed = 0L,
    remaining = nrow(data)
  )
  record_step <- function(label, keep) {
    before <- nrow(data)
    data <<- data[keep %in% TRUE, , drop = FALSE]
    log <<- dplyr::add_row(
      log, step = label, removed = before - nrow(data), remaining = nrow(data)
    )
  }

  record_step(
    "Genus-only names",
    !is.na(data$species) &
      grepl("^[A-Z][a-z-]+ [a-z-]+( [a-z-]+)*$", data$species)
  )
  record_step("Missing coordinates",
              !is.na(data$decimalLatitude) & !is.na(data$decimalLongitude))
  record_step("Zero coordinates",
              !(data$decimalLatitude == 0 & data$decimalLongitude == 0))
  record_step("Outside South Africa",
              data$decimalLatitude >= -35 & data$decimalLatitude <= -22 &
                data$decimalLongitude >= 16 & data$decimalLongitude <= 33.5)
  record_step("Imprecise coordinates",
              is.na(data$coordinateUncertaintyInMeters) |
                data$coordinateUncertaintyInMeters <= 10000)
  record_step("Missing years", !is.na(data$year))
  record_step("Invalid years", data$year >= 1900 & data$year <= as.integer(format(Sys.Date(), "%Y")))

  before <- nrow(data)
  data <- dplyr::distinct(
    data, species, decimalLatitude, decimalLongitude, year, .keep_all = TRUE
  )
  log <- dplyr::add_row(
    log, step = "Duplicate records", removed = before - nrow(data),
    remaining = nrow(data)
  )

  list(data = data, log = log)
}

diversity_summary <- function(data) {
  counts <- data |>
    dplyr::count(species, name = "records") |>
    dplyr::pull(records)
  richness <- length(counts)
  if (!richness) {
    return(list(richness = 0L, shannon = NA_real_, simpson = NA_real_,
                evenness = NA_real_))
  }

  shannon <- vegan::diversity(counts, index = "shannon")
  list(
    richness = richness,
    shannon = unname(shannon),
    simpson = unname(vegan::diversity(counts, index = "simpson")),
    evenness = if (richness > 1) unname(shannon / log(richness)) else NA_real_
  )
}

richness_by_cell <- function(data, cell_size) {
  if (!is.numeric(cell_size) || length(cell_size) != 1 ||
      is.na(cell_size) || cell_size <= 0) {
    stop("cell_size must be a positive number.", call. = FALSE)
  }
  data |>
    dplyr::mutate(
      lat0 = floor(decimalLatitude / cell_size) * cell_size,
      lon0 = floor(decimalLongitude / cell_size) * cell_size
    ) |>
    dplyr::group_by(lat0, lon0) |>
    dplyr::summarise(
      lat1 = lat0[1] + cell_size,
      lon1 = lon0[1] + cell_size,
      richness = dplyr::n_distinct(species),
      records = dplyr::n(),
      .groups = "drop"
    )
}

species_accumulation <- function(data, cell_size) {
  if (!is.numeric(cell_size) || length(cell_size) != 1 ||
      is.na(cell_size) || cell_size <= 0) {
    stop("cell_size must be a positive number.", call. = FALSE)
  }
  cells <- data |>
    dplyr::mutate(site = paste(
      floor(decimalLatitude / cell_size),
      floor(decimalLongitude / cell_size), sep = ":"
    )) |>
    dplyr::distinct(site, species) |>
    dplyr::mutate(present = 1L) |>
    tidyr::pivot_wider(names_from = species, values_from = present,
                       values_fill = 0)

  if (nrow(cells) < 2) return(NULL)
  community <- as.matrix(dplyr::select(cells, -site))
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(42)
  curve <- vegan::specaccum(community, method = "random")
  tibble::tibble(sites = curve$sites, richness = curve$richness, sd = curve$sd)
}

compare_areas <- function(data, area_a, area_b) {
  areas <- c(area_a, area_b)
  subsets <- lapply(areas, function(area) data[data$region == area, , drop = FALSE])
  area_records <- vapply(subsets, nrow, integer(1))
  sample_size <- if (all(area_records > 0)) min(area_records) else 0L

  dplyr::bind_rows(lapply(seq_along(areas), function(i) {
    subset <- subsets[[i]]
    stats <- diversity_summary(subset)
    counts <- subset |>
      dplyr::count(species, name = "records") |>
      dplyr::pull(records)
    rarefied <- if (sample_size >= 1) {
      unname(vegan::rarefy(matrix(counts, nrow = 1), sample_size)[1])
    } else {
      NA_real_
    }
    tibble::tibble(
      area = areas[i],
      records = nrow(subset),
      richness = stats$richness,
      shannon = stats$shannon,
      simpson = stats$simpson,
      evenness = stats$evenness,
      rarefied_richness = rarefied,
      rarefied_n = sample_size
    )
  }))
}
