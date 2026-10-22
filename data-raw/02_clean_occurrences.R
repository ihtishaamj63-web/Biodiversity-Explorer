# data-raw/02_clean_occurrences.R -------------------------------------------
# Cleans data-raw/raw_occurrences.csv and writes the files the app reads:
#   data/clean_occurrences.csv   cleaned records + IUCN status + region
#   data/cleaning_log.csv        how many records each step removed
#
# Works for the mock data AND for a real GBIF download: save the real file as
# data-raw/raw_occurrences.csv (comma or tab separated) and run this script.
#
# Run from the project root:  source("data-raw/02_clean_occurrences.R")

suppressPackageStartupMessages(library(tidyverse))
source("R/helpers.R")

raw_path  <- "data-raw/raw_occurrences.csv"
iucn_path <- "data-raw/iucn_status.csv"

if (!file.exists(raw_path)) {
  stop("Missing ", raw_path, ". Run data-raw/01_generate_mock_data.R first, ",
       "or save your GBIF download there.", call. = FALSE)
}

# GBIF "simple" downloads are tab separated; the mock file is comma separated.
first_line <- readLines(raw_path, n = 1, warn = FALSE)
delim <- if (grepl("\t", first_line)) "\t" else ","

raw <- read_delim(
  raw_path,
  delim = delim,
  quote = if (delim == "\t") "" else "\"",
  col_select = any_of(c("species", "decimalLatitude", "decimalLongitude", "year",
                        "coordinateUncertaintyInMeters", "data_source")),
  show_col_types = FALSE,
  guess_max = 100000
)

if (!"data_source" %in% names(raw)) raw$data_source <- "GBIF"

# IUCN lookup: one row per species. Anything not in the file becomes "Not Evaluated".
if (file.exists(iucn_path)) {
  iucn <- read_csv(iucn_path, show_col_types = FALSE) %>% distinct(species, .keep_all = TRUE)
  bad <- setdiff(unique(iucn$iucn_status), IUCN_LEVELS)
  if (length(bad) > 0) {
    stop("Unknown IUCN categories in ", iucn_path, ": ", paste(bad, collapse = ", "),
         "\nAllowed: ", paste(IUCN_LEVELS, collapse = ", "), call. = FALSE)
  }
} else {
  message("No ", iucn_path, " found: all species will be 'Not Evaluated'.")
  iucn <- tibble(species = character(), iucn_status = character())
}

res <- clean_occurrences(raw)

clean <- res$data %>%
  left_join(iucn, by = "species")

missing_status_species <- clean %>%
  filter(is.na(iucn_status)) %>%
  distinct(species) %>%
  pull(species)
if (length(missing_status_species) == 0) {
  message("IUCN status missing for 0 species.")
} else {
  message(
    "IUCN status missing for ", length(missing_status_species), " species: ",
    paste(missing_status_species, collapse = ", ")
  )
}

clean <- clean %>%
  mutate(
    iucn_status = coalesce(iucn_status, "Not Evaluated"),
    genus = stringr::word(species, 1),
    region = assign_region(decimalLatitude, decimalLongitude)
  ) %>%
  select(species, genus, decimalLatitude, decimalLongitude, year,
         iucn_status, region, data_source)

dir.create("data", showWarnings = FALSE)
write_csv(clean, "data/clean_occurrences.csv")
write_csv(res$log, "data/cleaning_log.csv")

print(res$log)
message("\nKept ", nrow(clean), " of ", res$log$remaining[1], " records -> data/clean_occurrences.csv")
