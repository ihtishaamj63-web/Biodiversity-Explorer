# data-raw/01_generate_mock_data.R ------------------------------------------
# Simulates GBIF-style Cape Fynbos occurrence records, then
# deliberately messes some of them up so the cleaning step has real work to do.
#
# Run from the project root:  source("data-raw/01_generate_mock_data.R")
#
# IMPORTANT: species ranges and IUCN statuses below are ILLUSTRATIVE PLACEHOLDERS,
# not real assessments. The app shows a "demo data" banner while this is in use.

suppressPackageStartupMessages(library(tidyverse))
source("R/helpers.R")

set.seed(42)
n <- 12000

# ---- Species x region weights (how likely a record is each species in each region)
# Columns follow CAPE_REGIONS$region:
#   Cape Peninsula, Boland, Overberg, Cederberg, Little Karoo, Garden Route
W <- rbind(
  "Protea cynaroides"      = c(3, 3, 3, 2, 1, 3),
  "Protea repens"          = c(3, 3, 3, 2, 2, 3),
  "Protea neriifolia"      = c(3, 3, 2, 1, 0, 1),
  "Protea nitida"          = c(1, 3, 1, 2, 2, 1),
  "Protea eximia"          = c(0, 1, 3, 0, 2, 3),
  "Protea laurifolia"      = c(0, 2, 0, 3, 1, 2),
  "Protea compacta"        = c(0, 2, 3, 0, 0, 1),
  "Protea magnifica"       = c(0, 0, 2, 2, 3, 0),
  "Protea lepidocarpodendron" = c(3, 2, 0, 0, 0, 0),
  "Protea burchellii"      = c(0, 0, 2, 0, 3, 2),
  "Protea odorata"         = c(1, 3, 0, 0, 0, 0),
  "Leucospermum cordifolium" = c(1, 3, 3, 0, 0, 0),
  "Leucospermum conocarpodendron" = c(3, 2, 0, 0, 0, 0),
  "Leucospermum reflexum" = c(0, 0, 0, 3, 0, 0),
  "Leucadendron argenteum" = c(3, 2, 0, 0, 0, 0),
  "Leucadendron salignum" = c(3, 3, 3, 2, 2, 3),
  "Leucadendron laureolum" = c(3, 2, 2, 0, 0, 0),
  "Leucadendron rubrum" = c(0, 1, 1, 2, 3, 2),
  "Mimetes cucullatus" = c(3, 3, 3, 1, 0, 1),
  "Serruria florida" = c(0, 2, 0, 0, 0, 0),
  "Serruria fasciflora" = c(3, 1, 0, 0, 0, 0),
  "Orothamnus zeyheri" = c(0, 0, 3, 0, 0, 0),
  "Aulax umbellata" = c(0, 2, 3, 1, 0, 2),
  "Erica cerinthoides" = c(3, 3, 3, 2, 2, 3),
  "Erica plukenetii" = c(1, 3, 2, 1, 0, 2)
)
colnames(W) <- CAPE_REGIONS$region

# All statuses are ILLUSTRATIVE PLACEHOLDERS, not real IUCN/SANBI assessments.
# Keep the demo-data banner in the app when these simulated records are used.
status <- c(
  "Protea cynaroides" = "Least Concern",
  "Protea repens" = "Least Concern",
  "Protea neriifolia" = "Least Concern",
  "Protea nitida" = "Least Concern",
  "Protea eximia" = "Least Concern",
  "Protea laurifolia" = "Least Concern",
  "Protea compacta" = "Near Threatened",
  "Protea magnifica" = "Near Threatened",
  "Protea lepidocarpodendron" = "Vulnerable",
  "Protea burchellii" = "Vulnerable",
  "Protea odorata" = "Endangered",
  "Leucospermum cordifolium" = "Near Threatened",
  "Leucospermum conocarpodendron" = "Least Concern",
  "Leucospermum reflexum" = "Near Threatened",
  "Leucadendron argenteum" = "Vulnerable",
  "Leucadendron salignum" = "Least Concern",
  "Leucadendron laureolum" = "Least Concern",
  "Leucadendron rubrum" = "Least Concern",
  "Mimetes cucullatus" = "Least Concern",
  "Serruria florida" = "Endangered",
  "Serruria fasciflora" = "Near Threatened",
  "Orothamnus zeyheri" = "Critically Endangered",
  "Aulax umbellata" = "Least Concern",
  "Erica cerinthoides" = "Least Concern",
  "Erica plukenetii" = "Least Concern"
)
species_info <- tibble(species = rownames(W), iucn_status = unname(status[rownames(W)]))
stopifnot(!anyNA(species_info$iucn_status))

# ---- Where records come from: survey effort is uneven (Peninsula is over-sampled)
effort <- c(3, 2.5, 2, 1, 0.8, 1.5)
region <- sample(CAPE_REGIONS$region, n, replace = TRUE, prob = effort)

species <- vapply(
  region,
  function(r) sample(rownames(W), 1, prob = W[, r]),
  character(1),
  USE.NAMES = FALSE
)

centre_lat <- CAPE_REGIONS$lat[match(region, CAPE_REGIONS$region)]
centre_lon <- CAPE_REGIONS$lon[match(region, CAPE_REGIONS$region)]

# ---- When: recording ramps up in recent years (citizen-science effect)
years <- 1990:2024
year_weight <- case_when(years >= 2015 ~ 6, years >= 2005 ~ 2, TRUE ~ 1)

raw <- tibble(
  species = species,
  decimalLatitude  = round(centre_lat + rnorm(n, 0, 0.2), 5),
  decimalLongitude = round(centre_lon + rnorm(n, 0, 0.25), 5),
  year = sample(years, n, replace = TRUE, prob = year_weight),
  coordinateUncertaintyInMeters = round(rlnorm(n, log(30), 1)),
  data_source = "mock"
)

# ---- Make the data dirty (what real GBIF downloads look like)
big <- sample(n, round(0.03 * n))
raw$coordinateUncertaintyInMeters[big] <- 50000

counts <- c(na_coord = 160, zero = 60, swap = 80, na_year = 80,
            old_year = 20, lower = 40, genus = 40)
idx <- sample(n, sum(counts))
grp <- rep(names(counts), times = counts)
pick <- function(g) idx[grp == g]

raw$decimalLatitude[pick("na_coord")]  <- NA
raw$decimalLongitude[pick("na_coord")] <- NA

raw$decimalLatitude[pick("zero")]  <- 0
raw$decimalLongitude[pick("zero")] <- 0

i <- pick("swap")
tmp <- raw$decimalLatitude[i]
raw$decimalLatitude[i]  <- raw$decimalLongitude[i]
raw$decimalLongitude[i] <- tmp

raw$year[pick("na_year")]  <- NA
raw$year[pick("old_year")] <- 1850L

raw$species[pick("lower")] <- paste0(" ", str_to_lower(raw$species[pick("lower")]), " ")
raw$species[pick("genus")] <- "Protea"

dups <- raw[sample(nrow(raw), 206), ]
raw  <- bind_rows(raw, dups) %>% slice_sample(prop = 1)

# ---- Save
write_csv(raw, "data-raw/raw_occurrences.csv")
write_csv(species_info, "data-raw/iucn_status.csv")

message("Wrote ", nrow(raw), " raw records to data-raw/raw_occurrences.csv")
message("Wrote ", nrow(species_info), " species statuses to data-raw/iucn_status.csv")
