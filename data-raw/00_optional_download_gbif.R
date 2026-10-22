# data-raw/00_optional_download_gbif.R --------------------------------------
# OPTIONAL. Downloads real Protea records for South Africa from GBIF.
# Not tested against the live API, so treat it as a starting point.
#
# If this keeps failing, download manually instead:
#   1. gbif.org > Occurrences > filter: Country = South Africa, Scientific name = Protea,
#      "Has coordinate" = yes, "Has geospatial issue" = no
#   2. Download > Simple
#   3. Unzip and save the file as data-raw/raw_occurrences.csv
#   Then run data-raw/02_clean_occurrences.R
#
# install.packages("rgbif")

library(rgbif)
library(dplyr)
library(readr)

key <- name_backbone(name = "Protea", rank = "genus")$usageKey

res <- occ_data(
  taxonKey = key,
  country = "ZA",
  hasCoordinate = TRUE,
  hasGeospatialIssue = FALSE,
  limit = 20000
)

records <- res$data %>%
  select(any_of(c("species", "decimalLatitude", "decimalLongitude", "year",
                  "coordinateUncertaintyInMeters")))

write_csv(records, "data-raw/raw_occurrences.csv")
message("Saved ", nrow(records), " records to data-raw/raw_occurrences.csv")

# IUCN status: either build data-raw/iucn_status.csv by hand (columns: species,
# iucn_status) from the Red List website / SANBI, or use the rredlist package with
# a free IUCN API token. Species missing from that file become "Not Evaluated".
