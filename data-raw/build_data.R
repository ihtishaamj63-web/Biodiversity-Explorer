# data-raw/build_data.R -----------------------------------------------------
# One-step demo data build. Run from the project root:
#   source("data-raw/build_data.R")
#
# Using real GBIF data instead? Skip step 1, save your download as
# data-raw/raw_occurrences.csv (plus data-raw/iucn_status.csv), and run only step 2.

source("data-raw/01_generate_mock_data.R")   # step 1: simulate raw, messy records
source("data-raw/02_clean_occurrences.R")    # step 2: clean + join IUCN + assign regions
