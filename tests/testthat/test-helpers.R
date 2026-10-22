library(testthat)
source("../../R/helpers.R")

test_that("diversity summary matches hand-calculated values", {
  data <- tibble::tibble(species = c("a", "a", "b"))
  result <- diversity_summary(data)

  expect_equal(result$richness, 2L)
  expect_equal(result$shannon, -(2 / 3 * log(2 / 3) + 1 / 3 * log(1 / 3)))
  expect_equal(result$simpson, 4 / 9)
  expect_equal(result$evenness, result$shannon / log(2))
})

test_that("grid cells report record and species richness", {
  data <- tibble::tibble(
    species = c("a", "b", "a"),
    decimalLatitude = c(-34.01, -34.02, -33.7),
    decimalLongitude = c(18.01, 18.02, 18.4)
  )

  cells <- richness_by_cell(data, 0.1)
  expect_equal(nrow(cells), 2L)
  expect_equal(sort(cells$records), c(1L, 2L))
  expect_equal(max(cells$richness), 2L)
})

test_that("regions are assigned to the nearest Cape reference point", {
  result <- assign_region(
    c(CAPE_REGIONS$lat[1], NA_real_),
    c(CAPE_REGIONS$lon[1], NA_real_)
  )

  expect_equal(result, c("Cape Peninsula", "Other"))
})

test_that("only coordinates with both values zero fail the zero-coordinate check", {
  raw <- tibble::tibble(
    species = c("Protea repens", "Protea nitida"),
    decimalLatitude = c(0, -34),
    decimalLongitude = c(0, 0),
    year = c(2020L, 2020L),
    coordinateUncertaintyInMeters = c(10, 10)
  )
  result <- clean_occurrences(raw)

  expect_equal(result$log$removed[result$log$step == "Zero coordinates"], 1L)
  expect_equal(result$log$removed[result$log$step == "Outside South Africa"], 1L)
})

test_that("species accumulation uses occupied grid cells as sites", {
  data <- tibble::tibble(
    species = c("a", "b", "a"),
    decimalLatitude = c(-34.01, -34.02, -33.7),
    decimalLongitude = c(18.01, 18.02, 18.4)
  )

  curve <- species_accumulation(data, 0.1)
  curve_again <- species_accumulation(data, 0.1)
  expect_equal(max(curve$sites), 2)
  expect_equal(tail(curve$richness, 1), 2)
  expect_equal(curve, curve_again)

  set.seed(2718)
  rng_before <- .Random.seed
  species_accumulation(data, 0.1)
  expect_identical(.Random.seed, rng_before)
})

test_that("rarefaction compares areas at equal effort", {
  data <- tibble::tibble(
    species = c("a", "a", "b", "a", "b"),
    region = c("A", "A", "A", "B", "B")
  )
  result <- compare_areas(data, "A", "B")

  expect_equal(result$rarefied_n, c(2L, 2L))
  expect_equal(result$rarefied_richness[1], 5 / 3)
  expect_equal(result$rarefied_richness[2], 2)
})

test_that("cleaning logs each sequential removal", {
  raw <- tibble::tibble(
    species = c("Protea repens", "Protea", NA, "Protea cynaroides", "Protea repens"),
    decimalLatitude = c(-34, -34, -34, NA, -34),
    decimalLongitude = c(19, 19, 19, 19, 19),
    year = c(2020, 2020, 2020, 2020, 2020),
    coordinateUncertaintyInMeters = c(10, 10, 10, 10, 10)
  )
  result <- clean_occurrences(raw)

  expect_equal(nrow(result$data), 1L)
  expect_equal(result$log$removed, c(0L, 2L, 1L, 0L, 0L, 0L, 0L, 0L, 1L))
  expect_equal(result$data$species, "Protea repens")
})

test_that("only normalized binomial species names survive", {
  raw <- tibble::tibble(
    species = c("Protea repens", " protea  nitida ", "Protea", NA_character_),
    decimalLatitude = rep(-34, 4),
    decimalLongitude = rep(19, 4),
    year = rep(2020L, 4),
    coordinateUncertaintyInMeters = rep(10, 4)
  )
  result <- clean_occurrences(raw)

  expect_equal(result$data$species, c("Protea repens", "Protea nitida"))
  expect_equal(result$log$removed[result$log$step == "Genus-only names"], 2L)
})
