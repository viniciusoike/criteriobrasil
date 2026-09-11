helper_dir <- normalizePath(
  file.path(testthat::test_path(), "..", "..", "data-raw", "R"),
  mustWork = FALSE
)
if (!dir.exists(helper_dir)) {
  testthat::skip("data-raw helpers are available in the source checkout")
}

source(file.path(helper_dir, "pipeline_helpers.R"), local = environment())
source(file.path(helper_dir, "validate.R"), local = environment())
source(file.path(helper_dir, "fidelity.R"), local = environment())

.load_fidelity_data <- function() {
  root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = TRUE
  )
  result <- setNames(
    vector("list", length(.cceb_required_tables)),
    .cceb_required_tables
  )
  for (table_name in .cceb_required_tables) {
    environment <- new.env(parent = emptyenv())
    load(
      file.path(root, "data", paste0(table_name, ".rda")),
      envir = environment
    )
    result[[table_name]] <- environment[[table_name]]
  }

  return(result)
}

.fidelity_paths <- function() {
  root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = TRUE
  )
  list(
    fingerprints = file.path(root, "data-raw", "fidelity.csv"),
    sources = file.path(root, "data-raw", "manifest.csv")
  )
}

test_that("package data matches every source fingerprint", {
  data <- .load_fidelity_data()
  paths <- .fidelity_paths()

  expect_no_error(
    verify_cceb_fidelity(data, paths$fingerprints, paths$sources)
  )
})

test_that("a plausible income change fails source fidelity", {
  data <- .load_fidelity_data()
  paths <- .fidelity_paths()
  data$cceb_income$income_mean[[1L]] <-
    data$cceb_income$income_mean[[1L]] + 1
  expect_no_error(validate_cceb_data(data))

  expect_snapshot(
    error = TRUE,
    verify_cceb_fidelity(data, paths$fingerprints, paths$sources)
  )
})

test_that("a geographic column swap fails source fidelity", {
  data <- .load_fidelity_data()
  paths <- .fidelity_paths()
  distribution <- data$cceb_distribution
  br <- which(distribution$edition_id == 2026L & distribution$geo_code == "BR")
  se <- which(distribution$edition_id == 2026L & distribution$geo_code == "SE")
  distribution$share[c(br, se)] <- distribution$share[c(se, br)]
  data$cceb_distribution <- distribution
  expect_no_error(validate_cceb_data(data))

  expect_snapshot(
    error = TRUE,
    verify_cceb_fidelity(data, paths$fingerprints, paths$sources)
  )
})
