helper_dir <- normalizePath(
  file.path(testthat::test_path(), "..", "..", "data-raw", "R"),
  mustWork = FALSE
)
if (!dir.exists(helper_dir)) {
  testthat::skip("data-raw helpers are available in the source checkout")
}

source(file.path(helper_dir, "validate.R"), local = environment())

.load_package_data <- function() {
  root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = TRUE
  )
  tables <- .cceb_required_tables
  result <- setNames(vector("list", length(tables)), tables)
  for (table_name in tables) {
    environment <- new.env(parent = emptyenv())
    load(
      file.path(root, "data", paste0(table_name, ".rda")),
      envir = environment
    )
    result[[table_name]] <- environment[[table_name]]
  }

  return(result)
}

test_that("the package data passes generic validation", {
  data <- .load_package_data()

  expect_no_error(validate_cceb_data(data))
})

test_that("duplicate point keys fail validation", {
  data <- .load_package_data()
  data$cceb_points <- rbind(data$cceb_points, data$cceb_points[1L, ])

  expect_snapshot(error = TRUE, validate_cceb_data(data))
})

test_that("cutoff gaps fail validation", {
  data <- .load_package_data()
  index <- which(
    data$cceb_cutoffs$edition_id == 2026L &
      data$cceb_cutoffs$class == "DE"
  )
  data$cceb_cutoffs$points_max[index] <- 33L

  expect_snapshot(error = TRUE, validate_cceb_data(data))
})

test_that("invalid income values fail validation", {
  data <- .load_package_data()
  data$cceb_income$income_mean[[1L]] <- Inf

  expect_snapshot(error = TRUE, validate_cceb_data(data))
})

test_that("invalid distribution totals fail validation", {
  data <- .load_package_data()
  data$cceb_distribution$share[[1L]] <- 0.5

  expect_snapshot(error = TRUE, validate_cceb_data(data))
})

test_that("incomplete distribution class sets fail validation", {
  data <- .load_package_data()
  index <- which(
    data$cceb_distribution$edition_id == 2026L &
      data$cceb_distribution$geo_code == "BR" &
      data$cceb_distribution$class == "A"
  )
  data$cceb_distribution$class[index] <- "unexpected"

  expect_snapshot(error = TRUE, validate_cceb_data(data))
})

test_that("orphan editions fail relational validation", {
  data <- .load_package_data()
  data$cceb_income$edition_id[[1L]] <- 9999L

  expect_snapshot(error = TRUE, validate_cceb_data(data))
})
