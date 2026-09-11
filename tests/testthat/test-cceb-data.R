.clear_cceb_asset_cache <- function() {
  cache <- criteriobrasil:::.cceb_asset_cache
  rm(list = ls(cache, all.names = TRUE), envir = cache)
}

.cceb_test_fixtures <- function() {
  registry <- criteriobrasil:::.cceb_table_registry
  fixtures <- lapply(seq_len(nrow(registry)), function(index) {
    columns <- registry$columns[[index]]
    editions <- registry$editions[[index]]
    data <- setNames(
      rep(list(rep(NA_character_, length(editions))), length(columns)),
      columns
    )
    data$edition_id <- editions
    tibble::as_tibble(data)
  })

  setNames(fixtures, registry$asset)
}

.mock_cceb_release <- function() {
  .clear_cceb_asset_cache()
  fixtures <- .cceb_test_fixtures()

  testthat::local_mocked_bindings(
    .cceb_download_file = function(url, destination) {
      saveRDS(fixtures[[basename(url)]], destination, version = 2L)
    },
    .package = "criteriobrasil",
    .env = parent.frame()
  )
}

test_that("cceb_list_tables describes the local registry", {
  tables <- cceb_list_tables()

  expect_s3_class(tables, "tbl_df")
  expect_named(tables, c("table", "description", "editions"))
  expect_identical(
    tables$table,
    c("editions", "points", "cutoffs", "income", "distribution")
  )
  expect_identical(
    tables$editions[[4L]],
    c(
      2003L,
      2008L,
      2009L,
      2010L,
      2011L,
      2012L,
      2013L,
      2014L,
      2016L,
      2018L,
      2019L,
      2020L,
      2021L,
      2022L,
      2024L,
      2026L
    )
  )
})

test_that("cceb_get defaults to the latest distribution", {
  .mock_cceb_release()
  fixtures <- .cceb_test_fixtures()

  result <- cceb_get()
  distribution <- fixtures$cceb_distribution.rds
  expected <- distribution[
    distribution$edition_id == 2026L,
    ,
    drop = FALSE
  ]

  expect_s3_class(result, "tbl_df")
  expect_identical(result, expected)
})

test_that("cceb_get selects one edition or all editions", {
  .mock_cceb_release()
  income <- .cceb_test_fixtures()$cceb_income.rds

  one <- cceb_get("income", edition = 2024)
  all <- cceb_get("income", edition = "all")

  expect_identical(unique(one$edition_id), 2024L)
  expect_identical(one, income[income$edition_id == 2024L, ])
  expect_identical(all, income)
})

test_that("cceb_get returns every table in registry order", {
  .mock_cceb_release()
  points <- .cceb_test_fixtures()$cceb_points.rds

  latest <- cceb_get("all")
  all <- cceb_get("all", edition = "all")

  expect_named(latest, cceb_list_tables()$table)
  expect_named(all, cceb_list_tables()$table)
  expect_identical(
    vapply(latest, \(data) unique(data$edition_id), integer(1)),
    setNames(rep(2026L, 5L), cceb_list_tables()$table)
  )
  expect_identical(all$points, points)
})

test_that("cceb_get validates its arguments before downloading", {
  .clear_cceb_asset_cache()
  testthat::local_mocked_bindings(
    .cceb_download_file = function(...) {
      fail("download should not be attempted")
    },
    .package = "criteriobrasil"
  )

  expect_snapshot(error = TRUE, cceb_get("unknown"))
  expect_snapshot(error = TRUE, cceb_get(table = NA_character_))
  expect_snapshot(error = TRUE, cceb_get(edition = "2024"))
  expect_snapshot(error = TRUE, cceb_get(edition = c(2024, 2026)))
  expect_snapshot(error = TRUE, cceb_get("income", edition = 2015))
  expect_snapshot(error = TRUE, cceb_get("all", edition = 2015))
})

test_that("cceb_get memoizes a downloaded asset within the session", {
  .clear_cceb_asset_cache()
  points <- .cceb_test_fixtures()$cceb_points.rds
  downloads <- 0L
  testthat::local_mocked_bindings(
    .cceb_download_file = function(url, destination) {
      downloads <<- downloads + 1L
      saveRDS(points, destination, version = 2L)
    },
    .package = "criteriobrasil"
  )

  cceb_get("points", edition = 2024)
  cceb_get("points", edition = 2026)

  expect_identical(downloads, 1L)
})

test_that("cceb_get reports download and asset errors", {
  .clear_cceb_asset_cache()
  testthat::local_mocked_bindings(
    .cceb_download_file = function(...) stop("network unavailable"),
    .package = "criteriobrasil"
  )
  expect_snapshot(error = TRUE, cceb_get("points"))

  .clear_cceb_asset_cache()
  testthat::local_mocked_bindings(
    .cceb_download_file = function(url, destination) {
      saveRDS(data.frame(wrong = 1L), destination, version = 2L)
    },
    .package = "criteriobrasil"
  )
  expect_snapshot(error = TRUE, cceb_get(table = "points"))

  .clear_cceb_asset_cache()
  points <- .cceb_test_fixtures()$cceb_points.rds
  points$edition_id[[1L]] <- 9999L
  testthat::local_mocked_bindings(
    .cceb_download_file = function(url, destination) {
      saveRDS(points, destination, version = 2L)
    },
    .package = "criteriobrasil"
  )
  expect_snapshot(error = TRUE, cceb_get("points", edition = NULL))
})
