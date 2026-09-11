helper_dir <- normalizePath(
  file.path(testthat::test_path(), "..", "..", "data-raw", "R"),
  mustWork = FALSE
)
if (!dir.exists(helper_dir)) {
  testthat::skip("data-raw helpers are available in the source checkout")
}

source(file.path(helper_dir, "pipeline_helpers.R"), local = environment())
source(file.path(helper_dir, "extract_2015.R"), local = environment())
source(file.path(helper_dir, "tidy_2015.R"), local = environment())
source(file.path(helper_dir, "validate.R"), local = environment())
source(file.path(helper_dir, "validate_2015.R"), local = environment())

.cceb_2015_test_ids <- c(
  2015L,
  2016L,
  2018L,
  2019L,
  2020L,
  2021L,
  2022L,
  2024L
)

.cceb_2015_test_pdf <- function(edition_id) {
  manifest <- readr::read_csv(
    file.path(testthat::test_path(), "..", "..", "data-raw", "manifest.csv"),
    show_col_types = FALSE
  )
  row <- manifest[
    manifest$edition_id == edition_id & manifest$lang == "pt",
    ,
    drop = FALSE
  ]
  file.path(
    testthat::test_path(),
    "..",
    "..",
    "data-raw",
    "pdf",
    row$file_name[[1]]
  )
}

test_that("recent Portuguese regime PDFs pass structural validation", {
  paths <- vapply(.cceb_2015_test_ids, .cceb_2015_test_pdf, character(1))
  testthat::skip_if_not(all(file.exists(paths)))

  for (index in seq_along(.cceb_2015_test_ids)) {
    edition_id <- .cceb_2015_test_ids[[index]]
    raw <- extract_2015_raw(read_cceb_pdf(paths[[index]]))
    data <- tidy_2015(raw, edition_id = edition_id)

    validate_2015_data(data, edition_id = edition_id)
    expect_identical(nrow(data$cceb_points), 69L)
    expect_identical(nrow(data$cceb_distribution), 96L)
  }
})

test_that("known source variations remain visible in the tidy tables", {
  path_2018 <- .cceb_2015_test_pdf(2018L)
  path_2021 <- .cceb_2015_test_pdf(2021L)
  testthat::skip_if_not(file.exists(path_2018) && file.exists(path_2021))

  data_2018 <- tidy_2015(
    extract_2015_raw(read_cceb_pdf(path_2018)),
    edition_id = 2018L
  )
  data_2021 <- tidy_2015(
    extract_2015_raw(read_cceb_pdf(path_2021)),
    edition_id = 2021L
  )

  expect_identical(
    data_2018$cceb_cutoffs$points_min[
      data_2018$cceb_cutoffs$class == "DE"
    ],
    1L
  )
  expect_equal(
    data_2021$cceb_distribution$share[
      data_2021$cceb_distribution$geo_code == "BR" &
        data_2021$cceb_distribution$class == "A"
    ],
    0.028,
    tolerance = 1e-12
  )
})
