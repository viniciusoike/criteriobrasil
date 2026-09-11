helper_dir <- normalizePath(
  file.path(testthat::test_path(), "..", "..", "data-raw", "R"),
  mustWork = FALSE
)
if (!dir.exists(helper_dir)) {
  testthat::skip("data-raw helpers are available in the source checkout")
}

source(file.path(helper_dir, "pipeline_helpers.R"), local = environment())
source(file.path(helper_dir, "tidy_helpers.R"), local = environment())
source(file.path(helper_dir, "extract_2003.R"), local = environment())
source(file.path(helper_dir, "tidy_2003.R"), local = environment())
source(file.path(helper_dir, "validate.R"), local = environment())
source(file.path(helper_dir, "validate_2003.R"), local = environment())

.cceb_2003_test_ids <- c(
  2003L,
  2008L,
  2009L,
  2010L,
  2011L,
  2012L,
  2013L,
  2014L
)

.cceb_2003_test_manifest <- function() {
  readr::read_csv(
    file.path(testthat::test_path(), "..", "..", "data-raw", "manifest.csv"),
    show_col_types = FALSE
  )
}

.cceb_2003_test_pdf <- function(edition_id) {
  manifest <- .cceb_2003_test_manifest()
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

test_that("legacy Portuguese regime PDFs pass structural validation", {
  paths <- vapply(.cceb_2003_test_ids, .cceb_2003_test_pdf, character(1))
  testthat::skip_if_not(all(file.exists(paths)))

  for (index in seq_along(.cceb_2003_test_ids)) {
    edition_id <- .cceb_2003_test_ids[[index]]
    raw <- extract_2003_raw(read_cceb_pdf(paths[[index]]), edition_id)
    data <- tidy_2003(raw, edition_id = edition_id)

    validate_2003_data(data, edition_id = edition_id)
    expected_points <- if (edition_id == 2003L) 55L else 50L
    expected_income <- if (edition_id == 2009L) {
      16L
    } else if (edition_id %in% c(2013L, 2014L)) {
      6L
    } else if (edition_id == 2003L) {
      7L
    } else {
      8L
    }
    expected_distribution <- if (edition_id == 2009L) {
      160L
    } else if (edition_id %in% c(2003L, 2014L)) {
      70L
    } else if (edition_id == 2013L) {
      70L
    } else {
      80L
    }

    expect_identical(nrow(data$cceb_points), expected_points)
    expect_identical(
      nrow(data$cceb_cutoffs),
      length(.cceb_legacy_classes(edition_id))
    )
    expect_identical(nrow(data$cceb_income), expected_income)
    expect_identical(nrow(data$cceb_distribution), expected_distribution)
  }
})

test_that("known legacy source variations remain visible in the tidy tables", {
  paths <- vapply(.cceb_2003_test_ids, .cceb_2003_test_pdf, character(1))
  testthat::skip_if_not(all(file.exists(paths)))

  data_2003 <- tidy_2003(
    extract_2003_raw(read_cceb_pdf(paths[[1]]), edition_id = 2003L),
    edition_id = 2003L
  )
  data_2009 <- tidy_2003(
    extract_2003_raw(read_cceb_pdf(paths[[3]]), edition_id = 2009L),
    edition_id = 2009L
  )
  data_2011 <- tidy_2003(
    extract_2003_raw(read_cceb_pdf(paths[[5]]), edition_id = 2011L),
    edition_id = 2011L
  )
  data_2012 <- tidy_2003(
    extract_2003_raw(read_cceb_pdf(paths[[6]]), edition_id = 2012L),
    edition_id = 2012L
  )
  data_2013 <- tidy_2003(
    extract_2003_raw(read_cceb_pdf(paths[[7]]), edition_id = 2013L),
    edition_id = 2013L
  )
  data_2014 <- tidy_2003(
    extract_2003_raw(read_cceb_pdf(paths[[8]]), edition_id = 2014L),
    edition_id = 2014L
  )

  expect_true("vacuum_cleaners" %in% data_2003$cceb_points$variable)
  expect_true("C" %in% data_2003$cceb_cutoffs$class)
  expect_setequal(
    unique(data_2009$cceb_income$income_ref_year),
    c(2006L, 2007L)
  )
  expect_setequal(
    unique(data_2009$cceb_distribution$ref_year),
    c(2006L, 2007L)
  )
  expect_equal(
    data_2009$cceb_distribution$share[
      data_2009$cceb_distribution$ref_year == 2007L &
        data_2009$cceb_distribution$geo_code == "BR" &
        data_2009$cceb_distribution$class == "A1"
    ],
    0.0072
  )
  expect_equal(
    data_2011$cceb_distribution$share[
      data_2011$cceb_distribution$geo_code == "FOR" &
        data_2011$cceb_distribution$class == "A1"
    ],
    0.002
  )
  expect_equal(
    data_2012$cceb_distribution$share[
      data_2012$cceb_distribution$geo_code == "BSB" &
        data_2012$cceb_distribution$class == "A1"
    ],
    0.009
  )
  expect_identical(
    data_2013$cceb_income$income_mean,
    c(
      9263,
      5241,
      2654,
      1685,
      1147,
      776
    )
  )
  expect_equal(
    data_2013$cceb_distribution$share[
      data_2013$cceb_distribution$geo_code == "FOR" &
        data_2013$cceb_distribution$class == "A1"
    ],
    0.006
  )
  expect_true("DE" %in% data_2014$cceb_distribution$class)
  expect_false(any(data_2014$cceb_distribution$class %in% c("D", "E")))
  expect_equal(
    data_2014$cceb_distribution$share[
      data_2014$cceb_distribution$geo_code == "BSB" &
        data_2014$cceb_distribution$class == "A1"
    ],
    0.019
  )
  expect_true("D" %in% data_2014$cceb_cutoffs$class)
  expect_true("E" %in% data_2014$cceb_cutoffs$class)
  expect_true("A" %in% data_2014$cceb_income$class)
  expect_false(any(data_2014$cceb_income$class %in% c("A1", "A2")))
})
