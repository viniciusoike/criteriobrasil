release_directory <- normalizePath(
  file.path(testthat::test_path(), "..", "..", "data-raw", "release"),
  mustWork = FALSE
)
if (!dir.exists(release_directory)) {
  testthat::skip("release assets are available in the source checkout")
}

test_that("release assets match the package registry", {
  registry <- criteriobrasil:::.cceb_table_registry

  for (index in seq_len(nrow(registry))) {
    table <- registry$table[[index]]
    path <- file.path(release_directory, registry$asset[[index]])

    expect_true(file.exists(path), info = table)
    if (!file.exists(path)) {
      next
    }
    data <- readRDS(path)
    expect_identical(names(data), registry$columns[[index]], info = table)
    expect_identical(
      sort(unique(data$edition_id)),
      registry$editions[[index]],
      info = table
    )
  }
})
