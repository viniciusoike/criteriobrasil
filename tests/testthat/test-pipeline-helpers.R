helper_dir <- normalizePath(
  file.path(testthat::test_path(), "..", "..", "data-raw", "R"),
  mustWork = FALSE
)
if (!dir.exists(helper_dir)) {
  testthat::skip("data-raw helpers are available in the source checkout")
}

source(file.path(helper_dir, "pipeline_helpers.R"), local = environment())

.write_test_manifest <- function(path, pdf_path, hash = sha256_file(pdf_path)) {
  readr::write_csv(
    tibble::tibble(
      edition_id = 2026L,
      lang = "pt",
      file_name = basename(pdf_path),
      url = "https://example.com/source.pdf",
      sha256 = hash,
      accessed_at = "2026-09-10"
    ),
    path
  )
}

test_that("source verification accepts the recorded PDF", {
  directory <- tempfile("cceb-source-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  pdf_path <- file.path(directory, "source.pdf")
  writeBin(charToRaw("%PDF-test-source"), pdf_path)
  manifest_path <- file.path(directory, "manifest.csv")
  .write_test_manifest(manifest_path, pdf_path)

  expect_no_error(
    verify_source_files(manifest_path, directory, edition_ids = 2026L)
  )
})

test_that("source verification rejects a changed PDF", {
  directory <- tempfile("cceb-source-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  pdf_path <- file.path(directory, "source.pdf")
  writeBin(charToRaw("%PDF-original"), pdf_path)
  manifest_path <- file.path(directory, "manifest.csv")
  .write_test_manifest(manifest_path, pdf_path)
  writeBin(charToRaw("%PDF-changed"), pdf_path)

  expect_snapshot(
    error = TRUE,
    verify_source_files(manifest_path, directory, edition_ids = 2026L)
  )
})

test_that("ordinary manifest updates reject changed source identity", {
  directory <- tempfile("cceb-source-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  pdf_path <- file.path(directory, "source.pdf")
  writeBin(charToRaw("%PDF-original"), pdf_path)
  manifest_path <- file.path(directory, "manifest.csv")
  .write_test_manifest(manifest_path, pdf_path)
  writeBin(charToRaw("%PDF-changed"), pdf_path)
  link <- tibble::tibble(
    edition_id = 2026L,
    lang = "pt",
    file_name = "source.pdf",
    url = "https://example.com/source.pdf"
  )

  expect_snapshot(
    error = TRUE,
    update_manifest(link, pdf_path, manifest_path)
  )
})
