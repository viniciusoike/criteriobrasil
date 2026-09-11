# Refresh source PDFs and their manifest entries after manual source review.
#
# This script deliberately replaces committed hashes. Review the PDF contents
# and the resulting manifest diff before rebuilding package data.

source("data-raw/R/pipeline_helpers.R")

purrr::walk(
  .cceb_edition_ids,
  download_cceb_edition,
  overwrite = TRUE,
  replace = TRUE
)
