# Refresh source PDFs and their manifest entries after manual source review.
#
# This script deliberately replaces committed hashes. Review the PDF contents
# and the resulting manifest diff before rebuilding package data.

source("data-raw/R/pipeline_helpers.R")

cceb_edition_ids <- c(
  2003L,
  2008L,
  2009L,
  2010L,
  2011L,
  2012L,
  2013L,
  2014L,
  2015L,
  2016L,
  2018L,
  2019L,
  2020L,
  2021L,
  2022L,
  2024L,
  2026L
)

for (edition_id in cceb_edition_ids) {
  if (as.character(edition_id) %in% names(.cceb_legacy_urls)) {
    url <- unname(.cceb_legacy_urls[as.character(edition_id)])
    links <- tibble::tibble(
      edition_id = edition_id,
      label = NA_character_,
      url = url,
      file_name = basename(url),
      lang = "pt"
    )
  } else {
    links <- get_cceb_links(edition_id = edition_id)
    links <- links[links$lang == "pt", , drop = FALSE]
  }
  if (nrow(links) != 1L) {
    cli::cli_abort(
      "Expected one Portuguese PDF for edition {edition_id}; found {nrow(links)}."
    )
  }

  destination <- file.path("data-raw", "pdf", links$file_name[[1]])
  download_with_retry(links$url[[1]], destination, overwrite = TRUE)
  update_manifest(links, destination, replace = TRUE)
}
