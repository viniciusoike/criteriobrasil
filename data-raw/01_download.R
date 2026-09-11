# Download the source PDFs listed by the ABEP CCEB page.
#
# PDFs are deliberately kept outside version control in `data-raw/pdf/`.
# Record one row per downloaded file in `data-raw/manifest.csv`, including
# the URL, SHA-256 hash, and access date.
#
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
    links <- tibble::tibble(
      edition_id = edition_id,
      label = NA_character_,
      url = unname(.cceb_legacy_urls[as.character(edition_id)]),
      file_name = basename(
        unname(.cceb_legacy_urls[as.character(edition_id)])
      ),
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
  download_with_retry(links$url[[1]], destination)
  update_manifest(links, destination)
}
