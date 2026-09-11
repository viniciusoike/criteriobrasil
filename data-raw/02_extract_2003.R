# Extract the Portuguese PDFs in the 2003--2014 CCEB regime.

source("data-raw/R/pipeline_helpers.R")
source("data-raw/R/tidy_helpers.R")
source("data-raw/R/extract_2003.R")
source("data-raw/R/tidy_2003.R")

cceb_2003_manifest <- verify_source_files(
  edition_ids = .cceb_2003_ids,
  lang = "pt"
)

missing_ids <- setdiff(.cceb_2003_ids, cceb_2003_manifest$edition_id)
if (length(missing_ids) > 0L) {
  cli::cli_abort(
    "The manifest is missing Portuguese legacy editions: {paste(missing_ids, collapse = ', ')}."
  )
}

cceb_2003_by_edition <- purrr::map(.cceb_2003_ids, function(edition) {
  link <- edition_source_link(cceb_2003_manifest, edition)
  pdf <- read_cceb_pdf(edition_source_pdf(cceb_2003_manifest, edition))
  raw <- extract_2003_raw(pdf, edition)

  tidy_2003(raw, edition_id = edition, links = link)
})

cceb_2003_data <- bind_cceb_tables(cceb_2003_by_edition)
