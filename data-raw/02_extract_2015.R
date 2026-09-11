# Extract the Portuguese PDFs in the 2015--2024 CCEB regime.

source("data-raw/R/pipeline_helpers.R")
source("data-raw/R/tidy_helpers.R")
source("data-raw/R/extract_2015.R")
source("data-raw/R/tidy_2015.R")

cceb_2015_ids <- c(2015L, 2016L, 2018L, 2019L, 2020L, 2021L, 2022L, 2024L)

cceb_2015_manifest <- verify_source_files(
  edition_ids = cceb_2015_ids,
  lang = "pt"
)

missing_ids <- setdiff(cceb_2015_ids, cceb_2015_manifest$edition_id)
if (length(missing_ids) > 0L) {
  cli::cli_abort(
    "The manifest is missing Portuguese editions: {paste(missing_ids, collapse = ', ')}."
  )
}

cceb_2015_by_edition <- purrr::map(cceb_2015_ids, function(edition) {
  link <- edition_source_link(cceb_2015_manifest, edition)
  pdf <- read_cceb_pdf(edition_source_pdf(cceb_2015_manifest, edition))
  raw <- extract_2015_raw(pdf)

  tidy_2015(raw, edition_id = edition, links = link)
})

cceb_2015_data <- bind_cceb_tables(cceb_2015_by_edition)
