# Extract the Portuguese PDFs in the 2003--2014 CCEB regime.

source("data-raw/R/pipeline_helpers.R")
source("data-raw/R/extract_2003.R")
source("data-raw/R/tidy_2003.R")

if (!file.exists("data-raw/manifest.csv")) {
  cli::cli_abort("The download manifest is required before extraction.")
}

cceb_2003_manifest <- verify_source_files(
  edition_ids = .cceb_2003_ids,
  lang = "pt"
)
cceb_2003_manifest <- cceb_2003_manifest[
  cceb_2003_manifest$edition_id %in%
    .cceb_2003_ids &
    cceb_2003_manifest$lang == "pt",
  ,
  drop = FALSE
]

missing_ids <- setdiff(.cceb_2003_ids, cceb_2003_manifest$edition_id)
if (length(missing_ids) > 0L) {
  cli::cli_abort(
    "The manifest is missing Portuguese legacy editions: {paste(missing_ids, collapse = ', ')}."
  )
}

cceb_2003_by_edition <- purrr::map(.cceb_2003_ids, function(edition_id) {
  link <- cceb_2003_manifest[
    cceb_2003_manifest$edition_id == edition_id,
    ,
    drop = FALSE
  ]
  pdf_path <- file.path("data-raw/pdf", link$file_name[[1]])
  if (!file.exists(pdf_path)) {
    cli::cli_abort("The downloaded PDF {.path {pdf_path}} is missing.")
  }

  raw <- extract_2003_raw(read_cceb_pdf(pdf_path), edition_id)
  tidy_2003(raw, edition_id = edition_id, links = link)
})

cceb_2003_data <- list(
  cceb_editions = dplyr::bind_rows(
    purrr::map(cceb_2003_by_edition, "cceb_editions")
  ),
  cceb_points = dplyr::bind_rows(
    purrr::map(cceb_2003_by_edition, "cceb_points")
  ),
  cceb_cutoffs = dplyr::bind_rows(
    purrr::map(cceb_2003_by_edition, "cceb_cutoffs")
  ),
  cceb_income = dplyr::bind_rows(
    purrr::map(cceb_2003_by_edition, "cceb_income")
  ),
  cceb_distribution = dplyr::bind_rows(
    purrr::map(cceb_2003_by_edition, "cceb_distribution")
  )
)
