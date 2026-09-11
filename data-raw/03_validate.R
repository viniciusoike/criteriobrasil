# Validate the extracted tables before they are added to `data/`.
#
# Planned checks include score maxima, contiguous cutoffs, distribution sums,
# and parity between Portuguese and English source documents.

source("data-raw/R/pipeline_helpers.R")
source("data-raw/R/validate.R")
source("data-raw/R/fidelity.R")
source("data-raw/R/extract_2026.R")
source("data-raw/R/tidy_2026.R")
source("data-raw/R/validate_2026.R")
source("data-raw/R/extract_2015.R")
source("data-raw/R/tidy_2015.R")
source("data-raw/R/validate_2015.R")
source("data-raw/R/extract_2003.R")
source("data-raw/R/tidy_2003.R")
source("data-raw/R/validate_2003.R")
source("data-raw/02_extract_2003.R")
source("data-raw/02_extract_2026.R")
source("data-raw/02_extract_2015.R")

purrr::walk(.cceb_2003_ids, function(edition_id) {
  edition_data <- purrr::map(
    cceb_2003_data,
    function(table) table[table$edition_id == edition_id, , drop = FALSE]
  )
  validate_2003_data(edition_data, edition_id = edition_id)
})
validate_2026_data(cceb_2026_data)
purrr::walk(cceb_2015_ids, function(edition_id) {
  edition_data <- purrr::map(
    cceb_2015_data,
    function(table) table[table$edition_id == edition_id, , drop = FALSE]
  )
  validate_2015_data(edition_data, edition_id = edition_id)
})

cceb_validated_data <- list(
  cceb_editions = dplyr::bind_rows(
    cceb_2003_data$cceb_editions,
    cceb_2015_data$cceb_editions,
    cceb_2026_data$cceb_editions
  ),
  cceb_points = dplyr::bind_rows(
    cceb_2003_data$cceb_points,
    cceb_2015_data$cceb_points,
    cceb_2026_data$cceb_points
  ),
  cceb_cutoffs = dplyr::bind_rows(
    cceb_2003_data$cceb_cutoffs,
    cceb_2015_data$cceb_cutoffs,
    cceb_2026_data$cceb_cutoffs
  ),
  cceb_income = dplyr::bind_rows(
    cceb_2003_data$cceb_income,
    cceb_2015_data$cceb_income,
    cceb_2026_data$cceb_income
  ),
  cceb_distribution = dplyr::bind_rows(
    cceb_2003_data$cceb_distribution,
    cceb_2015_data$cceb_distribution,
    cceb_2026_data$cceb_distribution
  )
)
validate_cceb_data(cceb_validated_data)
verify_cceb_fidelity(cceb_validated_data)
