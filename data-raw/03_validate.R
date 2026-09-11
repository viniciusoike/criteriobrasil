# Validate the extracted tables before they are added to the release assets.
#
# Each edition is validated on its own first, so that a failure names the
# source document that caused it, and the stacked tables are then checked as
# a whole against the recorded source fingerprints.

source("data-raw/02_extract_2003.R")
source("data-raw/02_extract_2015.R")
source("data-raw/02_extract_2026.R")
source("data-raw/R/validate.R")
source("data-raw/R/fidelity.R")
source("data-raw/R/validate_2003.R")
source("data-raw/R/validate_2015.R")
source("data-raw/R/validate_2026.R")

purrr::walk(.cceb_2003_ids, function(edition) {
  edition_data <- filter_cceb_edition(cceb_2003_data, edition)
  validate_2003_data(edition_data, edition_id = edition)
})

purrr::walk(cceb_2015_ids, function(edition) {
  edition_data <- filter_cceb_edition(cceb_2015_data, edition)
  validate_2015_data(edition_data, edition_id = edition)
})

validate_2026_data(cceb_2026_data)

cceb_validated_data <- bind_cceb_tables(
  list(cceb_2003_data, cceb_2015_data, cceb_2026_data)
)
validate_cceb_data(cceb_validated_data)
verify_cceb_fidelity(cceb_validated_data)
