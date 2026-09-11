# Download the source PDFs listed by the ABEP CCEB page.
#
# PDFs are deliberately kept outside version control in `data-raw/pdf/`.
# Record one row per downloaded file in `data-raw/manifest.csv`, including
# the URL, SHA-256 hash, and access date.

source("data-raw/R/pipeline_helpers.R")

purrr::walk(.cceb_edition_ids, download_cceb_edition)
