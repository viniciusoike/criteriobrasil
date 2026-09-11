# Extract the 2026 CCEB regime from the downloaded PDF.
#
# Keep the reference year at the row level for distributions because the
# country and regional estimates use different source studies.

source("data-raw/R/pipeline_helpers.R")
source("data-raw/R/extract_2026.R")
source("data-raw/R/tidy_2026.R")

cceb_2026_links <- verify_source_files(edition_ids = 2026L, lang = "pt")
cceb_2026_pdf_path <- file.path(
  "data-raw/pdf",
  cceb_2026_links$file_name[[1]]
)
cceb_2026_pdf <- read_cceb_pdf(cceb_2026_pdf_path)
cceb_2026_raw <- extract_2026_raw(cceb_2026_pdf)
cceb_2026_data <- tidy_2026(cceb_2026_raw, links = cceb_2026_links)
