# Extract the 2026 CCEB regime from the downloaded PDF.

source("data-raw/R/pipeline_helpers.R")
source("data-raw/R/tidy_helpers.R")
source("data-raw/R/extract_2026.R")
source("data-raw/R/tidy_2026.R")

cceb_2026_links <- verify_source_files(edition_ids = 2026L, lang = "pt")
cceb_2026_pdf <- read_cceb_pdf(
  edition_source_pdf(cceb_2026_links, 2026L)
)
cceb_2026_raw <- extract_2026_raw(cceb_2026_pdf)
cceb_2026_data <- tidy_2026(cceb_2026_raw, links = cceb_2026_links)
