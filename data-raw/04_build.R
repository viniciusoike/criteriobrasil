# Build the package data objects and release assets.
#
# Use `usethis::use_data()` for package datasets and keep the IPCA series as
# internal data in `sysdata.rda`.

source("data-raw/02_extract_2026.R")
source("data-raw/02_extract_2015.R")
source("data-raw/02_extract_2003.R")
source("data-raw/R/validate.R")
source("data-raw/R/fidelity.R")
source("data-raw/R/validate_2003.R")
source("data-raw/R/validate_2026.R")
source("data-raw/R/validate_2015.R")

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

cceb_editions <- dplyr::bind_rows(
  cceb_2003_data$cceb_editions,
  cceb_2015_data$cceb_editions,
  cceb_2026_data$cceb_editions
)
cceb_points <- dplyr::bind_rows(
  cceb_2003_data$cceb_points,
  cceb_2015_data$cceb_points,
  cceb_2026_data$cceb_points
)
cceb_cutoffs <- dplyr::bind_rows(
  cceb_2003_data$cceb_cutoffs,
  cceb_2015_data$cceb_cutoffs,
  cceb_2026_data$cceb_cutoffs
)
cceb_income <- dplyr::bind_rows(
  cceb_2003_data$cceb_income,
  cceb_2015_data$cceb_income,
  cceb_2026_data$cceb_income
)
cceb_distribution <- dplyr::bind_rows(
  cceb_2003_data$cceb_distribution,
  cceb_2015_data$cceb_distribution,
  cceb_2026_data$cceb_distribution
)

cceb_data <- list(
  cceb_editions = cceb_editions,
  cceb_points = cceb_points,
  cceb_cutoffs = cceb_cutoffs,
  cceb_income = cceb_income,
  cceb_distribution = cceb_distribution
)
validate_cceb_data(cceb_data)
verify_cceb_fidelity(cceb_data)

table_descriptions <- c(
  editions = "Edition dates, sources, regimes, and methodological notes",
  points = "Points assigned to each questionnaire response",
  cutoffs = "Point ranges used to assign economic classes",
  income = "Published mean income estimates by economic class",
  distribution = "Published class shares by geography"
)

.cceb_table_registry <- tibble::tibble(
  table = names(table_descriptions),
  description = unname(table_descriptions),
  asset = paste0("cceb_", names(table_descriptions), ".rds"),
  editions = unname(lapply(
    cceb_data[paste0("cceb_", names(table_descriptions))],
    \(data) sort(unique(data$edition_id))
  )),
  columns = unname(lapply(
    cceb_data[paste0("cceb_", names(table_descriptions))],
    names
  ))
)

release_directory <- file.path("data-raw", "release")
dir.create(release_directory, recursive = TRUE, showWarnings = FALSE)

for (table in names(table_descriptions)) {
  object_name <- paste0("cceb_", table)
  saveRDS(
    cceb_data[[object_name]],
    file.path(release_directory, paste0(object_name, ".rds")),
    version = 2L,
    compress = "xz"
  )
}

usethis::use_data(
  .cceb_table_registry,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz"
)
