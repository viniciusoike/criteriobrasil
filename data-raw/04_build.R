# Build the release assets and the internal table registry.
#
# Every table is published as a release asset; `sysdata.rda` keeps only the
# registry that `cceb_get()` uses to find and check those assets.

source("data-raw/03_validate.R")

cceb_data <- cceb_validated_data

table_descriptions <- tibble::tribble(
  ~table         , ~description                                                ,
  "editions"     , "Edition dates, sources, regimes, and methodological notes" ,
  "points"       , "Points assigned to each questionnaire response"            ,
  "cutoffs"      , "Point ranges used to assign economic classes"              ,
  "income"       , "Published mean income estimates by economic class"         ,
  "distribution" , "Published class shares by geography"
)

.cceb_table_registry <- table_descriptions |>
  dplyr::mutate(
    object = paste0("cceb_", table),
    asset = paste0(object, ".rds"),
    editions = purrr::map(object, \(name) {
      sort(unique(cceb_data[[name]]$edition_id))
    }),
    columns = purrr::map(object, \(name) names(cceb_data[[name]]))
  ) |>
  dplyr::select(table, description, asset, editions, columns)

release_directory <- file.path("data-raw", "release")
dir.create(release_directory, recursive = TRUE, showWarnings = FALSE)

purrr::walk(paste0("cceb_", table_descriptions$table), function(name) {
  saveRDS(
    cceb_data[[name]],
    file.path(release_directory, paste0(name, ".rds")),
    version = 2L,
    compress = "xz"
  )
})

usethis::use_data(
  .cceb_table_registry,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz"
)
