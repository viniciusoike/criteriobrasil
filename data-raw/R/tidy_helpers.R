# Tidying steps shared by the 2003, 2015, and 2026 regimes.
#
# Every regime describes its questionnaire rows with the same spec schema:
# one row per point rule, holding the English label and the Portuguese
# aliases used by the source PDFs.

.cceb_point_columns <- c(
  "edition_id",
  "block",
  "variable",
  "label_pt",
  "label_en",
  "level",
  "level_order",
  "points"
)

.cceb_distribution_columns <- c(
  "edition_id",
  "geo_level",
  "geo_code",
  "geo_name",
  "class",
  "share",
  "ref_year",
  "ref_source"
)

point_specs <- function(variable, label_en, aliases) {
  specs <- tibble::tibble(
    variable = variable,
    label_en = label_en,
    aliases = aliases
  )

  return(specs)
}

# Joins every spec row to the raw row whose label matches one of its aliases,
# keeping the spec order.
.match_point_specs <- function(raw_block, specs) {
  matched <- specs |>
    dplyr::mutate(spec_order = dplyr::row_number()) |>
    tidyr::unnest_longer(aliases, values_to = "label_pt") |>
    dplyr::inner_join(
      dplyr::select(raw_block, label_pt, points),
      by = "label_pt"
    )

  matched_once <- matched |>
    dplyr::count(spec_order) |>
    dplyr::filter(n == 1L)
  unmatched <- setdiff(seq_len(nrow(specs)), matched_once$spec_order)
  if (length(unmatched) > 0L) {
    alias <- specs$aliases[[unmatched[[1]]]][[1]]
    cli::cli_abort("Expected one raw point row for {.val {alias}}.")
  }

  return(dplyr::arrange(matched, spec_order))
}

tidy_points_block <- function(
  raw_block,
  specs,
  block,
  levels,
  edition_id
) {
  matched <- .match_point_specs(raw_block, specs)
  wrong_size <- which(purrr::map_int(matched$points, length) != length(levels))
  if (length(wrong_size) > 0L) {
    offender <- dplyr::slice(matched, wrong_size[[1]])
    cli::cli_abort(
      "Expected {length(levels)} levels for {.val {offender$label_pt[[1]]}}; found {length(offender$points[[1]])}."
    )
  }

  result <- matched |>
    dplyr::mutate(
      edition_id = edition_id,
      block = block,
      level = list(levels),
      level_order = list(seq_along(levels))
    ) |>
    tidyr::unnest(c(points, level, level_order)) |>
    dplyr::select(dplyr::all_of(.cceb_point_columns))

  return(result)
}

# Education rules give a single point value per schooling level, so the spec
# rows and the level names line up one to one.
tidy_education_points <- function(raw_block, specs, levels, edition_id) {
  if (nrow(specs) != length(levels)) {
    cli::cli_abort(
      "Expected {length(levels)} education spec{?s}; found {nrow(specs)}."
    )
  }

  matched <- .match_point_specs(raw_block, specs)
  wrong_size <- which(purrr::map_int(matched$points, length) != 1L)
  if (length(wrong_size) > 0L) {
    offender <- matched$label_pt[[wrong_size[[1]]]]
    cli::cli_abort("Expected one point value for {.val {offender}}.")
  }

  result <- matched |>
    dplyr::mutate(
      edition_id = edition_id,
      block = "education",
      level = levels,
      level_order = dplyr::row_number(),
      points = purrr::map_int(points, 1L)
    ) |>
    dplyr::select(dplyr::all_of(.cceb_point_columns))

  return(result)
}

# Published distribution tables hold one column per geography and one row per
# class; `metadata` names the columns in the order they appear.
tidy_distribution_group <- function(raw_group, metadata, edition_id) {
  wrong_size <- which(purrr::map_int(raw_group$share, length) != nrow(metadata))
  if (length(wrong_size) > 0L) {
    offender <- dplyr::slice(raw_group, wrong_size[[1]])
    cli::cli_abort(
      "Expected {nrow(metadata)} shares for class {.val {offender$class[[1]]}}; found {length(offender$share[[1]])}."
    )
  }

  shares <- raw_group |>
    dplyr::select(class, share) |>
    dplyr::mutate(share = purrr::map(share, unname)) |>
    tidyr::unnest_longer(share, indices_to = "geo_order")

  result <- metadata |>
    dplyr::mutate(edition_id = edition_id, geo_order = dplyr::row_number()) |>
    dplyr::inner_join(shares, by = "geo_order") |>
    dplyr::select(dplyr::all_of(.cceb_distribution_columns))

  return(result)
}

empty_cceb_distribution <- function() {
  result <- tibble::tibble(
    edition_id = integer(),
    geo_level = character(),
    geo_code = character(),
    geo_name = character(),
    class = character(),
    share = numeric(),
    ref_year = integer(),
    ref_source = character()
  )

  return(result)
}

empty_cceb_income <- function() {
  result <- tibble::tibble(
    class = character(),
    income_ref_year = integer(),
    income_mean = numeric()
  )

  return(result)
}

# Regime assembly --------------------------------------------------------

filter_cceb_edition <- function(data, edition) {
  result <- purrr::map(data, \(dat) dplyr::filter(dat, edition_id == edition))

  return(result)
}

# Turns a list of per-edition (or per-regime) table lists into one list of
# stacked tables.
bind_cceb_tables <- function(tables) {
  result <- tables |>
    purrr::list_transpose() |>
    purrr::map(dplyr::bind_rows)

  return(result)
}
