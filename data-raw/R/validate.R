.cceb_required_tables <- c(
  "cceb_editions",
  "cceb_points",
  "cceb_cutoffs",
  "cceb_income",
  "cceb_distribution"
)

# Building blocks --------------------------------------------------------

.require_columns <- function(dat, required, table_name) {
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.field {table_name}} is missing columns: {paste(missing, collapse = ', ')}."
    )
  }

  return(invisible(dat))
}

.check_unique_key <- function(dat, key, table_name) {
  if (nrow(dat) == 0L) {
    return(invisible(dat))
  }

  counts <- dplyr::count(dat, dplyr::across(dplyr::all_of(key)))
  if (any(counts$n > 1L)) {
    cli::cli_abort(
      "{.field {table_name}} has duplicate rows for key {paste(key, collapse = ', ')}."
    )
  }

  return(invisible(dat))
}

.check_integer_values <- function(x, field, table_name, minimum = NULL) {
  invalid <- is.na(x) | !is.finite(x) | x != floor(x)
  if (!is.null(minimum)) {
    invalid <- invalid | x < minimum
  }
  if (any(invalid)) {
    requirement <- "complete integer values"
    if (!is.null(minimum)) {
      requirement <- paste(requirement, "no smaller than", minimum)
    }
    cli::cli_abort(
      "{.field {table_name}${field}} must contain {requirement}."
    )
  }

  return(invisible(x))
}

# Table checks -----------------------------------------------------------

validate_cceb_editions <- function(editions) {
  required <- c(
    "edition_id",
    "effective_date",
    "effective_date_source",
    "regime",
    "income_source",
    "income_ref_year",
    "income_concept",
    "distribution_source",
    "lang",
    "url_pt",
    "url_en",
    "method_note"
  )
  .require_columns(editions, required, "cceb_editions")
  .check_unique_key(editions, "edition_id", "cceb_editions")
  .check_integer_values(editions$edition_id, "edition_id", "cceb_editions")
  if (
    !inherits(editions$effective_date, "Date") || anyNA(editions$effective_date)
  ) {
    cli::cli_abort(
      "{.field cceb_editions$effective_date} must contain complete dates."
    )
  }
  identity_columns <- c("effective_date_source", "regime", "lang")
  if (anyNA(dplyr::select(editions, dplyr::all_of(identity_columns)))) {
    cli::cli_abort("The edition identity metadata must be complete.")
  }

  return(invisible(editions))
}

validate_cceb_points <- function(points) {
  required <- c(
    "edition_id",
    "block",
    "variable",
    "label_pt",
    "label_en",
    "level",
    "level_order",
    "points"
  )
  .require_columns(points, required, "cceb_points")
  .check_unique_key(
    points,
    c("edition_id", "variable", "level"),
    "cceb_points"
  )
  .check_integer_values(points$edition_id, "edition_id", "cceb_points")
  .check_integer_values(points$level_order, "level_order", "cceb_points", 1L)
  .check_integer_values(points$points, "points", "cceb_points", 0L)
  identity_columns <- c("block", "variable", "label_pt", "label_en", "level")
  if (anyNA(dplyr::select(points, dplyr::all_of(identity_columns)))) {
    cli::cli_abort("The point-rule identity fields must be complete.")
  }

  level_orders <- dplyr::summarise(
    points,
    ordered = identical(sort(level_order), seq_along(level_order)),
    .by = c(edition_id, variable)
  )
  if (!all(level_orders$ordered)) {
    cli::cli_abort("Point-rule levels must be consecutively ordered from one.")
  }

  return(invisible(points))
}

validate_cceb_cutoffs <- function(cutoffs, points) {
  required <- c(
    "edition_id",
    "class",
    "class_order",
    "points_min",
    "points_max"
  )
  .require_columns(cutoffs, required, "cceb_cutoffs")
  .check_unique_key(cutoffs, c("edition_id", "class"), "cceb_cutoffs")
  .check_unique_key(cutoffs, c("edition_id", "class_order"), "cceb_cutoffs")
  .check_integer_values(cutoffs$edition_id, "edition_id", "cceb_cutoffs")
  .check_integer_values(cutoffs$class_order, "class_order", "cceb_cutoffs", 1L)
  .check_integer_values(cutoffs$points_min, "points_min", "cceb_cutoffs", 0L)
  .check_integer_values(cutoffs$points_max, "points_max", "cceb_cutoffs", 0L)
  if (anyNA(cutoffs$class) || any(cutoffs$points_min > cutoffs$points_max)) {
    cli::cli_abort(
      "The class cutoffs contain missing classes or reversed ranges."
    )
  }

  .check_cutoff_ranges(cutoffs)
  .check_cutoff_maximum(cutoffs, points)

  return(invisible(cutoffs))
}

# Consecutive classes must start one point above the previous class.
.check_cutoff_ranges <- function(cutoffs, call = rlang::caller_env()) {
  ranges <- dplyr::arrange(cutoffs, edition_id, points_min)
  ranges <- dplyr::mutate(
    ranges,
    contiguous = points_min == dplyr::lag(points_max) + 1L,
    .by = edition_id
  )

  broken <- dplyr::filter(ranges, !contiguous)
  if (nrow(broken) > 0L) {
    cli::cli_abort(
      "The cutoff ranges for edition {broken$edition_id[[1]]} overlap or contain gaps.",
      call = call
    )
  }

  return(invisible(cutoffs))
}

# The top cutoff must equal the score of a household that scores the maximum
# on every question.
.check_cutoff_maximum <- function(
  cutoffs,
  point_rules,
  call = rlang::caller_env()
) {
  variable_maxima <- dplyr::summarise(
    point_rules,
    variable_max = max(points),
    .by = c(edition_id, variable)
  )
  attainable <- dplyr::summarise(
    variable_maxima,
    attainable_maximum = sum(variable_max),
    .by = edition_id
  )
  published <- dplyr::summarise(
    cutoffs,
    published_maximum = max(points_max),
    .by = edition_id
  )

  compared <- dplyr::left_join(published, attainable, by = "edition_id")
  mismatched <- dplyr::filter(
    compared,
    is.na(attainable_maximum) | published_maximum != attainable_maximum
  )
  if (nrow(mismatched) > 0L) {
    cli::cli_abort(
      "The top cutoff for edition {mismatched$edition_id[[1]]} does not equal its attainable score maximum.",
      call = call
    )
  }

  return(invisible(cutoffs))
}

validate_cceb_income <- function(income) {
  required <- c(
    "edition_id",
    "class",
    "income_mean",
    "income_ref_date",
    "income_ref_year",
    "concept"
  )
  .require_columns(income, required, "cceb_income")
  .check_unique_key(
    income,
    c("edition_id", "class", "income_ref_year"),
    "cceb_income"
  )
  if (nrow(income) == 0L) {
    return(invisible(income))
  }
  .check_integer_values(income$edition_id, "edition_id", "cceb_income")
  .check_integer_values(
    income$income_ref_year,
    "income_ref_year",
    "cceb_income"
  )
  invalid_income <- is.na(income$income_mean) |
    !is.finite(income$income_mean) |
    income$income_mean <= 0
  if (any(invalid_income)) {
    cli::cli_abort("Income means must be complete, finite, and positive.")
  }
  if (anyNA(income$class) || anyNA(income$concept)) {
    cli::cli_abort("Income classes and concepts must be complete.")
  }

  return(invisible(income))
}

.cceb_distribution_group_fields <- c(
  "edition_id",
  "geo_level",
  "geo_code",
  "ref_year",
  "ref_source"
)

validate_cceb_distribution <- function(distribution, tolerance = 0.025) {
  required <- c(
    "edition_id",
    "geo_level",
    "geo_code",
    "geo_name",
    "class",
    "share",
    "ref_year",
    "ref_source"
  )
  .require_columns(distribution, required, "cceb_distribution")
  .check_unique_key(
    distribution,
    c("edition_id", "geo_code", "class", "ref_year", "ref_source"),
    "cceb_distribution"
  )
  if (nrow(distribution) == 0L) {
    return(invisible(distribution))
  }
  .check_integer_values(
    distribution$edition_id,
    "edition_id",
    "cceb_distribution"
  )
  identity_columns <- c(
    "geo_level",
    "geo_code",
    "geo_name",
    "class",
    "ref_source"
  )
  if (anyNA(dplyr::select(distribution, dplyr::all_of(identity_columns)))) {
    cli::cli_abort(
      "The distribution identity and source fields must be complete."
    )
  }
  invalid_share <- is.na(distribution$share) |
    !is.finite(distribution$share) |
    distribution$share < 0 |
    distribution$share > 1
  if (any(invalid_share)) {
    cli::cli_abort("Distribution shares must lie within [0, 1].")
  }

  class_sets <- dplyr::summarise(
    distribution,
    total = sum(share),
    classes = list(sort(class)),
    .by = dplyr::all_of(.cceb_distribution_group_fields)
  )
  if (any(abs(class_sets$total - 1) > tolerance)) {
    cli::cli_abort(
      "Distribution shares must sum to 100% within {tolerance * 100} percentage points."
    )
  }

  complete <- dplyr::summarise(
    class_sets,
    consistent = length(unique(classes)) == 1L,
    .by = edition_id
  )
  if (!all(complete$consistent)) {
    cli::cli_abort(
      "Every geography within an edition must contain the same class set."
    )
  }

  return(invisible(distribution))
}

# Whole-dataset checks ---------------------------------------------------

validate_cceb_relations <- function(data) {
  edition_ids <- data$cceb_editions$edition_id
  child_tables <- setdiff(.cceb_required_tables, "cceb_editions")
  for (table_name in child_tables) {
    orphaned <- setdiff(unique(data[[table_name]]$edition_id), edition_ids)
    if (length(orphaned) > 0L) {
      cli::cli_abort(
        "{.field {table_name}} contains unknown editions: {paste(orphaned, collapse = ', ')}."
      )
    }
  }
  if (
    !setequal(unique(data$cceb_points$edition_id), edition_ids) ||
      !setequal(unique(data$cceb_cutoffs$edition_id), edition_ids)
  ) {
    cli::cli_abort("Every edition must have point rules and class cutoffs.")
  }

  return(invisible(data))
}

validate_cceb_data <- function(data, tolerance = 0.025) {
  missing <- setdiff(.cceb_required_tables, names(data))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "The extracted data is missing: {paste(missing, collapse = ', ')}."
    )
  }

  validate_cceb_editions(data$cceb_editions)
  validate_cceb_points(data$cceb_points)
  validate_cceb_cutoffs(data$cceb_cutoffs, data$cceb_points)
  validate_cceb_income(data$cceb_income)
  validate_cceb_distribution(data$cceb_distribution, tolerance)
  validate_cceb_relations(data)

  return(invisible(data))
}

.validate_single_edition <- function(data, edition_id = NULL) {
  if (is.null(edition_id)) {
    edition_id <- unique(data$cceb_editions$edition_id)
  }
  if (length(edition_id) != 1L || anyNA(edition_id)) {
    cli::cli_abort("Validation requires exactly one edition.")
  }
  table_ids <- unique(purrr::list_c(purrr::map(data, \(dat) dat$edition_id)))
  if (!identical(as.integer(table_ids), as.integer(edition_id))) {
    cli::cli_abort("All non-empty tables must belong to edition {edition_id}.")
  }

  return(as.integer(edition_id))
}
