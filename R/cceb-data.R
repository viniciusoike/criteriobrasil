# Release configuration -------------------------------------------------------

.cceb_release_repo <- "viniciusoike/criteriobrasil"
.cceb_release_tag <- "data-2026-09-11"
.cceb_asset_cache <- new.env(parent = emptyenv())


# Public API ------------------------------------------------------------------

#' List available CCEB tables
#'
#' Lists the clean tables published by `criteriobrasil`, with a short
#' description and the CCEB editions available in each table. This function
#' uses the registry bundled with the package and does not require an internet
#' connection.
#'
#' @return A tibble with one row per table and columns `table`, `description`,
#'   and `editions`. `editions` is a list-column of integer vectors.
#'
#' @examples
#' cceb_list_tables()
#'
#' @export
cceb_list_tables <- function() {
  return(cceb_registry()[c("table", "description", "editions")])
}

#' Get a CCEB table
#'
#' Downloads a clean CCEB table from the package's GitHub release assets.
#' Repeated requests for the same table in one R session use an in-memory copy.
#'
#' @param table Character scalar. One of `"editions"`, `"points"`,
#'   `"cutoffs"`, `"income"`, `"distribution"`, or `"all"`. The default is
#'   `"distribution"`.
#' @param edition `NULL`, a whole-number CCEB edition identifier, or `"all"`.
#'   `NULL` selects the latest edition available for each requested table.
#'
#' @return A tibble for one table. When `table = "all"`, a named list of five
#'   tibbles in the order returned by [cceb_list_tables()].
#'
#' @details
#' `edition` identifies a published CCEB edition, not necessarily the year of
#' the underlying survey or income estimate. Use `table = "editions"` to
#' inspect effective dates and reference years.
#'
#' The function returns the released data without renaming, reordering, or
#' transforming columns. It only filters rows by `edition_id`.
#'
#' @examplesIf interactive()
#' cceb_get()
#' cceb_get("income", edition = 2024)
#' cceb_get("points", edition = "all")
#'
#' @export
cceb_get <- function(table = "distribution", edition = NULL) {
  table <- cceb_validate_table(table)
  edition <- cceb_validate_edition(edition)
  tables <- if (identical(table, "all")) {
    cceb_registry()$table
  } else {
    table
  }

  cceb_validate_availability(tables, edition)
  result <- lapply(
    tables,
    cceb_get_one,
    edition = edition,
    call = rlang::current_env()
  )

  if (!identical(table, "all")) {
    return(result[[1L]])
  }

  return(stats::setNames(result, tables))
}


# Argument validation ---------------------------------------------------------

cceb_validate_table <- function(table, call = rlang::caller_env()) {
  choices <- c(cceb_registry()$table, "all")
  valid <- is.character(table) &&
    length(table) == 1L &&
    !is.na(table) &&
    table %in% choices

  if (!valid) {
    cli::cli_abort(
      c(
        "{.arg table} must be one available table or {.val all}.",
        "i" = "Available tables: {paste(choices, collapse = ', ')}."
      ),
      call = call
    )
  }

  return(table)
}

cceb_validate_edition <- function(edition, call = rlang::caller_env()) {
  if (is.null(edition) || identical(edition, "all")) {
    return(edition)
  }

  valid <- is.numeric(edition) &&
    length(edition) == 1L &&
    !is.na(edition) &&
    is.finite(edition) &&
    edition >= 0 &&
    edition <= .Machine$integer.max &&
    edition == floor(edition)

  if (!valid) {
    cli::cli_abort(
      "{.arg edition} must be `NULL`, one whole number, or {.val all}.",
      call = call
    )
  }

  return(as.integer(edition))
}

cceb_validate_availability <- function(
  tables,
  edition,
  call = rlang::caller_env()
) {
  if (is.null(edition) || identical(edition, "all")) {
    return(invisible())
  }

  unavailable <- vapply(
    tables,
    cceb_edition_unavailable,
    logical(1),
    edition = edition
  )

  if (any(unavailable)) {
    table <- tables[which(unavailable)[[1L]]]
    available <- cceb_registry_entry(table)$editions[[1L]]
    available <- paste(available, collapse = ", ")
    cli::cli_abort(
      c(
        "Edition {edition} is not available for table {.val {table}}.",
        "i" = "Available editions: {available}."
      ),
      call = call
    )
  }

  return(invisible())
}

cceb_edition_unavailable <- function(table, edition) {
  return(!edition %in% cceb_registry_entry(table)$editions[[1L]])
}


# Asset access ----------------------------------------------------------------

cceb_get_one <- function(table, edition, call = rlang::caller_env()) {
  data <- cceb_fetch_asset(table, call = call)

  if (identical(edition, "all")) {
    return(data)
  }

  available <- cceb_registry_entry(table)$editions[[1L]]
  selected <- edition
  if (is.null(selected)) {
    selected <- max(available)
  }

  return(data[data$edition_id == selected, , drop = FALSE])
}

cceb_fetch_asset <- function(table, call = rlang::caller_env()) {
  if (exists(table, envir = .cceb_asset_cache, inherits = FALSE)) {
    return(get(table, envir = .cceb_asset_cache, inherits = FALSE))
  }

  entry <- cceb_registry_entry(table)
  asset <- entry$asset[[1L]]
  url <- cceb_asset_url(asset)
  destination <- tempfile(fileext = ".rds")
  on.exit(unlink(destination), add = TRUE)

  tryCatch(
    .cceb_download_file(url, destination),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Could not download CCEB table {.val {table}}.",
          "i" = "Release asset: {url}"
        ),
        parent = cnd,
        call = call
      )
    }
  )

  data <- tryCatch(
    readRDS(destination),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Could not read CCEB table {.val {table}}.",
          "i" = "Release asset: {url}"
        ),
        parent = cnd,
        call = call
      )
    }
  )

  data <- cceb_validate_asset(data, table, call = call)
  assign(table, data, envir = .cceb_asset_cache)
  return(data)
}

.cceb_download_file <- function(url, destination) {
  status <- suppressWarnings(
    utils::download.file(
      url = url,
      destfile = destination,
      mode = "wb",
      quiet = TRUE
    )
  )

  if (!identical(status, 0L)) {
    cli::cli_abort("Download failed with status {status}.")
  }

  return(invisible(destination))
}

cceb_asset_url <- function(asset) {
  return(sprintf(
    "https://github.com/%s/releases/download/%s/%s",
    .cceb_release_repo,
    .cceb_release_tag,
    asset
  ))
}

cceb_registry_entry <- function(table) {
  registry <- cceb_registry()
  return(registry[registry$table == table, , drop = FALSE])
}

cceb_registry <- function() {
  return(get(".cceb_table_registry", envir = environment()))
}

cceb_validate_asset <- function(data, table, call = rlang::caller_env()) {
  entry <- cceb_registry_entry(table)
  expected <- entry$columns[[1L]]

  if (!inherits(data, "data.frame")) {
    cli::cli_abort(
      "Release asset for table {.val {table}} must contain a data frame.",
      call = call
    )
  }

  if (!identical(names(data), expected)) {
    cli::cli_abort(
      c(
        "Release asset for table {.val {table}} has an unexpected schema.",
        "i" = "Expected columns: {paste(expected, collapse = ', ')}.",
        "i" = "Found columns: {paste(names(data), collapse = ', ')}."
      ),
      call = call
    )
  }

  if (!is.integer(data$edition_id)) {
    cli::cli_abort(
      "Column {.field edition_id} in table {.val {table}} must be integer.",
      call = call
    )
  }

  editions <- sort(unique(data$edition_id))
  expected_editions <- entry$editions[[1L]]
  if (!identical(editions, expected_editions)) {
    cli::cli_abort(
      c(
        "Release asset for table {.val {table}} has unexpected editions.",
        "i" = "Expected editions: {paste(expected_editions, collapse = ', ')}.",
        "i" = "Found editions: {paste(editions, collapse = ', ')}."
      ),
      call = call
    )
  }

  return(tibble::as_tibble(data))
}
