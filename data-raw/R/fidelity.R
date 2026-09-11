canonical_table_hash <- function(dat) {
  payload <- readr::format_csv(dat, na = "<NA>")

  return(digest::digest(payload, algo = "sha256", serialize = FALSE))
}

verify_cceb_fidelity <- function(
  data,
  fingerprint_path = "data-raw/fidelity.csv",
  source_manifest_path = "data-raw/manifest.csv"
) {
  if (!file.exists(fingerprint_path)) {
    cli::cli_abort(
      "The fidelity fingerprint file {.path {fingerprint_path}} is missing."
    )
  }
  fingerprints <- readr::read_csv(fingerprint_path, show_col_types = FALSE)
  required <- c("edition_id", "table_name", "nrow", "sha256", "source_sha256")
  .require_columns(fingerprints, required, "fidelity fingerprints")
  .check_unique_key(
    fingerprints,
    c("edition_id", "table_name"),
    "fidelity fingerprints"
  )

  source_manifest <- read_cceb_manifest(source_manifest_path)
  source_manifest <- source_manifest[
    source_manifest$lang == "pt",
    ,
    drop = FALSE
  ]
  table_names <- .cceb_required_tables
  expected_grid <- expand.grid(
    edition_id = data$cceb_editions$edition_id,
    table_name = table_names,
    stringsAsFactors = FALSE
  )
  observed_grid <- fingerprints[, c("edition_id", "table_name")]
  if (
    nrow(expected_grid) != nrow(observed_grid) ||
      !all(
        .group_key(expected_grid, names(expected_grid)) %in%
          .group_key(observed_grid, names(observed_grid))
      )
  ) {
    cli::cli_abort(
      "Fidelity fingerprints must cover every edition and package table."
    )
  }

  for (index in seq_len(nrow(fingerprints))) {
    fingerprint <- fingerprints[index, , drop = FALSE]
    edition_id <- fingerprint$edition_id[[1L]]
    table_name <- fingerprint$table_name[[1L]]
    if (!table_name %in% table_names) {
      cli::cli_abort(
        "Unknown fidelity table {.val {table_name}} for edition {edition_id}."
      )
    }

    source_row <- source_manifest[
      source_manifest$edition_id == edition_id,
      ,
      drop = FALSE
    ]
    if (
      nrow(source_row) != 1L ||
        !identical(source_row$sha256[[1L]], fingerprint$source_sha256[[1L]])
    ) {
      cli::cli_abort(
        "The fidelity fingerprint for edition {edition_id} is not tied to the current source hash."
      )
    }

    dat <- data[[table_name]]
    dat <- dat[dat$edition_id == edition_id, , drop = FALSE]
    if (
      nrow(dat) != fingerprint$nrow[[1L]] ||
        !identical(canonical_table_hash(dat), fingerprint$sha256[[1L]])
    ) {
      cli::cli_abort(
        "Source fidelity failed for edition {edition_id}, table {.field {table_name}}."
      )
    }
  }

  return(invisible(data))
}
