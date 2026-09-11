canonical_table_hash <- function(dat) {
  payload <- readr::format_csv(dat, na = "<NA>")

  return(digest::digest(payload, algo = "sha256", serialize = FALSE))
}

.read_cceb_fingerprints <- function(
  fingerprint_path,
  call = rlang::caller_env()
) {
  if (!file.exists(fingerprint_path)) {
    cli::cli_abort(
      "The fidelity fingerprint file {.path {fingerprint_path}} is missing.",
      call = call
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

  return(fingerprints)
}

# Every edition must be fingerprinted for every package table.
.check_fingerprint_coverage <- function(
  fingerprints,
  data,
  call = rlang::caller_env()
) {
  expected <- tidyr::expand_grid(
    edition_id = data$cceb_editions$edition_id,
    table_name = .cceb_required_tables
  )
  observed <- dplyr::select(fingerprints, edition_id, table_name)
  uncovered <- dplyr::anti_join(
    expected,
    observed,
    by = c("edition_id", "table_name")
  )

  if (nrow(uncovered) > 0L || nrow(expected) != nrow(observed)) {
    cli::cli_abort(
      "Fidelity fingerprints must cover every edition and package table.",
      call = call
    )
  }

  return(invisible(fingerprints))
}

verify_cceb_fidelity <- function(
  data,
  fingerprint_path = "data-raw/fidelity.csv",
  source_manifest_path = "data-raw/manifest.csv"
) {
  fingerprints <- .read_cceb_fingerprints(fingerprint_path)
  source_manifest <- read_cceb_manifest(source_manifest_path)
  source_manifest <- dplyr::filter(source_manifest, lang == "pt")
  .check_fingerprint_coverage(fingerprints, data)

  unknown <- setdiff(fingerprints$table_name, .cceb_required_tables)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      "Unknown fidelity table{?s}: {paste(unknown, collapse = ', ')}."
    )
  }

  sources <- dplyr::select(source_manifest, edition_id, current_sha256 = sha256)
  checked <- dplyr::rename(fingerprints, expected_nrow = nrow)
  checked <- dplyr::left_join(checked, sources, by = "edition_id")
  stale <- dplyr::filter(
    checked,
    is.na(current_sha256) | current_sha256 != source_sha256
  )
  if (nrow(stale) > 0L) {
    cli::cli_abort(
      "The fidelity fingerprint for edition {stale$edition_id[[1]]} is not tied to the current source hash."
    )
  }

  observed <- dplyr::mutate(
    checked,
    edition_table = purrr::map2(
      table_name,
      edition_id,
      \(name, edition) dplyr::filter(data[[name]], edition_id == edition)
    ),
    observed_nrow = purrr::map_int(edition_table, nrow),
    observed_sha256 = purrr::map_chr(edition_table, canonical_table_hash)
  )
  changed <- dplyr::filter(
    observed,
    observed_nrow != expected_nrow | observed_sha256 != sha256
  )
  if (nrow(changed) > 0L) {
    cli::cli_abort(
      "Source fidelity failed for edition {changed$edition_id[[1]]}, table {.field {changed$table_name[[1]]}}."
    )
  }

  return(invisible(data))
}
