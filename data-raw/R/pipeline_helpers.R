# Source catalog ---------------------------------------------------------

.cceb_source_page <- "https://abep.org/criterio-brasil/"

.cceb_2026_urls <- c(
  pt = "https://abep.org/wp-content/uploads/2026/03/CCEB_2026.pdf",
  en = "https://abep.org/wp-content/uploads/2026/03/CCEB-2026-Eng.pdf"
)

.cceb_legacy_urls <- c(
  `2003` = "https://abep.org/wp-content/uploads/2024/02/08_cceb_2003_em_vigor_em_2003_base_lse_2000.pdf",
  `2008` = "https://abep.org/wp-content/uploads/2024/02/07_cceb_2008_em_vigor_em_2008_base_lse_2005.pdf",
  `2009` = "https://abep.org/wp-content/uploads/2024/02/06_cceb_2009_base_2006_2007_cceb_em_vigor_em_2009_base_lse_2006_2007.pdf",
  `2010` = "https://abep.org/wp-content/uploads/2024/02/05_cceb_2008_em_vigor_em_2010_base_lse_2008.pdf",
  `2011` = "https://abep.org/wp-content/uploads/2024/02/04_cceb_base_lse_2009.pdf",
  `2012` = "https://abep.org/wp-content/uploads/2024/02/03_cceb_2012_base_lse_2010.pdf",
  `2013` = "https://abep.org/wp-content/uploads/2024/02/02_cceb_2013.pdf",
  `2014` = "https://abep.org/wp-content/uploads/2024/02/09_cceb_2014.pdf"
)

.cceb_edition_ids <- c(
  2003L,
  2008L,
  2009L,
  2010L,
  2011L,
  2012L,
  2013L,
  2014L,
  2015L,
  2016L,
  2018L,
  2019L,
  2020L,
  2021L,
  2022L,
  2024L,
  2026L
)

# Downloads --------------------------------------------------------------

.request_with_retry <- function(url, max_tries = 3, timeout = 60) {
  response <- httr2::request(url) |>
    httr2::req_user_agent(
      "criteriobrasil data pipeline (https://github.com/viniciusreginatto/criteriobrasil)"
    ) |>
    httr2::req_options(timeout = timeout) |>
    httr2::req_retry(
      max_tries = max_tries,
      retry_on_failure = TRUE,
      backoff = \(i) min(2^(i - 1), 30)
    ) |>
    httr2::req_perform()

  return(response)
}

download_with_retry <- function(
  url,
  dest,
  max_tries = 3,
  timeout = 60,
  overwrite = FALSE
) {
  if (file.exists(dest) && !overwrite) {
    return(invisible(dest))
  }

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  response <- .request_with_retry(
    url = url,
    max_tries = max_tries,
    timeout = timeout
  )
  body <- httr2::resp_body_raw(response)

  if (length(body) < 4L || rawToChar(body[seq_len(4L)]) != "%PDF") {
    cli::cli_abort("The response from {.url {url}} is not a PDF.")
  }

  temporary <- tempfile(pattern = ".download-", tmpdir = dirname(dest))
  on.exit(unlink(temporary), add = TRUE)
  writeBin(body, temporary)

  if (file.exists(dest)) {
    unlink(dest)
  }
  copied <- file.copy(temporary, dest, overwrite = TRUE)
  if (!copied) {
    cli::cli_abort("Could not write the downloaded PDF to {.path {dest}}.")
  }

  return(invisible(dest))
}

get_cceb_links <- function(
  edition_id,
  page_url = .cceb_source_page,
  html = NULL,
  max_tries = 3,
  timeout = 60
) {
  edition_id <- as.character(edition_id)

  if (is.null(html)) {
    response <- .request_with_retry(
      url = page_url,
      max_tries = max_tries,
      timeout = timeout
    )
    html <- httr2::resp_body_string(response)
  }

  anchors <- rvest::html_elements(xml2::read_html(html), "a")
  anchors <- tibble::tibble(
    label = rvest::html_text2(anchors),
    href = rvest::html_attr(anchors, "href")
  )
  candidates <- dplyr::filter(
    anchors,
    !is.na(href),
    stringr::str_detect(
      href,
      stringr::regex("\\.pdf(?:$|[?#])", ignore_case = TRUE)
    ),
    stringr::str_detect(
      stringr::str_c(label, href, sep = " "),
      stringr::fixed(edition_id)
    )
  )

  if (nrow(candidates) == 0L) {
    cli::cli_abort(
      "No PDF link for CCEB edition {edition_id} was found on {.url {page_url}}."
    )
  }

  # The Portuguese patterns are tested last so that they win over a label that
  # matches both languages.
  links <- candidates |>
    dplyr::mutate(
      edition_id = as.integer(edition_id),
      url = xml2::url_absolute(href, page_url),
      file_name = basename(stringr::str_remove(url, "[?#].*$")),
      lang = dplyr::case_when(
        stringr::str_detect(
          stringr::str_c(label, url, sep = " "),
          stringr::regex(
            "portugu|brasil|cceb_[0-9]+\\.pdf$",
            ignore_case = TRUE
          )
        ) ~ "pt",
        stringr::str_detect(
          stringr::str_c(label, url, sep = " "),
          stringr::regex("ingl|english|[-_]eng(?:lish)?", ignore_case = TRUE)
        ) ~ "en",
        .default = "pt"
      )
    ) |>
    dplyr::distinct(url, .keep_all = TRUE) |>
    dplyr::arrange(match(lang, c("pt", "en"))) |>
    dplyr::select(edition_id, label, url, file_name, lang)

  return(links)
}

# Source manifest --------------------------------------------------------

sha256_file <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("File {.path {path}} does not exist.")
  }

  hash <- digest::digest(path, algo = "sha256", file = TRUE)

  return(hash)
}

.duplicated_manifest_keys <- function(manifest, key) {
  duplicates <- manifest |>
    dplyr::count(dplyr::across(dplyr::all_of(key))) |>
    dplyr::filter(n > 1L) |>
    dplyr::select(dplyr::all_of(key))

  return(do.call(paste, c(duplicates, sep = "/")))
}

read_cceb_manifest <- function(
  manifest_path = "data-raw/manifest.csv"
) {
  if (!file.exists(manifest_path)) {
    cli::cli_abort(
      "The source manifest {.path {manifest_path}} does not exist."
    )
  }

  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
  required <- c(
    "edition_id",
    "lang",
    "file_name",
    "url",
    "sha256",
    "accessed_at"
  )
  missing <- setdiff(required, names(manifest))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "The source manifest is missing columns: {paste(missing, collapse = ', ')}."
    )
  }

  manifest <- manifest |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::mutate(
      edition_id = as.integer(edition_id),
      dplyr::across(!edition_id, as.character)
    )

  identity <- c("edition_id", "lang", "file_name", "url", "sha256")
  if (anyNA(dplyr::select(manifest, dplyr::all_of(identity)))) {
    cli::cli_abort("The source manifest contains missing identity fields.")
  }

  duplicated_editions <- .duplicated_manifest_keys(
    manifest,
    c("edition_id", "lang")
  )
  if (length(duplicated_editions) > 0L) {
    cli::cli_abort(
      "The source manifest has duplicate edition/language keys: {paste(duplicated_editions, collapse = ', ')}."
    )
  }
  duplicated_files <- .duplicated_manifest_keys(manifest, "file_name")
  if (length(duplicated_files) > 0L) {
    cli::cli_abort(
      "The source manifest has duplicate file names: {paste(duplicated_files, collapse = ', ')}."
    )
  }
  if (!all(stringr::str_detect(manifest$sha256, "^[0-9a-f]{64}$"))) {
    cli::cli_abort("The source manifest contains an invalid SHA-256 hash.")
  }

  return(manifest)
}

verify_source_files <- function(
  manifest_path = "data-raw/manifest.csv",
  pdf_dir = "data-raw/pdf",
  edition_ids = NULL,
  lang = "pt"
) {
  requested_langs <- lang
  manifest <- read_cceb_manifest(manifest_path)
  manifest <- dplyr::filter(manifest, lang %in% requested_langs)
  if (!is.null(edition_ids)) {
    requested_ids <- as.integer(edition_ids)
    manifest <- dplyr::filter(manifest, edition_id %in% requested_ids)
  }
  if (nrow(manifest) == 0L) {
    cli::cli_abort(
      "No source files match the requested editions and languages."
    )
  }

  sources <- dplyr::mutate(manifest, path = file.path(pdf_dir, file_name))
  missing <- dplyr::filter(sources, !file.exists(path))
  if (nrow(missing) > 0L) {
    cli::cli_abort("Missing source PDF{?s}: {.path {missing$path}}.")
  }

  changed <- sources |>
    dplyr::mutate(actual_sha256 = purrr::map_chr(path, sha256_file)) |>
    dplyr::filter(actual_sha256 != sha256)
  if (nrow(changed) > 0L) {
    details <- stringr::str_glue(
      "{changed$edition_id}/{changed$lang} ({changed$file_name})"
    )
    cli::cli_abort(
      c(
        "Source PDF hash mismatch: {paste(details, collapse = ', ')}.",
        "i" = "Review the changed source and use the explicit refresh workflow if it is intentional."
      )
    )
  }

  return(invisible(manifest))
}

update_manifest <- function(
  link,
  file_path,
  manifest_path = "data-raw/manifest.csv",
  replace = FALSE
) {
  required <- c("edition_id", "lang", "file_name", "url")
  missing <- setdiff(required, names(link))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "The link record is missing: {paste(missing, collapse = ', ')}."
    )
  }

  if (file.exists(manifest_path)) {
    manifest <- read_cceb_manifest(manifest_path)
  } else {
    manifest <- tibble::tibble(
      edition_id = integer(),
      lang = character(),
      file_name = character(),
      url = character(),
      sha256 = character(),
      accessed_at = character()
    )
  }

  new_row <- tibble::tibble(
    edition_id = as.integer(link$edition_id[[1]]),
    lang = link$lang[[1]],
    file_name = basename(file_path),
    url = link$url[[1]],
    sha256 = sha256_file(file_path),
    accessed_at = as.character(Sys.Date())
  )

  existing <- dplyr::filter(
    manifest,
    edition_id == new_row$edition_id,
    lang == new_row$lang
  )
  if (nrow(existing) == 1L && !replace) {
    identical_source <- identical(
      existing[, c("file_name", "url", "sha256")],
      new_row[, c("file_name", "url", "sha256")]
    )
    if (!identical_source) {
      cli::cli_abort(
        c(
          "The manifest entry for edition {new_row$edition_id}/{new_row$lang} changed.",
          "i" = "Use the explicit source-refresh workflow after reviewing the new PDF."
        )
      )
    }

    return(manifest)
  }

  manifest <- manifest |>
    dplyr::filter(
      edition_id != new_row$edition_id | lang != new_row$lang
    ) |>
    dplyr::bind_rows(new_row) |>
    dplyr::arrange(edition_id, lang)
  readr::write_csv(manifest, manifest_path)

  return(manifest)
}

cceb_edition_links <- function(edition) {
  key <- as.character(edition)
  if (key %in% names(.cceb_legacy_urls)) {
    url <- unname(.cceb_legacy_urls[[key]])
    links <- tibble::tibble(
      edition_id = as.integer(edition),
      label = NA_character_,
      url = url,
      file_name = basename(url),
      lang = "pt"
    )
  } else {
    links <- dplyr::filter(get_cceb_links(edition_id = edition), lang == "pt")
  }
  if (nrow(links) != 1L) {
    cli::cli_abort(
      "Expected one Portuguese PDF for edition {edition}; found {nrow(links)}."
    )
  }

  return(links)
}

download_cceb_edition <- function(
  edition,
  pdf_dir = file.path("data-raw", "pdf"),
  overwrite = FALSE,
  replace = FALSE
) {
  links <- cceb_edition_links(edition)
  destination <- file.path(pdf_dir, links$file_name[[1]])
  download_with_retry(links$url[[1]], destination, overwrite = overwrite)
  update_manifest(links, destination, replace = replace)

  return(invisible(destination))
}

edition_source_link <- function(manifest, edition) {
  link <- dplyr::filter(manifest, edition_id == edition)
  if (nrow(link) != 1L) {
    cli::cli_abort(
      "Expected one manifest row for edition {edition}; found {nrow(link)}."
    )
  }

  return(link)
}

edition_source_pdf <- function(manifest, edition, pdf_dir = "data-raw/pdf") {
  link <- edition_source_link(manifest, edition)
  pdf_path <- file.path(pdf_dir, link$file_name[[1]])
  if (!file.exists(pdf_path)) {
    cli::cli_abort("The downloaded PDF {.path {pdf_path}} is missing.")
  }

  return(pdf_path)
}

# PDF text ---------------------------------------------------------------

read_cceb_pdf <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("PDF {.path {path}} does not exist.")
  }

  document <- list(
    path = path,
    text = pdftools::pdf_text(path),
    data = pdftools::pdf_data(path)
  )
  class(document) <- c("cceb_pdf", "list")

  return(document)
}

as_cceb_pdf <- function(pdf) {
  if (inherits(pdf, "cceb_pdf")) {
    return(pdf)
  }

  if (is.character(pdf) && length(pdf) == 1L) {
    return(read_cceb_pdf(pdf))
  }

  if (is.list(pdf) && is.character(pdf$text)) {
    class(pdf) <- unique(c("cceb_pdf", class(pdf)))
    return(pdf)
  }

  cli::cli_abort("`pdf` must be a PDF path or a parsed {.cls cceb_pdf} object.")
}

pdf_pages_matching <- function(pdf, pattern) {
  pdf <- as_cceb_pdf(pdf)
  pages <- which(stringr::str_detect(
    pdf$text,
    stringr::regex(pattern, ignore_case = TRUE)
  ))

  return(pages)
}

pdf_page_containing <- function(pdf, pattern) {
  pdf <- as_cceb_pdf(pdf)
  pages <- pdf_pages_matching(pdf, pattern)

  if (length(pages) != 1L) {
    cli::cli_abort(
      "Expected one PDF page matching {.val {pattern}}, found {length(pages)}."
    )
  }

  return(pdf$text[[pages]])
}

# Turns a `str_match()` result into a tibble with named capture columns.
str_match_tibble <- function(string, pattern, names) {
  matches <- stringr::str_match(string, pattern)
  colnames(matches) <- names
  result <- tibble::as_tibble(matches)

  return(result)
}

pdf_page_lines <- function(page) {
  lines <- stringr::str_squish(stringr::str_split_1(page, "\n"))

  return(lines)
}

# Row labels -------------------------------------------------------------

# Source labels are literal text, so they are escaped before they reach a
# regular expression.
.alias_pattern <- function(aliases) {
  pattern <- stringr::str_c(
    "^(",
    stringr::str_c(stringr::str_escape(aliases), collapse = "|"),
    ")"
  )

  return(pattern)
}

alias_line_indexes <- function(lines, aliases) {
  indexes <- which(stringr::str_detect(lines, .alias_pattern(aliases)))

  return(indexes)
}

alias_label <- function(line, aliases) {
  matched <- stringr::str_starts(line, stringr::str_escape(aliases))

  return(aliases[[which(matched)[[1]]]])
}

# Numbers ----------------------------------------------------------------

parse_brazilian_number <- function(x) {
  x <- stringr::str_remove_all(x, "[^0-9,.-]")
  x <- stringr::str_remove_all(x, "\\.")
  x <- stringr::str_replace(x, ",", ".")
  value <- as.numeric(x)

  if (anyNA(value)) {
    cli::cli_abort("Could not parse Brazilian number {x}.")
  }

  return(value)
}

# Values written with a decimal comma follow the Brazilian convention, while
# the remaining ones are plain integers or already use a decimal point.
parse_cceb_percentages <- function(values) {
  values <- stringr::str_remove(values, "%")
  brazilian <- stringr::str_detect(values, ",")
  result <- rep(NA_real_, length(values))
  result[!brazilian] <- as.numeric(values[!brazilian])
  if (any(brazilian)) {
    result[brazilian] <- parse_brazilian_number(values[brazilian])
  }

  if (anyNA(result)) {
    cli::cli_abort("Could not parse CCEB percentage values.")
  }

  return(result / 100)
}
