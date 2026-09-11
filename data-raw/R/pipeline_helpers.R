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

.request_with_retry <- function(url, max_tries = 3, timeout = 60) {
  request <- httr2::request(url)
  request <- httr2::req_user_agent(
    request,
    "criteriobrasil data pipeline (https://github.com/viniciusreginatto/criteriobrasil)"
  )
  request <- httr2::req_options(request, timeout = timeout)
  request <- httr2::req_retry(
    request,
    max_tries = max_tries,
    retry_on_failure = TRUE,
    backoff = \(i) min(2^(i - 1), 30)
  )

  response <- httr2::req_perform(request)

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

  document <- xml2::read_html(html)
  anchors <- rvest::html_elements(document, "a")
  labels <- rvest::html_text2(anchors)
  hrefs <- rvest::html_attr(anchors, "href")
  absolute_hrefs <- xml2::url_absolute(hrefs, page_url)
  searchable <- paste(labels, hrefs)

  keep <- !is.na(hrefs) &
    stringr::str_detect(
      hrefs,
      stringr::regex("\\.pdf(?:$|[?#])", ignore_case = TRUE)
    ) &
    stringr::str_detect(searchable, stringr::fixed(edition_id))

  if (!any(keep)) {
    cli::cli_abort(
      "No PDF link for CCEB edition {edition_id} was found on {.url {page_url}}."
    )
  }

  links <- tibble::tibble(
    edition_id = as.integer(edition_id),
    label = labels[keep],
    url = absolute_hrefs[keep]
  )
  links$file_name <- basename(sub("[?#].*$", "", links$url))
  searchable <- stringr::str_c(links$label, links$url, sep = " ")
  links$lang <- NA_character_
  links$lang[stringr::str_detect(
    searchable,
    stringr::regex("ingl|english|[-_]eng(?:lish)?", ignore_case = TRUE)
  )] <- "en"
  links$lang[stringr::str_detect(
    searchable,
    stringr::regex("portugu|brasil|cceb_[0-9]+\\.pdf$", ignore_case = TRUE)
  )] <- "pt"

  unknown <- is.na(links$lang)
  if (any(unknown)) {
    links$lang[unknown] <- "pt"
  }

  links <- links[!duplicated(links$url), , drop = FALSE]
  links <- links[order(match(links$lang, c("pt", "en"))), , drop = FALSE]
  rownames(links) <- NULL

  return(links)
}

sha256_file <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("File {.path {path}} does not exist.")
  }

  hash <- digest::digest(path, algo = "sha256", file = TRUE)

  return(hash)
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

  manifest <- manifest[, required]
  manifest$edition_id <- as.integer(manifest$edition_id)
  manifest$lang <- as.character(manifest$lang)
  manifest$file_name <- as.character(manifest$file_name)
  manifest$url <- as.character(manifest$url)
  manifest$sha256 <- as.character(manifest$sha256)
  manifest$accessed_at <- as.character(manifest$accessed_at)

  if (
    anyNA(manifest$edition_id) ||
      anyNA(manifest$lang) ||
      anyNA(manifest$file_name) ||
      anyNA(manifest$url) ||
      anyNA(manifest$sha256)
  ) {
    cli::cli_abort("The source manifest contains missing identity fields.")
  }

  key <- paste(manifest$edition_id, manifest$lang, sep = "/")
  if (anyDuplicated(key)) {
    duplicates <- unique(key[
      duplicated(key) | duplicated(key, fromLast = TRUE)
    ])
    cli::cli_abort(
      "The source manifest has duplicate edition/language keys: {paste(duplicates, collapse = ', ')}."
    )
  }
  if (anyDuplicated(manifest$file_name)) {
    duplicates <- unique(manifest$file_name[
      duplicated(manifest$file_name) |
        duplicated(manifest$file_name, fromLast = TRUE)
    ])
    cli::cli_abort(
      "The source manifest has duplicate file names: {paste(duplicates, collapse = ', ')}."
    )
  }
  valid_hash <- stringr::str_detect(manifest$sha256, "^[0-9a-f]{64}$")
  if (any(!valid_hash)) {
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
  manifest <- read_cceb_manifest(manifest_path)
  manifest <- manifest[manifest$lang %in% lang, , drop = FALSE]
  if (!is.null(edition_ids)) {
    manifest <- manifest[
      manifest$edition_id %in% as.integer(edition_ids),
      ,
      drop = FALSE
    ]
  }
  if (nrow(manifest) == 0L) {
    cli::cli_abort(
      "No source files match the requested editions and languages."
    )
  }

  paths <- file.path(pdf_dir, manifest$file_name)
  missing <- !file.exists(paths)
  if (any(missing)) {
    cli::cli_abort(
      "Source PDF{?s} {?is/are} missing: {paste(paths[missing], collapse = ', ')}."
    )
  }

  actual <- vapply(paths, sha256_file, character(1))
  changed <- actual != manifest$sha256
  if (any(changed)) {
    details <- paste0(
      manifest$edition_id[changed],
      "/",
      manifest$lang[changed],
      " (",
      manifest$file_name[changed],
      ")"
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

  same_file <- manifest$edition_id == new_row$edition_id &
    manifest$lang == new_row$lang
  existing <- manifest[same_file & !is.na(same_file), , drop = FALSE]
  if (nrow(existing) == 1L && !replace) {
    if (
      !identical(existing$file_name[[1]], new_row$file_name[[1]]) ||
        !identical(existing$url[[1]], new_row$url[[1]]) ||
        !identical(existing$sha256[[1]], new_row$sha256[[1]])
    ) {
      cli::cli_abort(
        c(
          "The manifest entry for edition {new_row$edition_id}/{new_row$lang} changed.",
          "i" = "Use the explicit source-refresh workflow after reviewing the new PDF."
        )
      )
    }

    return(manifest)
  }
  manifest <- manifest[!same_file | is.na(same_file), , drop = FALSE]
  manifest <- dplyr::bind_rows(manifest, new_row)
  manifest <- manifest[
    order(manifest$edition_id, manifest$lang),
    ,
    drop = FALSE
  ]
  readr::write_csv(manifest, manifest_path)

  return(manifest)
}

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

pdf_page_containing <- function(pdf, pattern) {
  pdf <- as_cceb_pdf(pdf)
  matches <- purrr::map_lgl(
    pdf$text,
    \(page) {
      stringr::str_detect(
        page,
        stringr::regex(pattern, ignore_case = TRUE)
      )
    }
  )
  pages <- which(matches)

  if (length(pages) != 1L) {
    cli::cli_abort(
      "Expected one PDF page matching {.val {pattern}}, found {length(pages)}."
    )
  }

  return(pdf$text[[pages]])
}

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
