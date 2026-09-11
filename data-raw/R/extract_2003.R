.cceb_2003_ids <- c(2003L, 2008L, 2009L, 2010L, 2011L, 2012L, 2013L, 2014L)

.cceb_legacy_classes <- function(edition_id) {
  if (as.integer(edition_id) == 2003L) {
    return(c("A1", "A2", "B1", "B2", "C", "D", "E"))
  }

  return(c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"))
}

.cceb_legacy_count_specs <- point_specs(
  variable = c(
    "color_televisions",
    "radios",
    "bathrooms",
    "automobiles",
    "domestic_service",
    "vacuum_cleaners",
    "washing_machine",
    "video_players",
    "refrigerators",
    "freezer"
  ),
  label_en = c(
    "Color televisions",
    "Radios",
    "Bathrooms",
    "Automobiles",
    "Domestic workers",
    "Vacuum cleaners",
    "Washing machines",
    "Videocassette or DVD players",
    "Refrigerators",
    "Freezers"
  ),
  aliases = list(
    "Televisão em cores",
    "Rádio",
    "Banheiro",
    "Automóvel",
    "Empregada mensalista",
    "Aspirador de pó",
    "Máquina de lavar",
    "Videocassete e/ou DVD",
    "Geladeira",
    "Freezer"
  )
)

.cceb_legacy_education_specs <- point_specs(
  variable = rep("householder_education", 5L),
  label_en = c(
    "No schooling or incomplete elementary school",
    "Elementary school diploma or incomplete middle school",
    "Middle school diploma or incomplete high school",
    "High school diploma or incomplete higher education",
    "Higher education degree"
  ),
  aliases = list(
    c("Analfabeto / Primário incompleto", "Analfabeto/ Primário incompleto"),
    c(
      "Primário completo / Ginasial incompleto",
      "Primário completo/ Ginasial incompleto"
    ),
    c(
      "Ginasial completo / Colegial incompleto",
      "Ginasial completo/ Colegial incompleto"
    ),
    c(
      "Colegial completo / Superior incompleto",
      "Colegial completo/ Superior incompleto"
    ),
    "Superior completo"
  )
)

# Only the 2003 questionnaire scores vacuum cleaners.
.cceb_legacy_point_specs <- function(edition_id) {
  count <- .cceb_legacy_count_specs
  if (as.integer(edition_id) != 2003L) {
    count <- dplyr::filter(count, variable != "vacuum_cleaners")
  }

  return(list(count = count, education = .cceb_legacy_education_specs))
}

.legacy_class_pattern <- function(classes) {
  ordered <- classes[order(nchar(classes), decreasing = TRUE)]

  return(paste(ordered, collapse = "|"))
}

.legacy_class_from_line <- function(lines, classes) {
  pattern <- paste0(
    "^\\s*(?:Classe\\s+)?(",
    .legacy_class_pattern(classes),
    ")(?:\\s|$)"
  )
  result <- stringr::str_match(
    lines,
    stringr::regex(pattern, ignore_case = TRUE)
  )[, 2]
  result <- stringr::str_to_upper(result)
  result <- stringr::str_remove_all(result, "\\s+")
  result <- stringr::str_replace(result, "^D-E$", "DE")

  return(result)
}

.legacy_class_prefix <- function(classes) {
  prefix <- paste0(
    "^\\s*(?:Classe\\s+)?(?:",
    .legacy_class_pattern(classes),
    ")\\s*"
  )

  return(prefix)
}

# Point rules ------------------------------------------------------------

.line_numbers <- function(line) {
  return(stringr::str_extract_all(line, "[0-9]+")[[1]])
}

# Education point values are sometimes typeset on the line above the label.
.values_above <- function(lines, index, n_values) {
  previous <- index - 1L
  while (previous > 0L && stringr::str_length(lines[[previous]]) == 0L) {
    previous <- previous - 1L
  }
  if (previous == 0L) {
    return(character())
  }

  values <- .line_numbers(lines[[previous]])
  if (length(values) < n_values) {
    return(character())
  }

  return(values)
}

# Wide questionnaire rows wrap, so the remaining values follow on the next
# lines.
.values_continued <- function(lines, index, n_values) {
  candidate <- lines[[index]]
  values <- .line_numbers(candidate)
  next_index <- index + 1L
  while (length(values) < n_values && next_index <= length(lines)) {
    candidate <- paste(candidate, lines[[next_index]])
    values <- .line_numbers(candidate)
    next_index <- next_index + 1L
  }

  return(values)
}

.legacy_numeric_rows <- function(lines, specs, n_values, section) {
  rows <- purrr::map(seq_len(nrow(specs)), function(index) {
    aliases <- specs$aliases[[index]]
    match_indexes <- alias_line_indexes(lines, aliases)
    if (length(match_indexes) != 1L) {
      cli::cli_abort(
        "Expected one row for {.val {specs$variable[[index]]}} in {section}; found {length(match_indexes)}."
      )
    }

    line <- lines[[match_indexes]]
    values <- .line_numbers(line)
    if (length(values) == 0L && section == "education") {
      values <- .values_above(lines, match_indexes, n_values)
    }
    if (length(values) < n_values) {
      values <- .values_continued(lines, match_indexes, n_values)
    }
    if (length(values) < n_values) {
      cli::cli_abort(
        "Expected at least {n_values} values for {.val {specs$variable[[index]]}} in {section}; found {length(values)}."
      )
    }

    tibble::tibble(
      variable = specs$variable[[index]],
      label_pt = alias_label(line, aliases),
      points = list(as.integer(tail(values, n_values)))
    )
  })

  return(purrr::list_rbind(rows))
}

extract_2003_raw_points <- function(pdf, edition_id) {
  page <- pdf_page_containing(pdf, "SISTEMA DE PONTOS")
  lines <- pdf_page_lines(page)
  specs <- .cceb_legacy_point_specs(edition_id)

  result <- list(
    count = .legacy_numeric_rows(lines, specs$count, 5L, "count items"),
    education = .legacy_numeric_rows(lines, specs$education, 1L, "education")
  )

  return(result)
}

# Class cutoffs ----------------------------------------------------------

extract_2003_raw_cutoffs <- function(pdf, edition_id) {
  classes_expected <- .cceb_legacy_classes(edition_id)
  page <- pdf_page_containing(pdf, "CORTES DO CRITÉRIO BRASIL")
  lines <- pdf_page_lines(page)
  candidates <- str_match_tibble(
    lines,
    "([0-9]+)\\s*[-–]\\s*([0-9]+)",
    c("range", "points_min", "points_max")
  )
  candidates <- dplyr::mutate(
    candidates,
    class = .legacy_class_from_line(lines, classes_expected)
  )

  cutoffs <- dplyr::filter(candidates, !is.na(class), !is.na(range))
  if (nrow(cutoffs) != length(classes_expected)) {
    cli::cli_abort(
      "Expected {length(classes_expected)} cutoff rows; found {nrow(cutoffs)}."
    )
  }

  result <- cutoffs |>
    dplyr::mutate(
      class_order = match(class, classes_expected),
      dplyr::across(c(points_min, points_max), as.integer)
    ) |>
    dplyr::arrange(class_order) |>
    dplyr::select(class, class_order, points_min, points_max)

  return(result)
}

# Class distribution -----------------------------------------------------

.legacy_distribution_page <- function(pdf) {
  pages <- pdf_pages_matching(pdf, "DISTRIBUIÇÃO|Distribuição das classes")
  if (length(pages) == 0L) {
    return(NULL)
  }
  if (length(pages) != 1L) {
    cli::cli_abort(
      "Expected one legacy distribution page; found {length(pages)}."
    )
  }

  return(pdf$text[[pages]])
}

.legacy_distribution_values <- function(line, classes) {
  stripped <- stringr::str_remove(
    line,
    stringr::regex(.legacy_class_prefix(classes), ignore_case = TRUE)
  )
  values <- stringr::str_extract_all(stripped, "[0-9]+(?:[,.][0-9]+)?%?")[[1]]

  return(values)
}

# Editions in the `metro_first` layout print the metropolitan columns before
# the line that carries the class total.
.legacy_metro_first_row <- function(lines, line_classes, classes, class_name) {
  indexes <- which(line_classes == class_name)
  candidates <- purrr::map(
    indexes,
    \(index) .legacy_distribution_values(lines[[index]], classes)
  )
  total_index <- indexes[which(purrr::map_int(candidates, length) == 1L)[1]]
  if (is.na(total_index)) {
    cli::cli_abort(
      "Could not locate the total for legacy class {.val {class_name}}."
    )
  }

  before <- seq_len(total_index - 1L)
  metro_indexes <- purrr::keep(
    before,
    \(index) length(.legacy_distribution_values(lines[[index]], classes)) == 9L
  )
  if (length(metro_indexes) == 0L) {
    cli::cli_abort(
      "Could not locate the metropolitan values for {.val {class_name}}."
    )
  }

  values <- c(
    .legacy_distribution_values(lines[[total_index]], classes),
    .legacy_distribution_values(lines[[tail(metro_indexes, 1L)]], classes)
  )

  return(values)
}

.legacy_wide_row <- function(
  lines,
  line_classes,
  classes,
  class_name,
  distribution_type,
  occurrence
) {
  indexes <- which(line_classes == class_name)
  candidates <- purrr::map(
    indexes,
    \(index) .legacy_distribution_values(lines[[index]], classes)
  )
  matches <- purrr::keep(candidates, \(values) length(values) == 10L)
  if (length(matches) < occurrence) {
    cli::cli_abort(
      "Could not locate distribution occurrence {occurrence} for {.val {class_name}}."
    )
  }

  shares <- parse_cceb_percentages(matches[[occurrence]])
  if (distribution_type == "metro_total_last") {
    shares <- c(shares[[10]], shares[seq_len(9L)])
  }

  return(shares)
}

.legacy_distribution_rows <- function(
  lines,
  classes,
  distribution_type,
  occurrence = 1L
) {
  line_classes <- .legacy_class_from_line(lines, classes)
  rows <- purrr::map(classes, function(class_name) {
    if (distribution_type == "metro_first") {
      values <- .legacy_metro_first_row(
        lines,
        line_classes,
        classes,
        class_name
      )
      shares <- parse_cceb_percentages(values)
    } else {
      shares <- .legacy_wide_row(
        lines,
        line_classes,
        classes,
        class_name,
        distribution_type,
        occurrence
      )
    }

    tibble::tibble(class = class_name, share = list(shares))
  })

  return(purrr::list_rbind(rows))
}

# The 2013 tables are embedded as images, so their values are transcribed in
# `data-raw/manual/` and keyed to the source hash.
.legacy_2013_distribution <- function(pdf, spec) {
  manual <- .read_2013_manual_table(pdf, "distribution")
  geo_codes <- c(
    "9_rms",
    "FOR",
    "REC",
    "SSA",
    "BH",
    "RJ",
    "SP",
    "CWB",
    "POA",
    "BSB"
  )

  expected <- tidyr::expand_grid(
    class = spec$distribution_classes,
    geo_code = geo_codes
  )
  ordered <- dplyr::left_join(expected, manual, by = c("class", "geo_code"))
  if (anyNA(ordered$share)) {
    incomplete <- ordered$class[is.na(ordered$share)][[1]]
    cli::cli_abort(
      "The manual 2013 distribution is incomplete for class {.val {incomplete}}."
    )
  }

  table <- dplyr::summarise(ordered, share = list(share), .by = class)
  table <- dplyr::mutate(table, ref_year = spec$distribution_ref_year)

  return(table)
}

extract_2003_raw_distribution <- function(pdf, edition_id) {
  spec <- .cceb_2003_edition_spec(edition_id)
  if (as.integer(edition_id) == 2013L) {
    return(list(
      table = .legacy_2013_distribution(pdf, spec),
      distribution_type = "manual_image"
    ))
  }

  page <- .legacy_distribution_page(pdf)
  if (is.null(page)) {
    return(list(
      table = tibble::tibble(class = character(), share = vector("list", 0L)),
      distribution_type = "none"
    ))
  }

  lines <- pdf_page_lines(page)
  if (as.integer(edition_id) == 2009L) {
    # The 2009 source prints the 2006 table first and the 2007 one below it.
    tables <- purrr::map2(
      c(2006L, 2007L),
      c(1L, 2L),
      function(ref_year, occurrence) {
        rows <- .legacy_distribution_rows(
          lines,
          spec$distribution_classes,
          spec$distribution_type,
          occurrence = occurrence
        )

        dplyr::mutate(rows, ref_year = ref_year)
      }
    )
    table <- purrr::list_rbind(tables)
  } else {
    rows <- .legacy_distribution_rows(
      lines,
      spec$distribution_classes,
      spec$distribution_type
    )
    table <- dplyr::mutate(rows, ref_year = spec$distribution_ref_year)
  }

  return(list(table = table, distribution_type = spec$distribution_type))
}

# Income -----------------------------------------------------------------

.legacy_income_number <- function(value) {
  numeric_value <- as.numeric(stringr::str_remove_all(value, "[^0-9]"))
  if (is.na(numeric_value)) {
    cli::cli_abort("Could not parse legacy income value {.val {value}}.")
  }

  return(numeric_value)
}

.read_2013_manual_table <- function(pdf, name, manual_dir = NULL) {
  if (is.null(manual_dir)) {
    manual_dir <- file.path(dirname(dirname(pdf$path)), "manual")
  }
  manifest_path <- file.path(manual_dir, "manifest.csv")
  if (!file.exists(manifest_path)) {
    cli::cli_abort(
      "The manual table manifest {.path {manifest_path}} is missing."
    )
  }

  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
  row <- dplyr::filter(manifest, edition_id == 2013L, table_name == name)
  if (nrow(row) != 1L) {
    cli::cli_abort(
      "Expected one manual 2013 {.val {name}} table; found {nrow(row)}."
    )
  }
  if (is.null(pdf$path) || !file.exists(pdf$path)) {
    cli::cli_abort("The 2013 manual table requires the source PDF path.")
  }
  if (!identical(sha256_file(pdf$path), row$source_sha256[[1]])) {
    cli::cli_abort(
      "The manual 2013 {.val {name}} table does not match the source PDF hash."
    )
  }

  path <- file.path(manual_dir, row$file_name[[1]])
  if (!file.exists(path)) {
    cli::cli_abort("The manual table {.path {path}} is missing.")
  }

  return(readr::read_csv(path, show_col_types = FALSE))
}

.legacy_income_page <- function(pdf) {
  pages <- pdf_pages_matching(
    pdf,
    "Renda média.*familiar|RENDA FAMILIAR POR CLASSES"
  )
  if (length(pages) == 0L) {
    return(NULL)
  }

  return(pdf$text[[pages[[1]]]])
}

extract_2003_raw_income <- function(pdf, edition_id) {
  spec <- .cceb_2003_edition_spec(edition_id)
  if (as.integer(edition_id) == 2013L) {
    manual <- .read_2013_manual_table(pdf, "income")
    result <- dplyr::mutate(manual, income_ref_year = spec$income_ref_year)

    return(dplyr::select(result, class, income_ref_year, income_mean))
  }

  page <- .legacy_income_page(pdf)
  if (length(spec$income_ref_years) == 0L || is.null(page)) {
    return(empty_cceb_income())
  }

  lines <- pdf_page_lines(page)
  header <- which(stringr::str_detect(
    lines,
    stringr::regex(
      "RENDA FAMILIAR POR CLASSES|Renda média bruta familiar",
      ignore_case = TRUE
    )
  ))
  if (length(header) == 0L) {
    cli::cli_abort("Could not locate the legacy income table header.")
  }

  lines <- lines[header[[1]]:length(lines)]
  classes_expected <- spec$income_classes
  income_lines <- stringr::str_remove(
    lines,
    stringr::regex(
      .legacy_class_prefix(classes_expected),
      ignore_case = TRUE
    )
  )
  expected_n <- length(spec$income_ref_years)
  candidates <- tibble::tibble(
    class = .legacy_class_from_line(lines, classes_expected),
    amounts = stringr::str_extract_all(income_lines, "[0-9]+(?:[,.][0-9]+)*")
  )
  candidates <- dplyr::filter(
    candidates,
    !is.na(class),
    purrr::map_int(amounts, length) >= expected_n
  )

  rows <- purrr::map(classes_expected, function(class_name) {
    matched <- dplyr::filter(candidates, class == class_name)
    if (nrow(matched) == 0L) {
      cli::cli_abort("Could not locate income values for {.val {class_name}}.")
    }

    tokens <- tail(matched$amounts[[1]], expected_n)
    tibble::tibble(
      class = class_name,
      income_ref_year = spec$income_ref_years,
      income_mean = purrr::map_dbl(tokens, .legacy_income_number)
    )
  })

  return(purrr::list_rbind(rows))
}

extract_2003_raw <- function(pdf, edition_id) {
  pdf <- as_cceb_pdf(pdf)

  result <- list(
    points = extract_2003_raw_points(pdf, edition_id),
    cutoffs = extract_2003_raw_cutoffs(pdf, edition_id),
    income = extract_2003_raw_income(pdf, edition_id),
    distribution = extract_2003_raw_distribution(pdf, edition_id)
  )

  return(result)
}
