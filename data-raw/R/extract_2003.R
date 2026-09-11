.cceb_2003_ids <- c(2003L, 2008L, 2009L, 2010L, 2011L, 2012L, 2013L, 2014L)

.cceb_legacy_classes <- function(edition_id) {
  if (as.integer(edition_id) == 2003L) {
    return(c("A1", "A2", "B1", "B2", "C", "D", "E"))
  }

  c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E")
}

.cceb_legacy_distribution_classes <- function(edition_id) {
  if (as.integer(edition_id) == 2014L) {
    return(c("A1", "A2", "B1", "B2", "C1", "C2", "DE"))
  }

  .cceb_legacy_classes(edition_id)
}

.cceb_legacy_point_specs <- function(edition_id) {
  if (as.integer(edition_id) == 2003L) {
    count <- tibble::tibble(
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
  } else {
    count <- tibble::tibble(
      variable = c(
        "color_televisions",
        "radios",
        "bathrooms",
        "automobiles",
        "domestic_service",
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
        "Máquina de lavar",
        "Videocassete e/ou DVD",
        "Geladeira",
        "Freezer"
      )
    )
  }

  education <- tibble::tibble(
    variable = rep("householder_education", 5L),
    label_en = c(
      "No schooling or incomplete elementary school",
      "Elementary school diploma or incomplete middle school",
      "Middle school diploma or incomplete high school",
      "High school diploma or incomplete higher education",
      "Higher education degree"
    ),
    aliases = list(
      c(
        "Analfabeto / Primário incompleto",
        "Analfabeto/ Primário incompleto"
      ),
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

  list(count = count, education = education)
}

.legacy_class_from_line <- function(lines, classes) {
  classes <- classes[order(nchar(classes), decreasing = TRUE)]
  class_pattern <- paste(classes, collapse = "|")
  pattern <- paste0(
    "^\\s*(?:Classe\\s+)?(",
    class_pattern,
    ")(?:\\s|$)"
  )
  result <- stringr::str_match(
    lines,
    stringr::regex(pattern, ignore_case = TRUE)
  )[, 2]
  result <- stringr::str_to_upper(result)
  result <- stringr::str_replace_all(result, "\\s+", "")
  result <- stringr::str_replace(result, "^D-E$", "DE")

  return(result)
}

.legacy_class_prefix <- function(classes) {
  classes <- classes[order(nchar(classes), decreasing = TRUE)]
  paste0(
    "^\\s*(?:Classe\\s+)?(?:",
    paste(classes, collapse = "|"),
    ")\\s*"
  )
}

.legacy_numeric_rows <- function(lines, specs, n_values, section) {
  lines <- stringr::str_squish(lines)

  result <- purrr::map_dfr(seq_len(nrow(specs)), function(index) {
    aliases <- specs$aliases[[index]]
    match_indexes <- which(vapply(
      lines,
      \(line) any(stringr::str_starts(line, aliases)),
      logical(1)
    ))
    matches <- lines[match_indexes]
    if (length(matches) != 1L) {
      cli::cli_abort(
        "Expected one row for {.val {specs$variable[[index]]}} in {section}; found {length(matches)}."
      )
    }

    candidate <- matches[[1]]
    values <- stringr::str_extract_all(candidate, "[0-9]+")[[1]]
    if (length(values) == 0L && section == "education") {
      previous_index <- match_indexes[[1]] - 1L
      while (
        previous_index > 0L &&
          stringr::str_length(lines[[previous_index]]) == 0L
      ) {
        previous_index <- previous_index - 1L
      }
      if (previous_index > 0L) {
        previous_values <- stringr::str_extract_all(
          lines[[previous_index]],
          "[0-9]+"
        )[[1]]
        if (length(previous_values) >= n_values) {
          values <- previous_values
        }
      }
    }
    next_index <- match_indexes[[1]] + 1L
    while (length(values) < n_values && next_index <= length(lines)) {
      candidate <- paste(candidate, lines[[next_index]])
      values <- stringr::str_extract_all(candidate, "[0-9]+")[[1]]
      next_index <- next_index + 1L
    }
    if (length(values) < n_values) {
      cli::cli_abort(
        "Expected at least {n_values} values for {.val {specs$variable[[index]]}} in {section}; found {length(values)}."
      )
    }

    label_pt <- aliases[[which(stringr::str_starts(matches[[1]], aliases))[1]]]
    tibble::tibble(
      variable = specs$variable[[index]],
      label_pt = label_pt,
      points = list(as.integer(tail(values, n_values)))
    )
  })

  return(result)
}

extract_2003_raw_points <- function(pdf, edition_id) {
  page <- pdf_page_containing(pdf, "SISTEMA DE PONTOS")
  lines <- unlist(strsplit(page, "\\n", fixed = FALSE))
  specs <- .cceb_legacy_point_specs(edition_id)

  result <- list(
    count = .legacy_numeric_rows(
      lines,
      specs$count,
      5L,
      "count items"
    ),
    education = .legacy_numeric_rows(
      lines,
      specs$education,
      1L,
      "education"
    )
  )

  return(result)
}

extract_2003_raw_cutoffs <- function(pdf, edition_id) {
  classes_expected <- .cceb_legacy_classes(edition_id)
  page <- pdf_page_containing(pdf, "CORTES DO CRITÉRIO BRASIL")
  lines <- stringr::str_squish(unlist(strsplit(page, "\\n")))
  classes <- .legacy_class_from_line(lines, classes_expected)
  ranges <- stringr::str_match(lines, "([0-9]+)\\s*[-–]\\s*([0-9]+)")
  keep <- !is.na(classes) & !is.na(ranges[, 1])

  if (sum(keep) != length(classes_expected)) {
    cli::cli_abort(
      "Expected {length(classes_expected)} cutoff rows; found {sum(keep)}."
    )
  }

  result <- tibble::tibble(
    class = classes[keep],
    class_order = match(classes[keep], classes_expected),
    points_min = as.integer(ranges[keep, 2]),
    points_max = as.integer(ranges[keep, 3])
  )
  result <- result[order(result$class_order), , drop = FALSE]

  return(result)
}

.legacy_distribution_page <- function(pdf) {
  matches <- purrr::map_lgl(
    pdf$text,
    \(page) {
      stringr::str_detect(
        page,
        stringr::regex(
          "DISTRIBUIÇÃO|Distribuição das classes",
          ignore_case = TRUE
        )
      )
    }
  )
  if (sum(matches) == 0L) {
    return(NULL)
  }
  if (sum(matches) != 1L) {
    cli::cli_abort(
      "Expected one legacy distribution page; found {sum(matches)}."
    )
  }

  pdf$text[[which(matches)]]
}

.legacy_distribution_values <- function(line, classes) {
  prefix <- .legacy_class_prefix(classes)
  stripped <- stringr::str_remove(
    line,
    stringr::regex(prefix, ignore_case = TRUE)
  )
  stringr::str_extract_all(
    stripped,
    "[0-9]+(?:[,.][0-9]+)?%?"
  )[[1]]
}

.legacy_share_values <- function(values) {
  if (any(stringr::str_detect(values, "%"))) {
    values <- stringr::str_remove(values, "%")
    result <- purrr::map_dbl(values, function(value) {
      if (stringr::str_detect(value, ",")) {
        return(parse_brazilian_number(value) / 100)
      }

      as.numeric(value) / 100
    })
    if (anyNA(result)) {
      cli::cli_abort("Could not parse legacy percentage values.")
    }
    return(result)
  }

  numeric_values <- purrr::map_dbl(values, function(value) {
    if (stringr::str_detect(value, ",")) {
      return(parse_brazilian_number(value))
    }

    as.numeric(value)
  })
  numeric_values / 100
}

.legacy_distribution_rows <- function(
  lines,
  classes,
  distribution_type,
  occurrence = 1L
) {
  lines <- stringr::str_squish(lines)
  line_classes <- .legacy_class_from_line(lines, classes)

  if (distribution_type == "metro_first") {
    result <- purrr::map_dfr(classes, function(class) {
      indexes <- which(line_classes == class)
      candidates <- purrr::map(
        indexes,
        \(index) .legacy_distribution_values(lines[[index]], classes)
      )
      label_index <- indexes[
        which(purrr::map_int(candidates, length) == 1L)[1]
      ]
      if (is.na(label_index)) {
        cli::cli_abort(
          "Could not locate the total for legacy class {.val {class}}."
        )
      }
      previous <- seq_len(label_index - 1L)
      previous <- previous[
        vapply(
          previous,
          \(index) {
            length(.legacy_distribution_values(lines[[index]], classes)) == 9L
          },
          logical(1)
        )
      ]
      if (length(previous) == 0L) {
        cli::cli_abort(
          "Could not locate the metropolitan values for {.val {class}}."
        )
      }
      metro_values <- .legacy_distribution_values(
        lines[[tail(previous, 1L)]],
        classes
      )
      total_value <- .legacy_distribution_values(lines[[label_index]], classes)
      values <- c(total_value, metro_values)
      tibble::tibble(class = class, share = list(.legacy_share_values(values)))
    })
  } else {
    result <- purrr::map_dfr(classes, function(class) {
      indexes <- which(line_classes == class)
      candidates <- purrr::map(
        indexes,
        \(index) .legacy_distribution_values(lines[[index]], classes)
      )
      matches <- candidates[purrr::map_int(candidates, length) == 10L]
      if (length(matches) < occurrence) {
        cli::cli_abort(
          "Could not locate distribution occurrence {occurrence} for {.val {class}}."
        )
      }
      values <- .legacy_share_values(matches[[occurrence]])
      if (distribution_type == "metro_total_last") {
        values <- c(values[[10]], values[seq_len(9L)])
      }
      tibble::tibble(class = class, share = list(values))
    })
  }

  return(result)
}

extract_2003_raw_distribution <- function(pdf, edition_id) {
  spec <- .cceb_2003_edition_specs[[as.character(edition_id)]]
  if (as.integer(edition_id) == 2013L) {
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
    table <- purrr::map_dfr(spec$distribution_classes, function(class) {
      rows <- manual[manual$class == class, , drop = FALSE]
      indexes <- match(geo_codes, rows$geo_code)
      if (anyNA(indexes)) {
        cli::cli_abort(
          "The manual 2013 distribution is incomplete for class {.val {class}}."
        )
      }
      tibble::tibble(
        class = class,
        share = list(rows$share[indexes]),
        ref_year = spec$distribution_ref_year
      )
    })

    return(list(table = table, distribution_type = "manual_image"))
  }

  page <- .legacy_distribution_page(pdf)
  if (is.null(page)) {
    return(list(
      table = tibble::tibble(
        class = character(),
        share = vector("list", 0L)
      ),
      distribution_type = "none"
    ))
  }

  lines <- unlist(strsplit(page, "\\n", fixed = FALSE))
  if (as.integer(edition_id) == 2009L) {
    table <- purrr::map2_dfr(
      c(2006L, 2007L),
      c(1L, 2L),
      function(ref_year, occurrence) {
        rows <- .legacy_distribution_rows(
          lines,
          spec$distribution_classes,
          spec$distribution_type,
          occurrence = occurrence
        )
        rows$ref_year <- ref_year
        rows
      }
    )
  } else {
    table <- .legacy_distribution_rows(
      lines,
      spec$distribution_classes,
      spec$distribution_type
    )
    table$ref_year <- spec$distribution_ref_year
  }
  result <- list(table = table, distribution_type = spec$distribution_type)

  return(result)
}

.legacy_income_number <- function(value) {
  numeric_value <- as.numeric(stringr::str_remove_all(value, "[^0-9]"))
  if (is.na(numeric_value)) {
    cli::cli_abort("Could not parse legacy income value {.val {value}}.")
  }

  numeric_value
}

.read_2013_manual_table <- function(
  pdf,
  table_name,
  manual_dir = NULL
) {
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
  row <- manifest[
    manifest$edition_id == 2013L & manifest$table_name == table_name,
    ,
    drop = FALSE
  ]
  if (nrow(row) != 1L) {
    cli::cli_abort(
      "Expected one manual 2013 {.val {table_name}} table; found {nrow(row)}."
    )
  }
  if (is.null(pdf$path) || !file.exists(pdf$path)) {
    cli::cli_abort("The 2013 manual table requires the source PDF path.")
  }
  if (!identical(sha256_file(pdf$path), row$source_sha256[[1]])) {
    cli::cli_abort(
      "The manual 2013 {.val {table_name}} table does not match the source PDF hash."
    )
  }

  path <- file.path(manual_dir, row$file_name[[1]])
  if (!file.exists(path)) {
    cli::cli_abort("The manual table {.path {path}} is missing.")
  }

  return(readr::read_csv(path, show_col_types = FALSE))
}

extract_2003_raw_income <- function(pdf, edition_id) {
  spec <- .cceb_2003_edition_specs[[as.character(edition_id)]]
  if (as.integer(edition_id) == 2013L) {
    result <- .read_2013_manual_table(pdf, "income")
    result$income_ref_year <- spec$income_ref_year
    return(result[, c("class", "income_ref_year", "income_mean")])
  }
  if (length(spec$income_ref_years) == 0L) {
    return(tibble::tibble(
      class = character(),
      income_ref_year = integer(),
      income_mean = numeric()
    ))
  }
  has_income <- purrr::map_lgl(
    pdf$text,
    \(page) {
      stringr::str_detect(
        page,
        stringr::regex(
          "Renda média.*familiar|RENDA FAMILIAR POR CLASSES",
          ignore_case = TRUE
        )
      )
    }
  )
  if (!any(has_income)) {
    return(tibble::tibble(
      class = character(),
      income_ref_year = integer(),
      income_mean = numeric()
    ))
  }

  page <- pdf$text[[which(has_income)[1]]]
  lines <- stringr::str_squish(unlist(strsplit(page, "\\n")))
  income_header <- which(stringr::str_detect(
    lines,
    stringr::regex(
      "RENDA FAMILIAR POR CLASSES|Renda média bruta familiar",
      ignore_case = TRUE
    )
  ))
  if (length(income_header) == 0L) {
    cli::cli_abort("Could not locate the legacy income table header.")
  }
  lines <- lines[income_header[[1]]:length(lines)]
  classes_expected <- spec$income_classes
  classes <- .legacy_class_from_line(lines, classes_expected)
  prefix <- .legacy_class_prefix(classes_expected)
  stripped <- stringr::str_remove(
    lines,
    stringr::regex(prefix, ignore_case = TRUE)
  )
  values <- stringr::str_extract_all(
    stripped,
    "[0-9]+(?:[,.][0-9]+)*"
  )
  expected_n <- length(spec$income_ref_years)
  candidates <- purrr::map_int(values, length)
  result <- purrr::map_dfr(classes_expected, function(class) {
    indexes <- which(classes == class & candidates >= expected_n)
    if (length(indexes) == 0L) {
      cli::cli_abort("Could not locate income values for {.val {class}}.")
    }
    tokens <- tail(values[[indexes[[1]]]], expected_n)
    tibble::tibble(
      class = class,
      income_ref_year = spec$income_ref_years,
      income_mean = purrr::map_dbl(tokens, .legacy_income_number)
    )
  })

  return(result)
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
