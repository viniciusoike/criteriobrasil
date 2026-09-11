.cceb_2015_classes <- c("A", "B1", "B2", "C1", "C2", "DE")

.cceb_2015_point_specs <- list(
  count = tibble::tibble(
    variable = c(
      "bathrooms",
      "domestic_service",
      "automobiles",
      "personal_computers",
      "dishwasher",
      "refrigerators",
      "freezer",
      "washing_machine",
      "dvd",
      "microwave",
      "motorcycles",
      "clothes_dryer"
    ),
    label_en = c(
      "Bathrooms",
      "Domestic workers",
      "Automobiles",
      "Personal computers",
      "Dishwasher",
      "Refrigerator",
      "Freezer",
      "Washing machine",
      "DVD player",
      "Microwave oven",
      "Motorcycles",
      "Clothes dryer"
    ),
    aliases = list(
      "Banheiros",
      c("Empregados domésticos", "Trabalhadores domésticos"),
      "Automóveis",
      "Microcomputador",
      "Lava louca",
      "Geladeira",
      "Freezer",
      "Lava roupa",
      "DVD",
      "Micro-ondas",
      "Motocicleta",
      "Secadora roupa"
    )
  ),
  education = tibble::tibble(
    variable = rep("householder_education", 5L),
    label_en = c(
      "No schooling or incomplete elementary school",
      "Elementary school diploma or incomplete middle school",
      "Middle school diploma or incomplete high school",
      "High school diploma or incomplete higher education",
      "Higher education degree"
    ),
    aliases = list(
      "Analfabeto / Fundamental I incompleto",
      "Fundamental I completo / Fundamental II incompleto",
      "Fundamental II completo / Médio incompleto",
      "Médio completo / Superior incompleto",
      "Superior completo"
    )
  ),
  public_service = tibble::tibble(
    variable = c("piped_water", "paved_street"),
    label_en = c("Piped water", "Paved street"),
    aliases = list("Água encanada", "Rua pavimentada")
  )
)

.cceb_2015_class_pattern <-
  "(?:D\\s*[-–]\\s*E|DE|A|B1|B2|C1|C2)"

.cceb_2015_class_from_line <- function(lines) {
  pattern <- paste0(
    "^\\s*(?:[1-6]\\s*[-–]?\\s*)?(",
    .cceb_2015_class_pattern,
    ")(?:\\s|$)"
  )
  result <- stringr::str_match(
    lines,
    stringr::regex(pattern, ignore_case = TRUE)
  )[, 2]
  result <- stringr::str_to_upper(result)
  result <- stringr::str_replace_all(result, "\\s+", "")
  result <- stringr::str_replace(result, "^D[-–]E$", "DE")

  return(result)
}

.extract_2015_numeric_rows <- function(lines, specs, n_values, section) {
  lines <- stringr::str_squish(lines)

  result <- purrr::map_dfr(seq_len(nrow(specs)), function(index) {
    aliases <- specs$aliases[[index]]
    matches <- purrr::keep(
      lines,
      \(line) any(stringr::str_starts(line, aliases))
    )
    if (length(matches) != 1L) {
      cli::cli_abort(
        "Expected one row for {.val {specs$variable[[index]]}} in {section}; found {length(matches)}."
      )
    }

    values <- stringr::str_extract_all(matches[[1]], "[0-9]+")[[1]]
    if (length(values) != n_values) {
      cli::cli_abort(
        "Expected {n_values} values for {.val {specs$variable[[index]]}} in {section}; found {length(values)}."
      )
    }

    label_pt <- aliases[[which(stringr::str_starts(matches[[1]], aliases))[1]]]
    tibble::tibble(
      variable = specs$variable[[index]],
      label_pt = label_pt,
      points = list(as.integer(values))
    )
  })

  return(result)
}

extract_2015_raw_points <- function(pdf) {
  page <- pdf_page_containing(pdf, "SISTEMA DE PONTOS")
  lines <- unlist(strsplit(page, "\\n", fixed = FALSE))

  result <- list(
    count = .extract_2015_numeric_rows(
      lines,
      .cceb_2015_point_specs$count,
      5L,
      "count items"
    ),
    education = .extract_2015_numeric_rows(
      lines,
      .cceb_2015_point_specs$education,
      1L,
      "education"
    ),
    public_service = .extract_2015_numeric_rows(
      lines,
      .cceb_2015_point_specs$public_service,
      2L,
      "public services"
    )
  )

  return(result)
}

extract_2015_raw_cutoffs <- function(pdf) {
  page <- pdf_page_containing(pdf, "Cortes do Critério Brasil")
  lines <- stringr::str_squish(unlist(strsplit(page, "\\n")))
  classes <- .cceb_2015_class_from_line(lines)
  ranges <- stringr::str_match(lines, "([0-9]+)\\s*[-–]\\s*([0-9]+)")
  keep <- !is.na(classes) &
    !is.na(ranges[, 1]) &
    !stringr::str_detect(lines, "%")

  if (sum(keep) != length(.cceb_2015_classes)) {
    cli::cli_abort(
      "Expected {length(.cceb_2015_classes)} cutoff rows; found {sum(keep)}."
    )
  }

  result <- tibble::tibble(
    class = classes[keep],
    class_order = match(classes[keep], .cceb_2015_classes),
    points_min = as.integer(ranges[keep, 2]),
    points_max = as.integer(ranges[keep, 3])
  )
  result <- result[order(result$class_order), , drop = FALSE]

  return(result)
}

extract_2015_raw_income <- function(pdf) {
  has_income <- purrr::map_lgl(
    pdf$text,
    \(page) {
      stringr::str_detect(
        page,
        stringr::regex("Renda Média Domiciliar", ignore_case = TRUE)
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

  page <- pdf_page_containing(pdf, "Renda Média Domiciliar")
  lines <- stringr::str_squish(unlist(strsplit(page, "\\n")))
  classes <- .cceb_2015_class_from_line(lines)
  class_prefix <- paste0(
    "^\\s*(?:[1-6]\\s*[-–]?\\s*)?",
    .cceb_2015_class_pattern,
    "\\s*"
  )
  income_lines <- stringr::str_remove(
    lines,
    stringr::regex(class_prefix, ignore_case = TRUE)
  )
  amounts <- stringr::str_extract_all(
    income_lines,
    "[0-9]+(?:[.,][0-9]+)*"
  )
  keep <- !is.na(classes) & !stringr::str_detect(lines, "^Total\\b")
  keep <- keep & purrr::map_int(amounts, length) == 1L

  if (sum(keep) != length(.cceb_2015_classes)) {
    cli::cli_abort(
      "Expected {length(.cceb_2015_classes)} income rows; found {sum(keep)}."
    )
  }

  result <- tibble::tibble(
    class = classes[keep],
    income_mean = purrr::map_dbl(
      amounts[keep],
      \(value) parse_brazilian_number(value[[1]])
    )
  )
  result <- result[
    order(match(result$class, .cceb_2015_classes)),
    ,
    drop = FALSE
  ]

  return(result)
}

parse_cceb_percentages <- function(values) {
  values <- stringr::str_remove(values, "%")
  result <- purrr::map_dbl(values, function(value) {
    if (stringr::str_detect(value, ",")) {
      return(parse_brazilian_number(value) / 100)
    }

    as.numeric(value) / 100
  })

  if (anyNA(result)) {
    cli::cli_abort("Could not parse CCEB percentage values.")
  }

  return(result)
}

extract_2015_raw_distribution <- function(pdf) {
  distribution_pages <- purrr::map_lgl(
    pdf$text,
    \(page) {
      page_lines <- stringr::str_squish(unlist(strsplit(page, "\\n")))
      page_classes <- .cceb_2015_class_from_line(page_lines)
      page_values <- stringr::str_extract_all(
        page_lines,
        "[0-9]+(?:[,.][0-9]+)?%"
      )
      page_n_values <- purrr::map_int(page_values, length)
      sum(
        !is.na(page_classes) & page_n_values %in% c(6L, 10L)
      ) ==
        12L
    }
  )
  if (sum(distribution_pages) != 1L) {
    cli::cli_abort(
      "Expected one PDF page with the class distribution; found {sum(distribution_pages)}."
    )
  }
  page <- pdf$text[[which(distribution_pages)]]
  lines <- stringr::str_squish(unlist(strsplit(page, "\\n")))
  classes <- .cceb_2015_class_from_line(lines)
  values <- stringr::str_extract_all(
    lines,
    "[0-9]+(?:[,.][0-9]+)?%"
  )
  n_values <- purrr::map_int(values, length)
  keep <- !is.na(classes) & n_values %in% c(6L, 10L)
  class_lines <- lines[keep]
  classes <- classes[keep]
  values <- values[keep]

  if (length(class_lines) != 2L * length(.cceb_2015_classes)) {
    cli::cli_abort(
      "Expected {2L * length(.cceb_2015_classes)} distribution rows; found {length(class_lines)}."
    )
  }

  rows <- tibble::tibble(
    class = classes,
    share = purrr::map(values, parse_cceb_percentages)
  )
  region_rows <- rows[seq_len(length(.cceb_2015_classes)), , drop = FALSE]
  metro_rows <- rows[
    length(.cceb_2015_classes) + seq_len(length(.cceb_2015_classes)),
    ,
    drop = FALSE
  ]

  result <- list(region = region_rows, metro = metro_rows)

  return(result)
}

extract_2015_raw <- function(pdf) {
  pdf <- as_cceb_pdf(pdf)

  result <- list(
    points = extract_2015_raw_points(pdf),
    cutoffs = extract_2015_raw_cutoffs(pdf),
    income = extract_2015_raw_income(pdf),
    distribution = extract_2015_raw_distribution(pdf)
  )

  return(result)
}
