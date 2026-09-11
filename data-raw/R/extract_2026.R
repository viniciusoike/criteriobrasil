.cceb_2026_classes <- c("A", "B1", "B2", "C1", "C2", "DE")

.cceb_2026_point_specs <- list(
  count = point_specs(
    variable = c(
      "automobiles",
      "refrigerators",
      "personal_computers",
      "bathrooms"
    ),
    label_en = c(
      "Automobiles",
      "Refrigerators",
      "Personal computers",
      "Bathrooms"
    ),
    aliases = list(
      "Automóveis",
      "Geladeiras",
      "Microcomputadores",
      "Banheiros"
    )
  ),
  binary = point_specs(
    variable = c(
      "dishwasher",
      "washing_machine",
      "microwave",
      "piped_water",
      "domestic_service"
    ),
    label_en = c(
      "Dishwasher",
      "Washing machine",
      "Microwave oven",
      "Piped water",
      "Domestic service"
    ),
    aliases = list(
      "Lavadora de louças",
      "Lavadora de roupas",
      "Micro-ondas",
      "Água encanada",
      "Serviço doméstico"
    )
  ),
  education = point_specs(
    variable = rep("householder_education", 7L),
    label_en = c(
      "No schooling",
      "Incomplete elementary school",
      "Elementary school diploma",
      "Incomplete high school",
      "High school diploma",
      "Incomplete higher education",
      "Higher education degree"
    ),
    aliases = list(
      "Sem Instrução",
      "Ensino Fundamental Incompleto",
      "Ensino Fundamental Completo",
      "Ensino Médio Incompleto",
      "Ensino Médio Completo",
      "Ensino Superior Incompleto",
      "Ensino Superior Completo"
    )
  )
)

.cceb_2026_labels <- function(specs) {
  return(purrr::list_c(specs$aliases))
}

# `lines` comes from `pdf_page_lines()` and is already squished.
.extract_numeric_rows <- function(lines, labels, n_values, section) {
  rows <- purrr::map(labels, function(label) {
    matches <- lines[stringr::str_starts(lines, stringr::str_escape(label))]
    if (length(matches) != 1L) {
      cli::cli_abort(
        "Expected one row for {.val {label}} in {section}; found {length(matches)}."
      )
    }

    values <- stringr::str_extract_all(matches, "[0-9]+")[[1]]
    if (length(values) != n_values) {
      cli::cli_abort(
        "Expected {n_values} values for {.val {label}} in {section}; found {length(values)}."
      )
    }

    tibble::tibble(label_pt = label, points = list(as.integer(values)))
  })

  return(purrr::list_rbind(rows))
}

extract_2026_raw_points <- function(pdf) {
  page <- pdf_page_containing(pdf, "SISTEMA DE PONTOS")
  lines <- pdf_page_lines(page)

  result <- list(
    count = .extract_numeric_rows(
      lines,
      .cceb_2026_labels(.cceb_2026_point_specs$count),
      5L,
      "count items"
    ),
    binary = .extract_numeric_rows(
      lines,
      .cceb_2026_labels(.cceb_2026_point_specs$binary),
      2L,
      "binary items"
    ),
    education = .extract_numeric_rows(
      lines,
      .cceb_2026_labels(.cceb_2026_point_specs$education),
      1L,
      "education"
    )
  )

  return(result)
}

extract_2026_raw_cutoffs <- function(pdf) {
  page <- pdf_page_containing(pdf, "Cortes do Critério Brasil")
  lines <- pdf_page_lines(page)
  pattern <- paste0(
    "^([1-6])\\s*[-–]\\s*",
    "(A|B1|B2|C1|C2|DE)\\s+",
    "([0-9]+)\\s*[-–]\\s*([0-9]+)$"
  )
  matches <- str_match_tibble(
    lines,
    pattern,
    c("line", "class_order", "class", "points_min", "points_max")
  )

  result <- matches |>
    dplyr::filter(!is.na(line)) |>
    dplyr::mutate(dplyr::across(!c(line, class), as.integer)) |>
    dplyr::select(class, class_order, points_min, points_max)

  if (nrow(result) != length(.cceb_2026_classes)) {
    cli::cli_abort(
      "Expected {length(.cceb_2026_classes)} cutoff rows; found {nrow(result)}."
    )
  }

  return(result)
}

extract_2026_raw_income <- function(pdf) {
  page <- pdf_page_containing(pdf, "Renda Média Domiciliar")
  lines <- pdf_page_lines(page)
  pattern <- paste0(
    "^(",
    paste(.cceb_2026_classes, collapse = "|"),
    ")\\s+R\\$\\s+([0-9.]+,[0-9]{2})$"
  )
  matches <- str_match_tibble(lines, pattern, c("line", "class", "amount"))

  result <- matches |>
    dplyr::filter(!is.na(line)) |>
    dplyr::mutate(income_mean = parse_brazilian_number(amount)) |>
    dplyr::select(class, income_mean)

  if (nrow(result) != length(.cceb_2026_classes)) {
    cli::cli_abort(
      "Expected {length(.cceb_2026_classes)} income rows; found {nrow(result)}."
    )
  }

  return(result)
}

# The regional and metropolitan tables share one page, in that order.
extract_2026_raw_distribution <- function(pdf) {
  page <- pdf_page_containing(pdf, "Cortes do Critério Brasil")
  lines <- pdf_page_lines(page)
  pattern <- "^([1-6])\\s*[-–]\\s*(A|B1|B2|C1|C2|DE)\\b"
  class_lines <- lines[
    stringr::str_detect(lines, pattern) & stringr::str_detect(lines, "%")
  ]

  if (length(class_lines) != 2L * length(.cceb_2026_classes)) {
    cli::cli_abort(
      "Expected {2L * length(.cceb_2026_classes)} distribution rows; found {length(class_lines)}."
    )
  }

  parse_distribution_rows <- function(rows, n_values, table_name) {
    parsed <- purrr::map(rows, function(line) {
      class <- stringr::str_match(line, pattern)[, 3]
      values <- stringr::str_extract_all(line, "[0-9]+(?:,[0-9]+)?%")[[1]]
      if (length(values) != n_values) {
        cli::cli_abort(
          "Expected {n_values} values in {table_name} row {.val {class}}; found {length(values)}."
        )
      }

      tibble::tibble(
        class = class,
        share = list(parse_cceb_percentages(values))
      )
    })

    return(purrr::list_rbind(parsed))
  }

  n_classes <- length(.cceb_2026_classes)
  result <- list(
    region = parse_distribution_rows(
      class_lines[seq_len(n_classes)],
      6L,
      "region"
    ),
    metro = parse_distribution_rows(
      class_lines[n_classes + seq_len(n_classes)],
      10L,
      "metropolitan"
    )
  )

  return(result)
}

extract_2026_raw <- function(pdf) {
  pdf <- as_cceb_pdf(pdf)

  result <- list(
    points = extract_2026_raw_points(pdf),
    cutoffs = extract_2026_raw_cutoffs(pdf),
    income = extract_2026_raw_income(pdf),
    distribution = extract_2026_raw_distribution(pdf)
  )

  return(result)
}
