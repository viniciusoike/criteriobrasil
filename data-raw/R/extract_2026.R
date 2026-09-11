.cceb_2026_classes <- c("A", "B1", "B2", "C1", "C2", "DE")

.extract_numeric_rows <- function(lines, labels, n_values, section) {
  lines <- stringr::str_squish(lines)

  rows <- purrr::map_dfr(labels, function(label) {
    matches <- lines[stringr::str_starts(lines, label)]
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

  return(rows)
}

extract_2026_raw_points <- function(pdf) {
  page <- pdf_page_containing(pdf, "SISTEMA DE PONTOS")
  lines <- unlist(strsplit(page, "\\n", fixed = FALSE))

  count_labels <- c(
    "Automóveis",
    "Geladeiras",
    "Microcomputadores",
    "Banheiros"
  )
  binary_labels <- c(
    "Lavadora de louças",
    "Lavadora de roupas",
    "Micro-ondas",
    "Água encanada",
    "Serviço doméstico"
  )
  education_labels <- c(
    "Sem Instrução",
    "Ensino Fundamental Incompleto",
    "Ensino Fundamental Completo",
    "Ensino Médio Incompleto",
    "Ensino Médio Completo",
    "Ensino Superior Incompleto",
    "Ensino Superior Completo"
  )

  result <- list(
    count = .extract_numeric_rows(lines, count_labels, 5L, "count items"),
    binary = .extract_numeric_rows(lines, binary_labels, 2L, "binary items"),
    education = .extract_numeric_rows(lines, education_labels, 1L, "education")
  )

  return(result)
}

extract_2026_raw_cutoffs <- function(pdf) {
  page <- pdf_page_containing(pdf, "Cortes do Critério Brasil")
  lines <- stringr::str_squish(unlist(strsplit(page, "\\n")))
  pattern <- paste0(
    "^([1-6])\\s*[-–]\\s*",
    "(A|B1|B2|C1|C2|DE)\\s+",
    "([0-9]+)\\s*[-–]\\s*([0-9]+)$"
  )
  matches <- stringr::str_match(lines, pattern)
  matches <- matches[!is.na(matches[, 1]), , drop = FALSE]

  if (nrow(matches) != length(.cceb_2026_classes)) {
    cli::cli_abort(
      "Expected {length(.cceb_2026_classes)} cutoff rows; found {nrow(matches)}."
    )
  }

  result <- tibble::tibble(
    class = matches[, 3],
    class_order = as.integer(matches[, 2]),
    points_min = as.integer(matches[, 4]),
    points_max = as.integer(matches[, 5])
  )

  return(result)
}

extract_2026_raw_income <- function(pdf) {
  page <- pdf_page_containing(pdf, "Renda Média Domiciliar")
  lines <- stringr::str_squish(unlist(strsplit(page, "\\n")))
  class_pattern <- paste(.cceb_2026_classes, collapse = "|")
  pattern <- paste0(
    "^(",
    class_pattern,
    ")\\s+R\\$\\s+([0-9.]+,[0-9]{2})$"
  )
  matches <- stringr::str_match(lines, pattern)
  matches <- matches[!is.na(matches[, 1]), , drop = FALSE]

  if (nrow(matches) != length(.cceb_2026_classes)) {
    cli::cli_abort(
      "Expected {length(.cceb_2026_classes)} income rows; found {nrow(matches)}."
    )
  }

  result <- tibble::tibble(
    class = matches[, 2],
    income_mean = purrr::map_dbl(matches[, 3], parse_brazilian_number)
  )

  return(result)
}

extract_2026_raw_distribution <- function(pdf) {
  page <- pdf_page_containing(pdf, "Cortes do Critério Brasil")
  lines <- stringr::str_squish(unlist(strsplit(page, "\\n")))
  pattern <- paste0(
    "^([1-6])\\s*[-–]\\s*(A|B1|B2|C1|C2|DE)\\b"
  )
  class_lines <- lines[
    stringr::str_detect(lines, pattern) & stringr::str_detect(lines, "%")
  ]

  if (length(class_lines) != 2L * length(.cceb_2026_classes)) {
    cli::cli_abort(
      "Expected {2L * length(.cceb_2026_classes)} distribution rows; found {length(class_lines)}."
    )
  }

  parse_distribution_rows <- function(rows, n_values, table_name) {
    result <- purrr::map_dfr(rows, function(line) {
      class <- stringr::str_match(line, pattern)[, 3]
      values <- stringr::str_extract_all(
        line,
        "[0-9]+(?:,[0-9]+)?%"
      )[[1]]
      if (length(values) != n_values) {
        cli::cli_abort(
          "Expected {n_values} values in {table_name} row {.val {class}}; found {length(values)}."
        )
      }

      tibble::tibble(
        class = class,
        share = list(parse_brazilian_number(values) / 100)
      )
    })

    return(result)
  }

  result <- list(
    region = parse_distribution_rows(
      class_lines[seq_len(length(.cceb_2026_classes))],
      6L,
      "region"
    ),
    metro = parse_distribution_rows(
      class_lines[
        length(.cceb_2026_classes) + seq_len(length(.cceb_2026_classes))
      ],
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
