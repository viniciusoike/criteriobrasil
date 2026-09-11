.cceb_2015_classes <- c("A", "B1", "B2", "C1", "C2", "DE")

.cceb_2015_point_specs <- list(
  count = point_specs(
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
  education = point_specs(
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
  public_service = point_specs(
    variable = c("piped_water", "paved_street"),
    label_en = c("Piped water", "Paved street"),
    aliases = list("Água encanada", "Rua pavimentada")
  )
)

.cceb_2015_class_pattern <-
  "(?:D\\s*[-–]\\s*E|DE|A|B1|B2|C1|C2)"

.cceb_2015_class_prefix <- paste0(
  "^\\s*(?:[1-6]\\s*[-–]?\\s*)?",
  .cceb_2015_class_pattern,
  "\\s*"
)

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
  result <- stringr::str_remove_all(result, "\\s+")
  result <- stringr::str_replace(result, "^D[-–]E$", "DE")

  return(result)
}

.extract_2015_numeric_rows <- function(lines, specs, n_values, section) {
  rows <- purrr::map(seq_len(nrow(specs)), function(index) {
    aliases <- specs$aliases[[index]]
    matches <- lines[alias_line_indexes(lines, aliases)]
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

    tibble::tibble(
      variable = specs$variable[[index]],
      label_pt = alias_label(matches[[1]], aliases),
      points = list(as.integer(values))
    )
  })

  return(purrr::list_rbind(rows))
}

extract_2015_raw_points <- function(pdf) {
  page <- pdf_page_containing(pdf, "SISTEMA DE PONTOS")
  lines <- pdf_page_lines(page)

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
  lines <- pdf_page_lines(page)
  candidates <- str_match_tibble(
    lines,
    "([0-9]+)\\s*[-–]\\s*([0-9]+)",
    c("range", "points_min", "points_max")
  )
  candidates <- dplyr::mutate(
    candidates,
    class = .cceb_2015_class_from_line(lines),
    is_share = stringr::str_detect(lines, "%")
  )

  cutoffs <- dplyr::filter(candidates, !is.na(class), !is.na(range), !is_share)
  if (nrow(cutoffs) != length(.cceb_2015_classes)) {
    cli::cli_abort(
      "Expected {length(.cceb_2015_classes)} cutoff rows; found {nrow(cutoffs)}."
    )
  }

  result <- cutoffs |>
    dplyr::mutate(
      class_order = match(class, .cceb_2015_classes),
      dplyr::across(c(points_min, points_max), as.integer)
    ) |>
    dplyr::arrange(class_order) |>
    dplyr::select(class, class_order, points_min, points_max)

  return(result)
}

extract_2015_raw_income <- function(pdf) {
  if (length(pdf_pages_matching(pdf, "Renda Média Domiciliar")) == 0L) {
    return(empty_cceb_income())
  }

  page <- pdf_page_containing(pdf, "Renda Média Domiciliar")
  lines <- pdf_page_lines(page)
  income_lines <- stringr::str_remove(
    lines,
    stringr::regex(.cceb_2015_class_prefix, ignore_case = TRUE)
  )
  candidates <- tibble::tibble(
    class = .cceb_2015_class_from_line(lines),
    is_total = stringr::str_detect(lines, "^Total\\b"),
    amounts = stringr::str_extract_all(income_lines, "[0-9]+(?:[.,][0-9]+)*")
  )

  matched <- dplyr::filter(
    candidates,
    !is.na(class),
    !is_total,
    purrr::map_int(amounts, length) == 1L
  )
  if (nrow(matched) != length(.cceb_2015_classes)) {
    cli::cli_abort(
      "Expected {length(.cceb_2015_classes)} income rows; found {nrow(matched)}."
    )
  }

  result <- matched |>
    dplyr::mutate(
      income_mean = purrr::map_dbl(
        amounts,
        \(amount) parse_brazilian_number(amount[[1]])
      )
    ) |>
    dplyr::arrange(match(class, .cceb_2015_classes)) |>
    dplyr::select(class, income_mean)

  return(result)
}

# The distribution page is the one holding twelve class rows, six for the
# regional table and six for the metropolitan one.
.is_2015_distribution_page <- function(page) {
  lines <- pdf_page_lines(page)
  classes <- .cceb_2015_class_from_line(lines)
  values <- stringr::str_extract_all(lines, "[0-9]+(?:[,.][0-9]+)?%")
  n_class_rows <- sum(
    !is.na(classes) & purrr::map_int(values, length) %in% c(6L, 10L)
  )

  return(n_class_rows == 12L)
}

extract_2015_raw_distribution <- function(pdf) {
  pages <- which(purrr::map_lgl(pdf$text, .is_2015_distribution_page))
  if (length(pages) != 1L) {
    cli::cli_abort(
      "Expected one PDF page with the class distribution; found {length(pages)}."
    )
  }

  lines <- pdf_page_lines(pdf$text[[pages]])
  candidates <- tibble::tibble(
    class = .cceb_2015_class_from_line(lines),
    values = stringr::str_extract_all(lines, "[0-9]+(?:[,.][0-9]+)?%")
  )
  rows <- dplyr::filter(
    candidates,
    !is.na(class),
    purrr::map_int(values, length) %in% c(6L, 10L)
  )

  n_classes <- length(.cceb_2015_classes)
  if (nrow(rows) != 2L * n_classes) {
    cli::cli_abort(
      "Expected {2L * n_classes} distribution rows; found {nrow(rows)}."
    )
  }

  rows <- rows |>
    dplyr::mutate(share = purrr::map(values, parse_cceb_percentages)) |>
    dplyr::select(class, share)
  result <- list(
    region = dplyr::slice(rows, seq_len(n_classes)),
    metro = dplyr::slice(rows, n_classes + seq_len(n_classes))
  )

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
