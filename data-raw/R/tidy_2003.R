.cceb_2003_edition_specs <- list(
  `2003` = list(
    effective_date = "2003-01-01",
    effective_date_source = "stated",
    income_source = "LSE 2000",
    income_ref_year = 2000L,
    income_ref_years = 2000L,
    income_classes = c("A1", "A2", "B1", "B2", "C", "D", "E"),
    distribution_classes = c("A1", "A2", "B1", "B2", "C", "D", "E"),
    distribution_type = "country_first",
    distribution_ref_year = 2000L,
    distribution_ref_source = "IBOPE LSE 2000",
    distribution_source = "IBOPE LSE 2000",
    method_note = "The old regime has seven classes and a 34-point maximum."
  ),
  `2008` = list(
    effective_date = "2008-01-01",
    effective_date_source = "stated",
    income_source = "LSE 2005",
    income_ref_year = 2005L,
    income_ref_years = 2005L,
    income_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
    distribution_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
    distribution_type = "country_first",
    distribution_ref_year = 2005L,
    distribution_ref_source = "IBOPE LSE 2005",
    distribution_source = "IBOPE LSE 2005",
    method_note = NA_character_
  ),
  `2009` = list(
    effective_date = "2009-01-01",
    effective_date_source = "stated",
    income_source = "LSE 2006/2007",
    income_ref_year = NA_integer_,
    income_ref_years = c(2006L, 2007L),
    income_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
    distribution_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
    distribution_type = "country_first",
    distribution_ref_year = 2006L,
    distribution_ref_source = "IBOPE LSE 2006",
    distribution_source = "IBOPE LSE 2006/2007",
    method_note = paste(
      "The source reports income for both 2006 and 2007;",
      "the row-level income_ref_year preserves both columns."
    )
  ),
  `2010` = list(
    effective_date = "2010-01-01",
    effective_date_source = "stated",
    income_source = "LSE 2008",
    income_ref_year = 2008L,
    income_ref_years = 2008L,
    income_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
    distribution_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
    distribution_type = "country_first",
    distribution_ref_year = 2008L,
    distribution_ref_source = "IBOPE LSE 2008",
    distribution_source = "IBOPE LSE 2008",
    method_note = "The filename identifies this document as in force in 2010."
  ),
  `2011` = list(
    effective_date = "2011-01-01",
    effective_date_source = "inferred",
    income_source = "LSE 2009",
    income_ref_year = 2009L,
    income_ref_years = 2009L,
    income_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
    distribution_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
    distribution_type = "metro_first",
    distribution_ref_year = 2009L,
    distribution_ref_source = "IBOPE LSE 2009",
    distribution_source = "IBOPE LSE 2009",
    method_note = "The effective date is inferred from the 2011 publication footer."
  ),
  `2012` = list(
    effective_date = "2012-01-01",
    effective_date_source = "stated",
    income_source = "LSE 2010",
    income_ref_year = 2010L,
    income_ref_years = 2010L,
    income_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
    distribution_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
    distribution_type = "metro_total_last",
    distribution_ref_year = 2010L,
    distribution_ref_source = "IBOPE LSE 2010",
    distribution_source = "IBOPE LSE 2010",
    method_note = NA_character_
  ),
  `2013` = list(
    effective_date = "2013-01-01",
    effective_date_source = "stated",
    income_source = "IBOPE Mídia LSE 2011",
    income_ref_year = 2011L,
    income_ref_years = 2011L,
    income_classes = c("A", "B1", "B2", "C1", "C2", "DE"),
    distribution_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "DE"),
    distribution_type = "manual_image",
    distribution_ref_year = 2011L,
    distribution_ref_source = "IBOPE Mídia LSE 2011",
    distribution_source = "IBOPE Mídia LSE 2011",
    method_note = paste(
      "The income and distribution tables are embedded as images;",
      "their values are transcribed from pages 4 and 5."
    )
  ),
  `2014` = list(
    effective_date = "2014-01-01",
    effective_date_source = "stated",
    income_source = "LSE 2012",
    income_ref_year = 2012L,
    income_ref_years = 2012L,
    income_classes = c("A", "B1", "B2", "C1", "C2", "DE"),
    distribution_classes = c("A1", "A2", "B1", "B2", "C1", "C2", "DE"),
    distribution_type = "metro_total_last",
    distribution_ref_year = 2012L,
    distribution_ref_source = "IBOPE Media LSE 2012",
    distribution_source = "IBOPE Media LSE 2012",
    method_note = paste(
      "Point cuts retain D and E, while the published distribution and",
      "income tables combine them as DE and A1/A2 income as A."
    )
  )
)

.cceb_2003_edition_spec <- function(edition_id) {
  result <- .cceb_2003_edition_specs[[as.character(edition_id)]]
  if (is.null(result)) {
    cli::cli_abort("Unknown legacy edition {.val {edition_id}}.")
  }

  result
}

.tidy_legacy_points_block <- function(
  raw_block,
  specs,
  block,
  levels,
  edition_id
) {
  result <- purrr::map_dfr(seq_len(nrow(specs)), function(index) {
    raw_index <- which(
      raw_block$variable == specs$variable[[index]] &
        raw_block$label_pt %in% specs$aliases[[index]]
    )
    if (length(raw_index) != 1L) {
      cli::cli_abort("Missing raw point row {.val {specs$variable[[index]]}}.")
    }
    raw_index <- raw_index[[1]]
    points <- raw_block$points[[raw_index]]
    if (length(points) != length(levels)) {
      cli::cli_abort(
        "Expected {length(levels)} levels for {.val {specs$variable[[index]]}}; found {length(points)}."
      )
    }

    tibble::tibble(
      edition_id = edition_id,
      block = block,
      variable = specs$variable[[index]],
      label_pt = raw_block$label_pt[[raw_index]],
      label_en = specs$label_en[[index]],
      level = levels,
      level_order = seq_along(levels),
      points = points
    )
  })

  result
}

.tidy_legacy_education_points <- function(raw_block, specs, edition_id) {
  levels <- c(
    "no_schooling_or_incomplete_elementary_school",
    "elementary_school_diploma_or_incomplete_middle_school",
    "middle_school_diploma_or_incomplete_high_school",
    "high_school_diploma_or_incomplete_higher_education",
    "higher_education_degree"
  )
  purrr::map_dfr(seq_len(nrow(specs)), function(index) {
    raw_index <- which(
      raw_block$variable == specs$variable[[index]] &
        raw_block$label_pt %in% specs$aliases[[index]]
    )
    if (length(raw_index) != 1L) {
      cli::cli_abort("Missing raw education row at position {index}.")
    }
    raw_index <- raw_index[[1]]
    points <- raw_block$points[[raw_index]]
    if (length(points) != 1L) {
      cli::cli_abort("Expected one point value for education level {index}.")
    }

    tibble::tibble(
      edition_id = edition_id,
      block = "education",
      variable = specs$variable[[index]],
      label_pt = raw_block$label_pt[[raw_index]],
      label_en = specs$label_en[[index]],
      level = levels[[index]],
      level_order = index,
      points = points[[1]]
    )
  })
}

tidy_2003_points <- function(raw_points, edition_id) {
  specs <- .cceb_legacy_point_specs(edition_id)
  count <- .tidy_legacy_points_block(
    raw_points$count,
    specs$count,
    "count_item",
    c("0", "1", "2", "3", "4_plus"),
    edition_id
  )
  education <- .tidy_legacy_education_points(
    raw_points$education,
    specs$education,
    edition_id
  )

  dplyr::bind_rows(count, education)
}

tidy_2003_editions <- function(edition_id, links = NULL) {
  spec <- .cceb_2003_edition_spec(edition_id)
  url_pt <- NA_character_
  if (!is.null(links) && nrow(links) > 0L) {
    pt <- links$url[links$lang == "pt"]
    if (length(pt) == 1L) {
      url_pt <- pt
    }
  }

  tibble::tibble(
    edition_id = edition_id,
    effective_date = as.Date(spec$effective_date),
    effective_date_source = spec$effective_date_source,
    regime = "2003",
    income_source = spec$income_source,
    income_ref_year = spec$income_ref_year,
    income_concept = "family",
    distribution_source = spec$distribution_source,
    lang = "pt",
    url_pt = url_pt,
    url_en = NA_character_,
    method_note = spec$method_note
  )
}

tidy_2003_cutoffs <- function(raw_cutoffs, edition_id) {
  result <- raw_cutoffs
  result$edition_id <- edition_id
  result[, c("edition_id", "class", "class_order", "points_min", "points_max")]
}

tidy_2003_income <- function(raw_income, edition_id) {
  result <- raw_income
  result$edition_id <- edition_id
  result$income_ref_date <- as.Date(NA)
  result$concept <- "family"
  result[, c(
    "edition_id",
    "class",
    "income_mean",
    "income_ref_date",
    "income_ref_year",
    "concept"
  )]
}

.legacy_distribution_metadata <- function(distribution_type, spec) {
  if (distribution_type == "country_first") {
    result <- tibble::tribble(
      ~geo_level , ~geo_code , ~geo_name        ,
      "country"  , "BR"      , "Brasil"         ,
      "metro"    , "FOR"     , "Fortaleza"      ,
      "metro"    , "REC"     , "Recife"         ,
      "metro"    , "SSA"     , "Salvador"       ,
      "metro"    , "BH"      , "Belo Horizonte" ,
      "metro"    , "RJ"      , "Rio de Janeiro" ,
      "metro"    , "SP"      , "São Paulo"      ,
      "metro"    , "CWB"     , "Curitiba"       ,
      "metro"    , "POA"     , "Porto Alegre"   ,
      "metro"    , "BSB"     , "Brasília"
    )
  } else {
    result <- tibble::tribble(
      ~geo_level    , ~geo_code , ~geo_name        ,
      "metro_total" , "9_rms"   , "9 RMs"          ,
      "metro"       , "FOR"     , "Fortaleza"      ,
      "metro"       , "REC"     , "Recife"         ,
      "metro"       , "SSA"     , "Salvador"       ,
      "metro"       , "BH"      , "Belo Horizonte" ,
      "metro"       , "RJ"      , "Rio de Janeiro" ,
      "metro"       , "SP"      , "São Paulo"      ,
      "metro"       , "CWB"     , "Curitiba"       ,
      "metro"       , "POA"     , "Porto Alegre"   ,
      "metro"       , "BSB"     , "Brasília"
    )
  }
  result$ref_year <- spec$distribution_ref_year
  result$ref_source <- spec$distribution_ref_source
  result
}

tidy_2003_distribution <- function(raw_distribution, edition_id) {
  spec <- .cceb_2003_edition_spec(edition_id)
  if (nrow(raw_distribution$table) == 0L) {
    return(tibble::tibble(
      edition_id = integer(),
      geo_level = character(),
      geo_code = character(),
      geo_name = character(),
      class = character(),
      share = numeric(),
      ref_year = integer(),
      ref_source = character()
    ))
  }

  ref_years <- unique(raw_distribution$table$ref_year)
  purrr::map_dfr(ref_years, function(ref_year) {
    raw_table <- raw_distribution$table[
      raw_distribution$table$ref_year == ref_year,
      ,
      drop = FALSE
    ]
    metadata <- .legacy_distribution_metadata(
      raw_distribution$distribution_type,
      spec
    )
    metadata$ref_year <- ref_year

    purrr::map_dfr(seq_len(nrow(metadata)), function(index) {
      shares <- purrr::map_dbl(
        raw_table$share,
        \(values) values[[index]]
      )
      tibble::tibble(
        edition_id = edition_id,
        geo_level = metadata$geo_level[[index]],
        geo_code = metadata$geo_code[[index]],
        geo_name = metadata$geo_name[[index]],
        class = raw_table$class,
        share = shares,
        ref_year = metadata$ref_year[[index]],
        ref_source = metadata$ref_source[[index]]
      )
    })
  })
}

tidy_2003 <- function(raw, edition_id, links = NULL) {
  list(
    cceb_editions = tidy_2003_editions(edition_id, links),
    cceb_points = tidy_2003_points(raw$points, edition_id),
    cceb_cutoffs = tidy_2003_cutoffs(raw$cutoffs, edition_id),
    cceb_income = tidy_2003_income(raw$income, edition_id),
    cceb_distribution = tidy_2003_distribution(raw$distribution, edition_id)
  )
}
