.cceb_2015_edition_specs <- tibble::tribble(
  ~edition_id                                                                           , ~effective_date                                    , ~effective_date_source , ~income_source ,
  ~income_ref_year                                                                      , ~distribution_source                               , ~region_ref_year       ,
  ~region_ref_source                                                                    , ~metro_ref_year                                    , ~metro_ref_source      , ~method_note   ,
  2015L                                                                                 , "2015-01-01"                                       , "stated"               , NA_character_  , NA_integer_ ,
  "Datafolha and IBOPE Inteligência; GFK, IPSOS and IBOPE Mídia (LSE)"                  ,
  NA_integer_                                                                           , "Datafolha and IBOPE Inteligência"                 , NA_integer_            ,
  "GFK, IPSOS and IBOPE Mídia (LSE)"                                                    ,
  "The PDF footer is dated 2014 and the edition has no income table."                   ,
  2016L                                                                                 , "2016-04-11"                                       , "inferred"             , "PNAD 2014"    , 2014L       ,
  "Datafolha, IBOPE Inteligência, GFK, IPSOS and Kantar IBOPE Media (LSE)"              ,
  NA_integer_                                                                           , "Datafolha and IBOPE Inteligência"                 , NA_integer_            ,
  "GFK, IPSOS and Kantar IBOPE Media (LSE)"                                             ,
  "The effective date is inferred from the 11/04/2016 filename."                        ,
  2018L                                                                                 , "2018-04-16"                                       , "stated"               , "PNADC 2017"   , 2017L       ,
  "Datafolha and IBOPE Inteligência; IPSOS and Kantar IBOPE Media (LSE)"                ,
  NA_integer_                                                                           , "Datafolha and IBOPE Inteligência"                 , NA_integer_            ,
  "IPSOS and Kantar IBOPE Media (LSE)"                                                  ,
  "The published D-E cutoff starts at 1, leaving point 0 outside the ranges."           ,
  2019L                                                                                 , "2019-06-01"                                       , "stated"               , "PNADC 2018"   , 2018L       ,
  "Datafolha and IBOPE Inteligência; Kantar IBOPE Media (base 2018)"                    ,
  NA_integer_                                                                           , "Datafolha and IBOPE Inteligência"                 , 2018L                  ,
  "Kantar IBOPE Media (base 2018)"                                                      ,
  NA_character_                                                                         ,
  2020L                                                                                 , "2020-09-01"                                       , "stated"               , "PNADC 2019"   , 2019L       ,
  "Datafolha and IBOPE Inteligência; Kantar IBOPE Media (base 2019)"                    ,
  NA_integer_                                                                           , "Datafolha and IBOPE Inteligência"                 , 2019L                  ,
  "Kantar IBOPE Media (base 2019)"                                                      ,
  "The PDF footer identifies 2019; the source catalog treats this as the 2020 edition." ,
  2021L                                                                                 , "2021-06-01"                                       , "stated"               , "PNADC 2020"   , 2020L       ,
  "IBOPE Inteligência and Kantar IBOPE Media (base 2020; pandemic-adapted)"             ,
  2020L                                                                                 , "IBOPE Inteligência (base 2020; pandemic-adapted)" , 2020L                  ,
  "Kantar IBOPE Media (base 2020; pandemic-adapted)"                                    ,
  "The source describes adaptations to the 2020 collection because of the pandemic."    ,
  2022L                                                                                 , "2022-06-01"                                       , "stated"               , "PNADC 2021"   , 2021L       ,
  "Datafolha and Ipec (2021); Kantar IBOPE Media LSE (2021)"                            ,
  2021L                                                                                 , "Datafolha and Ipec (2021)"                        , 2021L                  ,
  "Kantar IBOPE Media LSE (2021)"                                                       ,
  NA_character_                                                                         ,
  2024L                                                                                 , "2024-06-27"                                       , "stated"               , "PNADC 2023"   , 2023L       ,
  "Datafolha and IPEC (2023); Kantar IBOPE Media LSE (2023)"                            ,
  2023L                                                                                 , "Datafolha and IPEC (2023)"                        , 2023L                  ,
  "Kantar IBOPE Media LSE (2023)"                                                       ,
  NA_character_
)

.cceb_2015_edition_spec <- function(edition_id) {
  result <- .cceb_2015_edition_specs[
    .cceb_2015_edition_specs$edition_id == as.integer(edition_id),
    ,
    drop = FALSE
  ]
  if (nrow(result) != 1L) {
    cli::cli_abort("Unknown 2015-regime edition {.val {edition_id}}.")
  }

  return(result)
}

.tidy_2015_points_block <- function(
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

  return(result)
}

.tidy_2015_education_points <- function(raw_block, specs, edition_id) {
  levels <- c(
    "no_schooling_or_incomplete_elementary_school",
    "elementary_school_diploma_or_incomplete_middle_school",
    "middle_school_diploma_or_incomplete_high_school",
    "high_school_diploma_or_incomplete_higher_education",
    "higher_education_degree"
  )
  result <- purrr::map_dfr(seq_len(nrow(specs)), function(index) {
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

  return(result)
}

tidy_2015_points <- function(raw_points, edition_id) {
  count <- .tidy_2015_points_block(
    raw_points$count,
    .cceb_2015_point_specs$count,
    "count_item",
    c("0", "1", "2", "3", "4_plus"),
    edition_id
  )
  education <- .tidy_2015_education_points(
    raw_points$education,
    .cceb_2015_point_specs$education,
    edition_id
  )
  public_service <- .tidy_2015_points_block(
    raw_points$public_service,
    .cceb_2015_point_specs$public_service,
    "public_service",
    c("does_not_have", "has"),
    edition_id
  )

  result <- dplyr::bind_rows(count, education, public_service)

  return(result)
}

tidy_2015_editions <- function(edition_id, links = NULL) {
  spec <- .cceb_2015_edition_spec(edition_id)
  url_pt <- NA_character_
  if (!is.null(links) && nrow(links) > 0L) {
    pt <- links$url[links$lang == "pt"]
    if (length(pt) == 1L) {
      url_pt <- pt
    }
  }

  result <- tibble::tibble(
    edition_id = spec$edition_id,
    effective_date = as.Date(spec$effective_date),
    effective_date_source = spec$effective_date_source,
    regime = "2015",
    income_source = spec$income_source,
    income_ref_year = spec$income_ref_year,
    income_concept = "household",
    distribution_source = spec$distribution_source,
    lang = "pt",
    url_pt = url_pt,
    url_en = NA_character_,
    method_note = spec$method_note
  )

  return(result)
}

tidy_2015_cutoffs <- function(raw_cutoffs, edition_id) {
  result <- raw_cutoffs
  result$edition_id <- edition_id
  result <- result[, c(
    "edition_id",
    "class",
    "class_order",
    "points_min",
    "points_max"
  )]

  return(result)
}

tidy_2015_income <- function(raw_income, edition_id) {
  result <- raw_income
  result$edition_id <- edition_id
  result$income_ref_date <- as.Date(NA)
  result$income_ref_year <- .cceb_2015_edition_spec(edition_id)$income_ref_year
  result$concept <- "household"
  result <- result[, c(
    "edition_id",
    "class",
    "income_mean",
    "income_ref_date",
    "income_ref_year",
    "concept"
  )]

  return(result)
}

.tidy_2015_distribution_group <- function(raw_group, metadata, edition_id) {
  result <- purrr::map_dfr(seq_len(nrow(metadata)), function(index) {
    shares <- purrr::map_dbl(
      raw_group$share,
      \(values) values[[index]]
    )
    tibble::tibble(
      edition_id = edition_id,
      geo_level = metadata$geo_level[[index]],
      geo_code = metadata$geo_code[[index]],
      geo_name = metadata$geo_name[[index]],
      class = raw_group$class,
      share = shares,
      ref_year = metadata$ref_year[[index]],
      ref_source = metadata$ref_source[[index]]
    )
  })

  return(result)
}

tidy_2015_distribution <- function(raw_distribution, edition_id) {
  spec <- .cceb_2015_edition_spec(edition_id)
  region_metadata <- tibble::tribble(
    ~geo_level , ~geo_code , ~geo_name      ,
    "country"  , "BR"      , "Brasil"       ,
    "region"   , "SE"      , "Sudeste"      ,
    "region"   , "S"       , "Sul"          ,
    "region"   , "NE"      , "Nordeste"     ,
    "region"   , "CO"      , "Centro-Oeste" ,
    "region"   , "N"       , "Norte"
  )
  region_metadata$ref_year <- spec$region_ref_year
  region_metadata$ref_source <- spec$region_ref_source

  metro_metadata <- tibble::tribble(
    ~geo_level    , ~geo_code , ~geo_name        ,
    "metro_total" , "9_rms"   , "9 RMs"          ,
    "metro"       , "POA"     , "Porto Alegre"   ,
    "metro"       , "CWB"     , "Curitiba"       ,
    "metro"       , "SP"      , "São Paulo"      ,
    "metro"       , "RJ"      , "Rio de Janeiro" ,
    "metro"       , "BH"      , "Belo Horizonte" ,
    "metro"       , "BSB"     , "Brasília"       ,
    "metro"       , "SSA"     , "Salvador"       ,
    "metro"       , "REC"     , "Recife"         ,
    "metro"       , "FOR"     , "Fortaleza"
  )
  metro_metadata$ref_year <- spec$metro_ref_year
  metro_metadata$ref_source <- spec$metro_ref_source

  result <- dplyr::bind_rows(
    .tidy_2015_distribution_group(
      raw_distribution$region,
      region_metadata,
      edition_id
    ),
    .tidy_2015_distribution_group(
      raw_distribution$metro,
      metro_metadata,
      edition_id
    )
  )

  return(result)
}

tidy_2015 <- function(raw, edition_id, links = NULL) {
  result <- list(
    cceb_editions = tidy_2015_editions(edition_id, links),
    cceb_points = tidy_2015_points(raw$points, edition_id),
    cceb_cutoffs = tidy_2015_cutoffs(raw$cutoffs, edition_id),
    cceb_income = tidy_2015_income(raw$income, edition_id),
    cceb_distribution = tidy_2015_distribution(raw$distribution, edition_id)
  )

  return(result)
}
