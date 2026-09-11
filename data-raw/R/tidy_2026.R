.cceb_2026_education_levels <- c(
  "no_schooling",
  "incomplete_elementary_school",
  "elementary_school_diploma",
  "incomplete_high_school",
  "high_school_diploma",
  "incomplete_higher_education",
  "higher_education_degree"
)

.cceb_2026_region_metadata <- tibble::tribble(
  ~geo_level , ~geo_code , ~geo_name      , ~ref_year , ~ref_source                ,
  "country"  , "BR"      , "Brasil"       , 2024L     , "Datafolha and Ipsos-Ipec" ,
  "region"   , "SE"      , "Sudeste"      , 2024L     , "Datafolha and Ipsos-Ipec" ,
  "region"   , "S"       , "Sul"          , 2024L     , "Datafolha and Ipsos-Ipec" ,
  "region"   , "NE"      , "Nordeste"     , 2024L     , "Datafolha and Ipsos-Ipec" ,
  "region"   , "CO"      , "Centro-Oeste" , 2024L     , "Datafolha and Ipsos-Ipec" ,
  "region"   , "N"       , "Norte"        , 2024L     , "Datafolha and Ipsos-Ipec"
)

.cceb_2026_metro_metadata <- tibble::tribble(
  ~geo_level    , ~geo_code , ~geo_name        , ~ref_year , ~ref_source              ,
  "metro_total" , "9_rms"   , "9 RMs"          , 2023L     , "Kantar IBOPE Media LSE" ,
  "metro"       , "POA"     , "Porto Alegre"   , 2023L     , "Kantar IBOPE Media LSE" ,
  "metro"       , "CWB"     , "Curitiba"       , 2023L     , "Kantar IBOPE Media LSE" ,
  "metro"       , "SP"      , "São Paulo"      , 2023L     , "Kantar IBOPE Media LSE" ,
  "metro"       , "RJ"      , "Rio de Janeiro" , 2023L     , "Kantar IBOPE Media LSE" ,
  "metro"       , "BH"      , "Belo Horizonte" , 2023L     , "Kantar IBOPE Media LSE" ,
  "metro"       , "BSB"     , "Brasília"       , 2023L     , "Kantar IBOPE Media LSE" ,
  "metro"       , "SSA"     , "Salvador"       , 2023L     , "Kantar IBOPE Media LSE" ,
  "metro"       , "REC"     , "Recife"         , 2023L     , "Kantar IBOPE Media LSE" ,
  "metro"       , "FOR"     , "Fortaleza"      , 2023L     , "Kantar IBOPE Media LSE"
)

tidy_2026_points <- function(raw_points, edition_id = 2026L) {
  count <- tidy_points_block(
    raw_points$count,
    .cceb_2026_point_specs$count,
    "count_item",
    c("0", "1", "2", "3", "4_plus"),
    edition_id
  )
  binary <- tidy_points_block(
    raw_points$binary,
    .cceb_2026_point_specs$binary,
    "binary_item",
    c("does_not_have", "has"),
    edition_id
  )
  education <- tidy_education_points(
    raw_points$education,
    .cceb_2026_point_specs$education,
    .cceb_2026_education_levels,
    edition_id
  )

  result <- dplyr::bind_rows(count, binary, education)

  return(result)
}

tidy_2026_editions <- function(links = NULL, edition_id = 2026L) {
  url_pt <- .cceb_2026_urls[["pt"]]
  url_en <- .cceb_2026_urls[["en"]]
  if (!is.null(links)) {
    pt <- links$url[links$lang == "pt"]
    en <- links$url[links$lang == "en"]
    if (length(pt) == 1L) {
      url_pt <- pt
    }
    if (length(en) == 1L) {
      url_en <- en
    }
  }

  result <- tibble::tibble(
    edition_id = edition_id,
    effective_date = as.Date("2026-02-05"),
    effective_date_source = "stated",
    regime = "2026",
    income_source = "PNADC 2025",
    income_ref_year = 2025L,
    income_concept = "household",
    distribution_source = paste(
      "Datafolha and Ipsos-Ipec (2024);",
      "Kantar IBOPE Media (2023)"
    ),
    lang = "pt",
    url_pt = url_pt,
    url_en = url_en,
    method_note = paste(
      "Updated using POF 2018 and an Item Response Theory model;",
      "operational rule published by ABEP."
    )
  )

  return(result)
}

tidy_2026_cutoffs <- function(raw_cutoffs, edition_id = 2026L) {
  result <- raw_cutoffs |>
    dplyr::mutate(edition_id = edition_id) |>
    dplyr::select(edition_id, class, class_order, points_min, points_max)

  return(result)
}

tidy_2026_income <- function(raw_income, edition_id = 2026L) {
  result <- raw_income |>
    dplyr::mutate(
      edition_id = edition_id,
      income_ref_date = as.Date(NA),
      income_ref_year = 2025L,
      concept = "household"
    ) |>
    dplyr::select(
      edition_id,
      class,
      income_mean,
      income_ref_date,
      income_ref_year,
      concept
    )

  return(result)
}

# The country and regional estimates come from different source studies, so
# the reference year stays at the row level.
tidy_2026_distribution <- function(raw_distribution, edition_id = 2026L) {
  region <- tidy_distribution_group(
    raw_distribution$region,
    .cceb_2026_region_metadata,
    edition_id
  )
  metro <- tidy_distribution_group(
    raw_distribution$metro,
    .cceb_2026_metro_metadata,
    edition_id
  )
  result <- dplyr::bind_rows(region, metro)

  return(result)
}

tidy_2026 <- function(raw, links = NULL, edition_id = 2026L) {
  result <- list(
    cceb_editions = tidy_2026_editions(links, edition_id),
    cceb_points = tidy_2026_points(raw$points, edition_id),
    cceb_cutoffs = tidy_2026_cutoffs(raw$cutoffs, edition_id),
    cceb_income = tidy_2026_income(raw$income, edition_id),
    cceb_distribution = tidy_2026_distribution(raw$distribution, edition_id)
  )

  return(result)
}
