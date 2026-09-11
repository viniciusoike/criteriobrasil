.cceb_2026_point_specs <- list(
  count = tibble::tribble(
    ~label_pt           , ~variable            , ~label_en            ,
    "Automóveis"        , "automobiles"        , "Automobiles"        ,
    "Geladeiras"        , "refrigerators"      , "Refrigerators"      ,
    "Microcomputadores" , "personal_computers" , "Personal computers" ,
    "Banheiros"         , "bathrooms"          , "Bathrooms"
  ),
  binary = tibble::tribble(
    ~label_pt            , ~variable          , ~label_en          ,
    "Lavadora de louças" , "dishwasher"       , "Dishwasher"       ,
    "Lavadora de roupas" , "washing_machine"  , "Washing machine"  ,
    "Micro-ondas"        , "microwave"        , "Microwave oven"   ,
    "Água encanada"      , "piped_water"      , "Piped water"      ,
    "Serviço doméstico"  , "domestic_service" , "Domestic service"
  ),
  education = tibble::tribble(
    ~label_pt                       , ~variable               , ~label_en                      ,
    "Sem Instrução"                 , "householder_education" , "No schooling"                 ,
    "Ensino Fundamental Incompleto" , "householder_education" , "Incomplete elementary school" ,
    "Ensino Fundamental Completo"   , "householder_education" , "Elementary school diploma"    ,
    "Ensino Médio Incompleto"       , "householder_education" , "Incomplete high school"       ,
    "Ensino Médio Completo"         , "householder_education" , "High school diploma"          ,
    "Ensino Superior Incompleto"    , "householder_education" , "Incomplete higher education"  ,
    "Ensino Superior Completo"      , "householder_education" , "Higher education degree"
  )
)

.tidy_points_block <- function(
  raw_block,
  specs,
  block,
  levels,
  edition_id
) {
  result <- purrr::pmap_dfr(
    specs,
    function(label_pt, variable, label_en) {
      index <- match(label_pt, raw_block$label_pt)
      if (is.na(index)) {
        cli::cli_abort("Missing raw point row {.val {label_pt}}.")
      }

      points <- raw_block$points[[index]]
      if (length(points) != length(levels)) {
        cli::cli_abort(
          "Expected {length(levels)} levels for {.val {label_pt}}; found {length(points)}."
        )
      }

      tibble::tibble(
        edition_id = edition_id,
        block = block,
        variable = variable,
        label_pt = label_pt,
        label_en = label_en,
        level = levels,
        level_order = seq_along(levels),
        points = points
      )
    }
  )

  return(result)
}

.tidy_education_points <- function(raw_block, specs, edition_id) {
  index <- match(specs$label_pt, raw_block$label_pt)
  if (anyNA(index)) {
    cli::cli_abort("The education table is missing an expected level.")
  }

  result <- tibble::tibble(
    edition_id = edition_id,
    block = "education",
    variable = "householder_education",
    label_pt = specs$label_pt,
    label_en = specs$label_en,
    level = c(
      "no_schooling",
      "incomplete_elementary_school",
      "elementary_school_diploma",
      "incomplete_high_school",
      "high_school_diploma",
      "incomplete_higher_education",
      "higher_education_degree"
    ),
    level_order = seq_len(nrow(specs)),
    points = purrr::map_int(index, \(i) raw_block$points[[i]][[1]])
  )

  return(result)
}

tidy_2026_points <- function(raw_points, edition_id = 2026L) {
  count <- .tidy_points_block(
    raw_points$count,
    .cceb_2026_point_specs$count,
    "count_item",
    c("0", "1", "2", "3", "4_plus"),
    edition_id
  )
  binary <- .tidy_points_block(
    raw_points$binary,
    .cceb_2026_point_specs$binary,
    "binary_item",
    c("does_not_have", "has"),
    edition_id
  )
  education <- .tidy_education_points(
    raw_points$education,
    .cceb_2026_point_specs$education,
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

tidy_2026_income <- function(raw_income, edition_id = 2026L) {
  result <- raw_income
  result$edition_id <- edition_id
  result$income_ref_date <- as.Date(NA)
  result$income_ref_year <- 2025L
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

.tidy_distribution_group <- function(
  raw_group,
  metadata,
  edition_id
) {
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

tidy_2026_distribution <- function(raw_distribution, edition_id = 2026L) {
  region_metadata <- tibble::tribble(
    ~geo_level , ~geo_code , ~geo_name      , ~ref_year , ~ref_source                ,
    "country"  , "BR"      , "Brasil"       , 2024L     , "Datafolha and Ipsos-Ipec" ,
    "region"   , "SE"      , "Sudeste"      , 2024L     , "Datafolha and Ipsos-Ipec" ,
    "region"   , "S"       , "Sul"          , 2024L     , "Datafolha and Ipsos-Ipec" ,
    "region"   , "NE"      , "Nordeste"     , 2024L     , "Datafolha and Ipsos-Ipec" ,
    "region"   , "CO"      , "Centro-Oeste" , 2024L     , "Datafolha and Ipsos-Ipec" ,
    "region"   , "N"       , "Norte"        , 2024L     , "Datafolha and Ipsos-Ipec"
  )
  metro_metadata <- tibble::tribble(
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

  region <- .tidy_distribution_group(
    raw_distribution$region,
    region_metadata,
    edition_id
  )
  metro <- .tidy_distribution_group(
    raw_distribution$metro,
    metro_metadata,
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
