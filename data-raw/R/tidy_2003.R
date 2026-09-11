# One row per legacy edition, describing how its published tables are laid
# out and which studies they come from.
.cceb_2003_edition_specs <- tibble::tribble(
  ~edition_id , ~effective_date , ~effective_date_source , ~income_source         , ~income_ref_year ,
  2003L       , "2003-01-01"    , "stated"               , "LSE 2000"             , 2000L            ,
  2008L       , "2008-01-01"    , "stated"               , "LSE 2005"             , 2005L            ,
  2009L       , "2009-01-01"    , "stated"               , "LSE 2006/2007"        , NA_integer_      ,
  2010L       , "2010-01-01"    , "stated"               , "LSE 2008"             , 2008L            ,
  2011L       , "2011-01-01"    , "inferred"             , "LSE 2009"             , 2009L            ,
  2012L       , "2012-01-01"    , "stated"               , "LSE 2010"             , 2010L            ,
  2013L       , "2013-01-01"    , "stated"               , "IBOPE Mídia LSE 2011" , 2011L            ,
  2014L       , "2014-01-01"    , "stated"               , "LSE 2012"             , 2012L
)

.cceb_2003_table_specs <- tibble::tribble(
  ~edition_id , ~distribution_type , ~distribution_ref_year , ~distribution_ref_source , ~distribution_source   ,
  2003L       , "country_first"    , 2000L                  , "IBOPE LSE 2000"         , "IBOPE LSE 2000"       ,
  2008L       , "country_first"    , 2005L                  , "IBOPE LSE 2005"         , "IBOPE LSE 2005"       ,
  2009L       , "country_first"    , 2006L                  , "IBOPE LSE 2006"         , "IBOPE LSE 2006/2007"  ,
  2010L       , "country_first"    , 2008L                  , "IBOPE LSE 2008"         , "IBOPE LSE 2008"       ,
  2011L       , "metro_first"      , 2009L                  , "IBOPE LSE 2009"         , "IBOPE LSE 2009"       ,
  2012L       , "metro_total_last" , 2010L                  , "IBOPE LSE 2010"         , "IBOPE LSE 2010"       ,
  2013L       , "manual_image"     , 2011L                  , "IBOPE Mídia LSE 2011"   , "IBOPE Mídia LSE 2011" ,
  2014L       , "metro_total_last" , 2012L                  , "IBOPE Media LSE 2012"   , "IBOPE Media LSE 2012"
)

.cceb_2003_method_notes <- tibble::tibble(
  edition_id = .cceb_2003_ids,
  method_note = c(
    "The old regime has seven classes and a 34-point maximum.",
    NA_character_,
    paste(
      "The source reports income for both 2006 and 2007;",
      "the row-level income_ref_year preserves both columns."
    ),
    "The filename identifies this document as in force in 2010.",
    "The effective date is inferred from the 2011 publication footer.",
    NA_character_,
    paste(
      "The income and distribution tables are embedded as images;",
      "their values are transcribed from pages 4 and 5."
    ),
    paste(
      "Point cuts retain D and E, while the published distribution and",
      "income tables combine them as DE and A1/A2 income as A."
    )
  )
)

# Published class sets drift from the point rules: from 2013 the income table
# reports a merged A class and the distribution merges D and E.
.cceb_2003_class_sets <- list(
  seven = c("A1", "A2", "B1", "B2", "C", "D", "E"),
  eight = c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E"),
  merged_de = c("A1", "A2", "B1", "B2", "C1", "C2", "DE"),
  merged_a_de = c("A", "B1", "B2", "C1", "C2", "DE")
)

.cceb_2003_class_specs <- tibble::tibble(
  edition_id = .cceb_2003_ids,
  income_class_set = c(
    "seven",
    "eight",
    "eight",
    "eight",
    "eight",
    "eight",
    "merged_a_de",
    "merged_a_de"
  ),
  distribution_class_set = c(
    "seven",
    "eight",
    "eight",
    "eight",
    "eight",
    "eight",
    "merged_de",
    "merged_de"
  ),
  income_ref_years = list(
    2000L,
    2005L,
    c(2006L, 2007L),
    2008L,
    2009L,
    2010L,
    2011L,
    2012L
  )
)

.cceb_2003_specs <- dplyr::left_join(
  .cceb_2003_edition_specs,
  .cceb_2003_table_specs,
  by = "edition_id"
)
.cceb_2003_specs <- dplyr::left_join(
  .cceb_2003_specs,
  .cceb_2003_method_notes,
  by = "edition_id"
)
.cceb_2003_specs <- dplyr::left_join(
  .cceb_2003_specs,
  .cceb_2003_class_specs,
  by = "edition_id"
)

.cceb_2003_education_levels <- c(
  "no_schooling_or_incomplete_elementary_school",
  "elementary_school_diploma_or_incomplete_middle_school",
  "middle_school_diploma_or_incomplete_high_school",
  "high_school_diploma_or_incomplete_higher_education",
  "higher_education_degree"
)

.cceb_2003_country_first_metadata <- tibble::tribble(
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

.cceb_2003_metro_first_metadata <- tibble::tribble(
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

# Returns the edition row as a list, with the class sets resolved so that
# callers read `spec$income_classes` as a plain vector.
.cceb_2003_edition_spec <- function(edition) {
  spec <- dplyr::filter(.cceb_2003_specs, edition_id == edition)
  if (nrow(spec) != 1L) {
    cli::cli_abort("Unknown legacy edition {.val {edition}}.")
  }

  result <- as.list(spec)
  result$income_ref_years <- result$income_ref_years[[1]]
  result$income_classes <- .cceb_2003_class_sets[[spec$income_class_set]]
  result$distribution_classes <-
    .cceb_2003_class_sets[[spec$distribution_class_set]]

  return(result)
}

tidy_2003_points <- function(raw_points, edition_id) {
  specs <- .cceb_legacy_point_specs(edition_id)
  count <- tidy_points_block(
    raw_points$count,
    specs$count,
    "count_item",
    c("0", "1", "2", "3", "4_plus"),
    edition_id
  )
  education <- tidy_education_points(
    raw_points$education,
    specs$education,
    .cceb_2003_education_levels,
    edition_id
  )

  result <- dplyr::bind_rows(count, education)

  return(result)
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

  result <- tibble::tibble(
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

  return(result)
}

tidy_2003_cutoffs <- function(raw_cutoffs, edition_id) {
  result <- dplyr::mutate(raw_cutoffs, edition_id = edition_id)
  result <- dplyr::select(
    result,
    edition_id,
    class,
    class_order,
    points_min,
    points_max
  )

  return(result)
}

tidy_2003_income <- function(raw_income, edition_id) {
  result <- dplyr::mutate(
    raw_income,
    edition_id = edition_id,
    income_ref_date = as.Date(NA),
    concept = "family"
  )
  result <- dplyr::select(
    result,
    edition_id,
    class,
    income_mean,
    income_ref_date,
    income_ref_year,
    concept
  )

  return(result)
}

.legacy_distribution_metadata <- function(distribution_type, spec) {
  if (distribution_type == "country_first") {
    metadata <- .cceb_2003_country_first_metadata
  } else {
    metadata <- .cceb_2003_metro_first_metadata
  }
  result <- dplyr::mutate(
    metadata,
    ref_year = spec$distribution_ref_year,
    ref_source = spec$distribution_ref_source
  )

  return(result)
}

tidy_2003_distribution <- function(raw_distribution, edition_id) {
  spec <- .cceb_2003_edition_spec(edition_id)
  if (nrow(raw_distribution$table) == 0L) {
    return(empty_cceb_distribution())
  }

  metadata <- .legacy_distribution_metadata(
    raw_distribution$distribution_type,
    spec
  )
  # The 2009 edition publishes one table per reference year.
  ref_years <- unique(raw_distribution$table$ref_year)
  tables <- purrr::map(ref_years, function(year) {
    raw_group <- dplyr::filter(raw_distribution$table, ref_year == year)
    year_metadata <- dplyr::mutate(metadata, ref_year = year)

    tidy_distribution_group(raw_group, year_metadata, edition_id)
  })

  return(purrr::list_rbind(tables))
}

tidy_2003 <- function(raw, edition_id, links = NULL) {
  result <- list(
    cceb_editions = tidy_2003_editions(edition_id, links),
    cceb_points = tidy_2003_points(raw$points, edition_id),
    cceb_cutoffs = tidy_2003_cutoffs(raw$cutoffs, edition_id),
    cceb_income = tidy_2003_income(raw$income, edition_id),
    cceb_distribution = tidy_2003_distribution(raw$distribution, edition_id)
  )

  return(result)
}
