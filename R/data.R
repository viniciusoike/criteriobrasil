#' CCEB edition catalog
#'
#' Metadata for the Portuguese CCEB editions currently ingested from the ABEP
#' source PDFs, covering the 2003, 2015, and 2026 regimes.
#'
#' @format A tibble with one row per edition and 12 columns: `edition_id`,
#'   `effective_date`, `effective_date_source`, `regime`, `income_source`,
#'   `income_ref_year`, `income_concept`, `distribution_source`, `lang`,
#'   `url_pt`, `url_en`, and `method_note`.
#' @source <https://abep.org/criterio-brasil/>
#' @name cceb_editions
NULL

#' CCEB point rules
#'
#' The unified point table extracted from the Portuguese CCEB source PDFs. The
#' table contains one row per edition, variable, and response level.
#'
#' @format A tibble with one row per edition, variable, and level, and 8
#'   columns: `edition_id`, `block`,
#'   `variable`, `label_pt`, `label_en`, `level`, `level_order`, and `points`.
#' @source <https://abep.org/criterio-brasil/>
#' @name cceb_points
NULL

#' CCEB class cutoffs
#'
#' Point ranges for the economic classes in each ingested CCEB edition. The
#' number and names of classes vary across historical regimes.
#'
#' @format A tibble with one row per edition and class, and 5 columns:
#'   `edition_id`, `class`, `class_order`, `points_min`, and `points_max`.
#' @source <https://abep.org/criterio-brasil/>
#' @name cceb_cutoffs
NULL

#' CCEB income estimates
#'
#' Monthly mean income estimates published for the ingested classes. Historical
#' editions report family income, while the recent regime reports household
#' income. The 2015 source has no income table. The source PDFs report the
#' survey year in `income_ref_year` but do not specify a month anchor, so
#' `income_ref_date` is missing until that methodological choice is settled.
#' The 2013 income table was transcribed from an image in the source PDF.
#'
#' @format A tibble with up to 16 rows per edition and 6 columns: `edition_id`,
#'   `class`, `income_mean`, `income_ref_date`, `income_ref_year`, and
#'   `concept`.
#' @source <https://abep.org/criterio-brasil/>
#' @name cceb_income
NULL

#' CCEB geographic class distributions
#'
#' Published class shares for Brazil, macro-regions, the total of nine
#' metropolitan regions, and each metropolitan region in each ingested edition.
#' The available geographies and classes vary by source edition.
#'
#' The 2013 distribution was transcribed from an image in the source PDF.
#'
#' @format A tibble with one row per edition, reference year, geography, and
#'   class, and 8 columns: `edition_id`, `geo_level`, `geo_code`, `geo_name`,
#'   `class`, `share`, `ref_year`, and `ref_source`.
#' @source <https://abep.org/criterio-brasil/>
#' @name cceb_distribution
NULL
