validate_2003_data <- function(data, edition_id = NULL, tolerance = 0.025) {
  .validate_single_edition(data, edition_id)
  validate_cceb_data(data, tolerance)

  return(invisible(data))
}
