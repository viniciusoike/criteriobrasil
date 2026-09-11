validate_2026_data <- function(data, tolerance = 0.025) {
  .validate_single_edition(data, 2026L)
  validate_cceb_data(data, tolerance)

  return(invisible(data))
}
