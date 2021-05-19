# Function to avoid adding a lubridate dependency
#' Subtract a number of years from a date
#' @param date Date from which to subtract `n_years`
#' @param n_years Number of years to subtract. Will be coerced to integer -
#' cannot subtract fractions of a year.
#'
#' @examples
#' subtract_years(Sys.Date(), 5)
#' subtract_years(as.Date("2020-01-01"), 1)
#' \dontrun{
#' # This returns an error:
#' subtract_years(Sys.Date(), 1.5)
#' }
#' @export
subtract_years <- function(date, n_years) {
  # Part years cannot be subtracted (eg. 0.5, 1.5).
  # If the user supplies a non-integer, or a number that cannot be coerced
  # to integer without rounding/truncation, stop with error
  stopifnot(as.integer(n_years) == n_years)

  seq(date,
    length = 2,
    by = paste0("-", n_years, " years")
  )[2]
}
