
#' Get dash data table
#'
#' By default, this will re-download data from the `djprdashdata` repo if
#' the current dashboard's data is out of date.
#' @param refresh Refresh behaviour. Possible values:
#' - `auto`: will re-download data if it is out of date
#' - `always`: will always re-download data
#' - `never`: will never re-download data
#' @param branch The GitHub branch to download data from
#' @param verbose Whether to print verbose messages
#' @return A table with dashboard data.
#'
#' If `refresh` was `auto` or `always`, `attr(*, "date_updated")` will contain
#' a timestamp of when the data was last updated.
#' get_dash_data(refresh = "never") # Just return internally cached data
get_dash_data <- function(refresh = c("auto", "always", "never"), branch = "main", verbose = F) {
  refresh <- match.arg(refresh)

  if(refresh == "never" && exists("dash_data")) {
    return(dash_data)
  }

  remote_updated <- readLines(
    "https://github.com/djpr-data/djprdashdata/raw/main/data-raw/last_updated.txt"
  )

  should_refresh <-
    (refresh == "always") ||
    (!exists("dash_data_updated")) ||
    (dash_data_updated != remote_updated)

  if(should_refresh) {
    if(verbose) {
      message("Re-downloading dash data")
    }
    dash_data <- load_dash_data(branch)
  } else {
    if(verbose) {
      message("Re-using loaded dash data")
    }
    # Copy global variable
    dash_data <- dash_data
  }
  attr(dash_data, "date_updated") <- remote_updated
  dash_data
}
