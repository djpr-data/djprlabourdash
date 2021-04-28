#' Summarise a data frame containing ABS time series
#' @name ts_summarise
#' @param df Data frame containing time series
#' @return Data frame with the following columns
#' \itemize{
#'   \item {latest_val}{Latest value}
#'   \item {latest_date}{Date of latest value}
#'   \item {latest_month}{Month of latest value}
#'   \item {latest_year}{Year of latest value}
#'   \item {d_latest}{Change between previous and latest period}
#'   \item {d_year}{Change between latest observation and prior year}
#' }
#' @export
ts_summarise <- function(df) {

  # Add some checking here

  df
}
