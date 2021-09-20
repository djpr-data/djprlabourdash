#' Convenience function to extract specified ABS series IDs from `dash_data`
#' @param series_ids Vector of series ID(s)
#' @param df Data frame, expected to be the df returned by `load_dash_data()`
#' @return A tbl_df containing only the specified series ID(s)
#' @export
#' @examples
#' filter_dash_data("A84423354L")
#'
filter_dash_data <- function(series_ids, df = dash_data) {

  out <- subset(df, series_id %in% series_ids)

  all_present <- all(.env$series_ids %in% out$series_id)

  if (!all_present) {
    warning("Not all series IDs could be found")
  }

  out
}


#' Get a key fact about a time series
#' @param series_ids Character vector of ABS time series ID(s)
#' @param item Item to extract from summary; see `?ts_summarise()`
#' for the full list. Unquoted.
#' @param df Data frame containing time series summarised with
#' `djprshiny::ts_summarise()`.
#'
#' @examples
#' \dontrun{
#' ts_summ <- dash_data %>%
#'   djprshiny::ts_summarise()
#'
#' get_summ("A84601638A", latest_value)
#' }
get_summ <- function(series_ids,
                     item,
                     df = ts_summ) {

  subset(df, series_id %in% series_ids)[[deparse(substitute(item))]]
}
