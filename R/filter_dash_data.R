#' @import dplyr


filter_dash_data <- function(series_ids, df = dash_data) {
  df %>%
    dplyr::filter(.data$series_id %in% series_ids) %>%
    tidyr::unnest(cols = .data$data) %>%
    dplyr::mutate_if(is.factor, as.character)
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
#' dash_data <- load_and_hide()
#'
#' ts_summ <- dash_data %>%
#'   tidyr::unnest(cols = data) %>%
#'   djprshiny::ts_summarise()
#'
#' get_summ("A84601638A", latest_value)
#' }
#'
get_summ <- function(series_ids,
                     item,
                     df = ts_summ) {
  stopifnot(!missing(item))

  df %>%
    dplyr::filter(.data$series_id %in% series_ids) %>%
    dplyr::pull({{ item }})
}
