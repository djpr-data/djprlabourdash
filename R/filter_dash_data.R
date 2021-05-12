#' @import dplyr


filter_dash_data <- function(series_ids, df = dash_data) {
  df %>%
    dplyr::filter(.data$series_id %in% series_ids) %>%
    tidyr::unnest(cols = .data$data) %>%
    dplyr::mutate_if(is.factor, as.character)
}

get_summ <- function(series_id,
                     col,
                     df = ts_summ
) {

  stopifnot(length(series_id) == 1)
  stopifnot(!missing(col))

  df %>%
    dplyr::filter(.data$series_id == .env$series_id) %>%
    dplyr::pull({{col}})
}
