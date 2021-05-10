#' @import dplyr


filter_dash_data <- function(series_ids, df = dash_data) {
  df %>%
    dplyr::filter(.data$series_id %in% series_ids) %>%
    tidyr::unnest(cols = .data$data) %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))
}
