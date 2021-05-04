filter_dash_data <- function(series_ids, df = dash_data) {
  df %>%
    dplyr::filter(series_id %in% series_ids) %>%
    tidyr::unnest(cols = data)
}
