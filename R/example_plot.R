#' @import ggplot2
#' @importFrom rlang .data .env

example_plot <- function(data,
                         title = "A title",
                         subtitle = "A very long subtitle lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum",
                         caption = "A caption") {

  df <- data %>%
    dplyr::filter(.data$series_id %in%
                    find_lfs_series("unemployment rate",
                                    state = "victoria",
                                    sex = c("males", "females")))

  djprshiny::djpr_ts_linechart(df) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)

}
