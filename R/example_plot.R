#' @import ggplot2
#' @importFrom rlang .data .env

example_plot <- function(data = ggplot2::economics_long %>%
                           dplyr::filter(.data$variable == "unemploy") %>%
                           dplyr::rename(series = .data$variable),
                         title = "A title",
                         subtitle = "A very long subtitle lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum",
                         caption = "A caption") {

  djprshiny::djpr_ts_linechart(df = data) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)

}
