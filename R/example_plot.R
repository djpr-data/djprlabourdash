#' @import ggplot2
#' @importFrom rlang .data .env

example_plot <- function(data) {

  title <- "A title"
  subtitle <- "A very long subtitle lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum"
  caption <- "A caption"

  djprshiny::djpr_ts_linechart(data) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)

}
