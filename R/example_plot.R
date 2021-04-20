#' @import ggplot2

example_plot <- function(data = ggplot2::economics,
                         title = "A title",
                         subtitle = "A very long subtitle lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum",
                         caption = "A caption") {
  data %>%
    ggplot(aes(x = date, y = unemploy)) +
    geom_line() +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme_minimal(base_size = 16)
}
