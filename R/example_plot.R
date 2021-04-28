#' @import ggplot2
#' @importFrom rlang .data .env

example_plot <- function(data = ggplot2::economics,
                         title = "A title",
                         subtitle = "A very long subtitle lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum",
                         caption = "A caption") {
  data %>%
    ggplot(aes(x = .data$date, y = .data$unemploy)) +
    geom_line() +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    djprtheme::theme_djpr(base_size = 16) +
    theme(axis.title.x = element_blank())
}
