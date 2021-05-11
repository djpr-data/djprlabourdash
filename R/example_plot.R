#' An example function that creates a plot
#' @import ggplot2
#' @importFrom rlang .data .env
#' @param data a data frame
#' @examples
#' \dontrun{
#' dash_data <- load_dash_data()
#' df <- filter_dash_data(c("A84423242V", "A84423466F"), df = dash_data)
#' example_plot(df)
#' }
#' @noRd
example_plot <- function(data) {
  title <- "A title"
  subtitle <- "A very long subtitle lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum"
  caption <- "A caption"

  djprshiny::djpr_ts_linechart(data,
    col_var = .data$sex
  ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    )
}
