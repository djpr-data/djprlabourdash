#' Function that takes a dataframe and returns a list of line charts
#'
#' One line chart is returned for each level in the grouping variable. These
#' line charts are intended for use as sparklines in tables.
#'
#' @param df Dataframe containing data to summarise in sparkline form
#' @param group_var Unquoted name of grouping variable in `df`. One line chart
#' will be created for each unique value in `group_var`.
#' @param date_var Unquoted name of date variable in `df` to plot on the
#' x-axis in the line charts. Default is `date`.
#' @param value_var Unquoted name of variable in `df` to map to the y aesthetic
#' of the line charts. Default is `value`.
#' @return A list of line charts (`{ggplot2}` objects).
#' @details All line charts in the list will have the same maximum and minimum
#' dates on the x-axis, defined by the range in the `date_var` in `df`.
#' @examples
#' make_sparklines(df = ggplot2::economics_long, group_var = variable)
#' @export

make_sparklines <- function(df, group_var, date_var = date, value_var = value) {
  if (missing(group_var)) {
    stop("The `group_var` argument to `make_sparkline() must be specified.")
  }

  stopifnot(inherits(df, "data.frame"))

  date_var_vec <- df %>%
    dplyr::pull( {{ date_var}} )

    min_date <- min(date_var_vec)
    max_date <- max(date_var_vec)

    groups <- df %>%
      dplyr::pull( {{ group_var }} ) %>%
      unique()

    cols <- grDevices::colorRampPalette(suppressWarnings(djprtheme::djpr_pal(10)))(length(groups))

    make_group_line <- function(df, group, cols, min_x, max_x ) {
      col <- cols[which(group == groups)]

      df %>%
        dplyr::filter({{group_var}} == .env$group) %>%
        ggplot(aes(x = {{date_var}}, y = {{value_var}}, col = {{group_var}})) +
        geom_line(colour = col) +
        scale_x_date(limits = c(min_x, max_x),
                     expand = expansion(mult = 0)) +
        theme_void() +
        theme(legend.position = "none")
    }

    lapply(groups, make_group_line, df = df,
           cols = cols, min_x = min_date, max_x = max_date
           ) %>%
      stats::setNames(groups)
  }
