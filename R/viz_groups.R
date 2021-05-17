#' Function to create the graphs for the 'Groups' subpage on the dashboard.
#' @param data the dataframe containing data to visualise
#' @examples
#' \dontrun{
#'
#' # for viz_gr_gen_emp_bar:
#' ids <- c(
#'   "A84423349V",
#'   "A84423237A",
#'   "A84423461V",
#'   "A84423357V",
#'   "A84423245A",
#'   "A84423469L",
#'   "A84423350C",
#'   "A84423238C",
#'   "A84423462W"
#' )
#' # (also parttime figures and
#' # not in the labour force)
#'
#' # for viz_gr_gen_partrate_line:
#' ids <- c(
#'   "A84423355R",
#'   "A84423243W",
#'   "A84423467J"
#' )
#'
#' # for viz_gr_gen_unemp_line:
#' ids <- c(
#'   "A84423354L",
#'   "A84423242V",
#'   "A84423466F"
#' )
#'
#' # for viz_gr_yth_lfpart_line:
#' # ids <- will need smoothing and extracting data from LM1.
#'
#' # for viz_gr_yth_emp_line:
#' }
viz_gr_gen_emp_bar <- function(data = filter_dash_data(c(
                                 "A84423349V",
                                 "A84423237A",
                                 "A84423461V",
                                 "A84423357V",
                                 "A84423245A",
                                 "A84423469L",
                                 "A84423350C",
                                 "A84423238C",
                                 "A84423462W",
                                 "pt_emp_vic"
                               ), df = dash_data),
                               title = "") {
  # still missing: part time and not in labour force data

  data %>%
    ggplot(aes(x = series, fill = indicator),
      y = value
    ) +
    geom_bar(stat = "count") +
    coord_flip()


  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    breaks = seq(0, 10, 2),
    labels = function(x) paste0(x, "%")
  ) +
    djprtheme::theme_djpr(flipped = TRUE) +
    labs(
      title = "",
      subtitle = "Unemployment rate (%) in Victorian regions",
      caption = "Source: ABS Labour Force."
    )
}

viz_gr_gen_partrate_line <- function(data = filter_dash_data(c(
                                       "A84423355R",
                                       "A84423243W",
                                       "A84423467J"
                                     ),
                                     df = dash_data
                                     ),
                                     title = "") {
  data %>%
    djpr_ts_linechart() +
    labs(
      title = title,
      subtitle = "Participation rate by sex, Victoria",
      caption = "Source: ABS Labour Force. Note: seasonally adjusted data."
    )
}

viz_gr_gen_unemp_line <- function(data = filter_dash_data(c(
                                    "A84423354L",
                                    "A84423242V",
                                    "A84423466F"
                                  ),
                                  df = dash_data
                                  ), title = "") {
  data %>%
    djpr_ts_linechart() +
    labs(
      title = title,
      subtitle = "Unemployment by sex, Victoria",
      caption = "Source: ABS Labour Force. Note: seasonally adjusted data."
    )
}
