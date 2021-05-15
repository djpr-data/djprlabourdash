
#' Line chart of cumulative employment change since March 2020
#' in Victoria and Australia
#' @noRd
#' @examples
#' \dontrun{
#' dash_data <- load_dash_data()
#' viz_ind_emp_sincecovid_line()
#' }
#'
title_ind_emp_sincecovid_line <- function(data = filter_dash_data(c("A84423043C", "A84423349V"),
                                            df = dash_data
                                          )) {
  df <- data %>%
    dplyr::filter(.data$date >= as.Date("2020-03-01")) %>%
    dplyr::mutate(state = dplyr::if_else(.data$state == "", "Australia", .data$state)) %>%
    dplyr::group_by(.data$state, .data$series) %>%
    dplyr::mutate(value = 100 * ((value / value[date == as.Date("2020-03-01")]) - 1)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  latest <- df %>%
    dplyr::select(value, state) %>%
    tidyr::spread(key = state, value = value)

  if (latest$Victoria < 0) {
    title <- "Victorian employment is below pre-COVID levels"
  } else {
    if (round(latest$Victoria, 1) < round(latest$Australia)) {
      title <- "Victorian employment is above pre-COVID levels, but growth lags behind the national total"
    } else if (round(latest$Victoria, 1) == round(latest$Australia, 1)) {
      title <- "Victorian employment is above pre-COVID levels, and has caught up with the national employment growth"
    } else {
      title <- "Victorian employment is above pre-COVID levels, and has grown faster than the national total"
    }
  }
  return(title)
}

viz_ind_emp_sincecovid_line <- function(data = filter_dash_data(c("A84423043C", "A84423349V"),
                                          df = dash_data
                                        ),
                                        title = title_ind_emp_sincecovid_line(data)) {
  df <- data %>%
    dplyr::mutate(state = dplyr::if_else(state == "", "Australia", state))

  df <- df %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(value = 100 * ((value / value[date == as.Date("2020-03-01")]) - 1))

  max_date <- df %>%
    dplyr::filter(date == max(.data$date))

  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      state,
      "\n",
      format(
        .data$date,
        "%b %Y"
      ),
      "\n",
      round(.data$value, 1)
    ))

  days_in_data <- as.numeric(max(df$date) - min(df$date))

  lab_df <- max_date %>%
    dplyr::mutate(label = paste0(
      stringr::str_wrap(state, 10),
      "\n",
      stringr::str_wrap(round(.data$value, 1), 10)
    ))

  df %>%
    ggplot(aes(
      x = .data$date,
      y = .data$value,
      col = .data$state
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    scale_colour_discrete(palette = djprtheme::djpr_pal) +
    geom_point(
      data = max_date,
      fill = "white",
      stroke = 1.5,
      size = 2.5,
      shape = 21
    ) +
    ggrepel::geom_label_repel(
      data = lab_df,
      aes(label = label),
      hjust = 0,
      fontface = "bold",
      nudge_x = days_in_data * 0.033,
      label.padding = 0.01,
      label.size = NA,
      lineheight = 0.9,
      point.padding = unit(0, "lines"),
      direction = "y",
      seed = 123,
      show.legend = FALSE,
      min.segment.length = unit(5, "lines"),
      size = 14 / .pt
    ) +
    ggiraph::geom_point_interactive(aes(tooltip = .data$tooltip),
      size = 3,
      colour = "white",
      alpha = 0.01
    ) +
    scale_x_date(
      expand = expansion( # mult = c(0, 0.08)
        add = c(0, days_in_data * 0.18)
      ),
      date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.1),
      labels = function(x) paste0(x, "%")
    ) +
    djprtheme::theme_djpr() +
    theme(axis.title.x = element_blank()) +
    coord_cartesian(clip = "off") +
    labs(
      title = title,
      subtitle = "Cumulative employment growth since March 2020",
      caption = paste0("Source: ABS Labour Force and DJPR calculations. Note: Seasonally adjusted.")
    )
}
