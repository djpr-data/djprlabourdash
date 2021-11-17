# Functions to create the graphs for the 'Migration' subpage on the dashboard.

viz_gr_migration_test_line <- function(data = filter_dash_data(c(
  "15-24_greater melbourne_employed",
  "25-54_greater melbourne_employed",
  "55+_greater melbourne_employed",
  "15-24_rest of vic._employed",
  "25-54_rest of vic._employed",
  "55+_rest of vic._employed"
),
df = dash_data
) %>%
  dplyr::group_by(.data$series_id) %>%
  dplyr::mutate(value = slider::slide_mean(.data$value, before = 11, complete = TRUE)) %>%
  dplyr::filter(.data$date >= as.Date("2020-01-01"))) {
  data <- data %>%
    dplyr::group_by(.data$age, .data$date) %>%
    dplyr::summarise(value = sum(.data$value))

  # Indexing to Covid start
  data <- data %>%
    dplyr::group_by(.data$age) %>%
    dplyr::mutate(
      value = 100 * ((.data$value /
                        .data$value[.data$date == as.Date("2020-03-01")]) - 1),
      tooltip = paste0(
        .data$state, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

  latest <- data %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(-.data$date)

  latest <- latest %>%
    tidyr::spread(key = .data$age, value = .data$value)

  # draw line graph
  data %>%
    djpr_ts_linechart(
      col_var = .data$age,
      label_num = paste0(round2(.data$value, 1), "%"),
      hline = 0
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(5),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = "Employment for young people fell much faster after the COVID shock than employment for other Victorians",
      subtitle = "Cumulative change in employment for different age groups since March 2020",
      caption = paste0(caption_lfs_det_m(), "Data smoothed using a 12 month rolling average.")
    )
}
