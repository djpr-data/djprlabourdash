# Functions to create the graphs for the 'Migration' subpage on the dashboard.

viz_gr_refugee_jobact_sincecovid_line <- function(data = filter_dash_data(c(
                                                    "jobactive_refugee_ballarat",
                                                    "jobactive_refugee_bendigo",
                                                    "jobactive_refugee_barwon",
                                                    "jobactive_refugee_gippsland",
                                                    "jobactive_refugee_goulburn/murray",
                                                    "jobactive_refugee_inner metropolitan melbourne",
                                                    "jobactive_refugee_north eastern melbourne",
                                                    "jobactive_refugee_north western melbourne",
                                                    "jobactive_refugee_south coast of victoria",
                                                    "jobactive_refugee_south eastern melbourne and peninsula",
                                                    "jobactive_refugee_western melbourne",
                                                    "jobactive_refugee_wimmera mallee",
                                                    "jobactive_total_ballarat",
                                                    "jobactive_total_bendigo",
                                                    "jobactive_total_barwon",
                                                    "jobactive_total_gippsland",
                                                    "jobactive_total_goulburn/murray",
                                                    "jobactive_total_inner metropolitan melbourne",
                                                    "jobactive_total_north eastern melbourne",
                                                    "jobactive_total_north western melbourne",
                                                    "jobactive_total_south coast of victoria",
                                                    "jobactive_total_south eastern melbourne and peninsula",
                                                    "jobactive_total_western melbourne",
                                                    "jobactive_total_wimmera mallee"
                                                  ),
                                                  df = dash_data
                                                  ) %>%
                                                    dplyr::filter(date >= as.Date("2019-03-31"))) {
  df <- data %>%
    dplyr::select(
      .data$date, .data$series,
      .data$frequency, .data$value
    ) %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      indicator = .data$split_series[, 2],
      employment_region = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$series, -.data$jobactive)

  df <- df %>%
    dplyr::group_by(.data$indicator, .data$date) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value
    ) %>%
    dplyr::mutate("Others" = .data$Total - .data$Refugee) %>%
    dplyr::select(.data$date, .data$Others, .data$Refugee) %>%
    tidyr::pivot_longer(
      cols = !.data$date,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      value = 100 * (.data$value
        / .data$value[.data$date == as.Date("2020-03-31")] - 1),
      tooltip = paste0(
        .data$indicator, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )


  latest_refugee <- df %>%
    dplyr::filter(
      .data$indicator == "Refugee",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  title <- paste0(
    "Refugee Victorians jobactive caseload is ",
    dplyr::case_when(
      latest_refugee > 0 ~ paste0(abs(latest_refugee), " per cent higher than "),
      latest_refugee == 0 ~ "the same as ",
      latest_refugee < 0 ~ paste0(abs(latest_refugee), " per cent lower than ")
    ),
    "it was in March 2020"
  )


  df %>%
    djpr_ts_linechart(
      col_var = .data$indicator,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = title,
      subtitle = "Cumulative change in jobactive caseload since March 2020",
      caption = caption_jobactive()
    )
}


viz_gr_refugee_jobactive_bar <- function(data = filter_dash_data(c(
                                           "jobactive_refugee_ballarat",
                                           "jobactive_refugee_bendigo",
                                           "jobactive_refugee_barwon",
                                           "jobactive_refugee_gippsland",
                                           "jobactive_refugee_goulburn/murray",
                                           "jobactive_refugee_inner metropolitan melbourne",
                                           "jobactive_refugee_north eastern melbourne",
                                           "jobactive_refugee_north western melbourne",
                                           "jobactive_refugee_south coast of victoria",
                                           "jobactive_refugee_south eastern melbourne and peninsula",
                                           "jobactive_refugee_western melbourne",
                                           "jobactive_refugee_wimmera mallee"
                                         ),
                                         df = dash_data
                                         )) {
  df <- data %>%
    dplyr::select(
      .data$date, .data$series,
      .data$unit, .data$value
    ) %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      indicator = .data$split_series[, 2],
      region = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$series, -.data$jobactive, -.data$indicator) %>%
    dplyr::mutate(
      value = .data$value * 1000
    )

  # reduce to only latest month
  df <- df %>%
    dplyr::group_by(.data$region, ) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  # value = scales::comma(.data$value * 1000),
  high_low <- df %>%
    summarise(
      min_region = .data$region[.data$value == min(.data$value)],
      min_caseload = .data$value[.data$value == min(.data$value)],
      max_region = .data$region[.data$value == max(.data$value)],
      max_caseload = .data$value[.data$value == max(.data$value)],
      date = max(.data$date)
    )

  title <- paste0(
    "The number of refugees receiving jobactive assistance ranged from ",
    scales::comma(round2(high_low$min_caseload, 1),
      accuracy = 1
    ),
    " in ",
    high_low$min_region,
    " to ",
    scales::comma(round2(high_low$max_caseload, 1),
      accuracy = 1
    ),
    " in ",
    high_low$ max_region,
    " as at ",
    format(high_low$date, "%B %Y")
  )

  # draw bar chart for all employment regions
  df %>%
    dplyr::mutate(
      region = gsub(
        "South Eastern Melbourne",
        "SE Melbourne",
        .data$region
      )
    ) %>%
    ggplot(aes(
      x = stats::reorder(.data$region, .data$value),
      y = .data$value
    )) +
    geom_col(
      col = "grey85",
      aes(fill = -.data$value)
    ) +
    geom_text(
      nudge_y = 0.1,
      aes(label = paste0(
        scales::comma(round2(.data$value, 1),
          accuracy = 1
        )
      )),
      colour = "black",
      hjust = 0,
      size = 12 / .pt
    ) +
    coord_flip(clip = "off") +
    scale_fill_distiller(palette = "Blues") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    djprtheme::theme_djpr(flipped = TRUE) +
    theme(
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank()
    ) +
    labs(
      title = title,
      subtitle = paste0(
        "Jobactive caseload for refugees in Victoria by employment region, ",
        format(max(data$date), "%B %Y")
      ),
      caption = caption_jobactive()
    )
}
