# Functions to create  people with disabilities page graphs

Viz_gr_pwd_jobact_sincecovidIndex_line <- function (data = filter_dash_data(c("jobactive_pwd_ballarat",
                                                                          "jobactive_pwd_bendigo",
                                                                          "jobactive_pwd_barwon",
                                                                          "jobactive_pwd_gippsland",
                                                                          "jobactive_pwd_goulburn/murray",
                                                                          "jobactive_pwd_inner metropolitan melbourne",
                                                                          "jobactive_pwd_north eastern melbourne",
                                                                          "jobactive_pwd_north western melbourne",
                                                                          "jobactive_pwd_south coast of victoria",
                                                                          "jobactive_pwd_south eastern melbourne and peninsula",
                                                                          "jobactive_pwd_north western melbourne",
                                                                          "jobactive_pwd_wimmera mallee",
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
                                                                          "jobactive_total_north western melbourne",
                                                                          "jobactive_total_wimmera mallee"),

                                                                       df = dash_data) %>%
                                                 dplyr::filter(date >= as.Date("2019-03-31"))){


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
      jobactive= .data$split_series[, 1],
      indicator = .data$split_series[, 2],
      employment_region = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$series,-.data$jobactive )

  df <- df %>%
    dplyr::group_by(.data$indicator, .data$date) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value) %>%
    dplyr::mutate("Others" = .data$Total - .data$PWD) %>%
    dplyr::rename(`People with disabilities` = PWD) %>%
    dplyr::select(.data$date, .data$Others, .data$`People with disabilities`) %>%
    tidyr::pivot_longer(
      cols = !.data$date,
      names_to = "indicator",
      values_to = "value") %>%
    dplyr::mutate(
      value = 100 *(.data$value
                    / .data$value[.data$date == as.Date("2020-03-31")]),
      tooltip = paste0(
        .data$indicator, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1)
      )
    )
  # titl_df <- df %>%
  # dplyr::group_by(.data$indicator) %>%
  # dplyr::mutate(value =  ((.data$value[date == as.Date(max(.data$date))] -.data$value[date == as.Date("2020-03-01")])))

  latest_date <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::pull(.data$date)
  round2(1)

  latest_values <- df %>%
    dplyr::filter(date == max(.data$date)) %>%
    dplyr::select(-.data$tooltip) %>%
    dplyr::mutate(
      value = round2(.data$value, 1),
      date = format(.data$date, "%B %Y")
    ) %>%
    tidyr::pivot_wider(names_from = .data$indicator, values_from = .data$value)


  title <- dplyr::case_when(
    latest_values$`People with disabilities` > latest_values$Others ~
      paste0("Victoria's people with disabilities jobactive caseload in ", latest_values$date, " was higher than other Victorians "),
    latest_values$`People with disabilities` < latest_values$Others ~
      paste0("Victoria's people with disabilities jobactive caseload in ", latest_values$date, " was lower than other Victorians"),
    latest_values$`People with disabilities`< latest_values$Others ~
      paste0("Victoria's People with disabilities jobactive caseload in ", latest_values$date, " was the same as  other Victorians "),
    TRUE ~ "Jobactive caseload for People with disabilities and other Victorians"
  )


  df %>%
    djpr_ts_linechart(
      col_var = .data$indicator,
      label_num = paste0(round2(.data$value, 1)),
    ) +
    labs(
      title = title,
      subtitle = "People with a disabilities and Others Victorians Jobactive Caseload, Indexed March 2020",
      caption = caption_jobactive()
    )


}
