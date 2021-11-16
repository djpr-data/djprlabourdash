# Functions to create Aboriginal page graphs

viz_gr_abor_jobact_sincecovid_line <- function (data = filter_dash_data(c("jobactive_indigenous_ballarat",
                                                                          "jobactive_indigenous_bendigo",
                                                                          "jobactive_indigenous_barwon",
                                                                          "jobactive_indigenous_gippsland",
                                                                          "jobactive_indigenous_goulburn/murray",
                                                                          "jobactive_indigenous_inner metropolitan melbourne",
                                                                          "jobactive_indigenous_north eastern melbourne",
                                                                          "jobactive_indigenous_north western melbourne",
                                                                          "jobactive_indigenous_south coast of victoria",
                                                                          "jobactive_indigenous_south eastern melbourne and peninsula",
                                                                          "jobactive_indigenous_north western melbourne",
                                                                          "jobactive_indigenous_wimmera mallee",
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
                                                                                                  dplyr::filter(date >= as.Date("2019-12-31"))){


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

 df_Vic <- df %>%
      dplyr::group_by(.data$indicator, .data$date) %>%
      dplyr::summarise(value = sum(.data$value)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value) %>%
      dplyr::mutate(non_indiginous = .data$Total - .data$Indigenous) %>%
      dplyr::select(.data$date, .data$non_indiginous, .data$Indigenous) %>%
      tidyr::pivot_longer(
                    cols = !.data$date,
                    names_to = "Indicator",
                    values_to = "value") %>%
                    dplyr::mutate(
                      value = 100 *(.data$value
                                      / .data$value[.data$date == as.Date("2020-03-31")]),
                      tooltip = paste0(
                        .data$state, "\n",
                        format(.data$date, "%b %Y"), "\n",
                        round2(.data$value, 1)
                      )
                    )






}
