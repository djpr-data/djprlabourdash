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
      dplyr::mutate("Non-Aboriginal" = .data$Total - .data$Indigenous) %>%
      dplyr::rename(Aboriginal = Indigenous) %>%
      dplyr::select(.data$date, .data$`Non-Aboriginal`, .data$Aboriginal) %>%
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
   latest_values$`Non-Aboriginal` > latest_values$Aboriginal ~
     paste0("Victoria's Non-Aboriginal jobactive Caseload in ", latest_values$date, " was higher than Aboriginal's"),
   latest_values$`Non-Aboriginal` < latest_values$Aboriginal ~
     paste0("Victoria's Non-Aboriginal jobactive Caseloadin", latest_values$date, "was higher than Aboriginal's"),
   latest_values$`Non-Aboriginal` == latest_values$Aboriginal ~
     paste0("Victoria's Non-Aboriginal jobactive Caseload in ", latest_values$date, "was higher than Aboriginal's"),
   TRUE ~ "Jobactive Caseload for Aboriginal and Non-Aboriginal Victorians"
 )


 df %>%
   djpr_ts_linechart(
     col_var = .data$indicator,
     label_num = paste0(round2(.data$value, 1)),
   ) +
   labs(
     title = title,
     subtitle = "Aboriginal and Non-Aboriginal Victorians Jobactive Caseload, Indexed March 2020",
     caption = paste0("Department of Education, Skills and Employment, caseload data" )
   )


}
