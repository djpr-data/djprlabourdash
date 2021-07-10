
create_summary_df <- function(data,
                              for_reactable = TRUE,
                              pretty_names = FALSE,
                              years_in_sparklines = 2,
                              row_var = "indicator",
                              row_order = NULL) {

  startdate <- subtract_years(max(data$date), years_in_sparklines)

  # Drop unneeded columns -----
  summary_df <- data %>%
    dplyr::select(.data$date, .data$series_id,
                  series = .env$row_var, .data$value, .data$unit
    )

  # Tweak series names ----
  summary_df <- summary_df %>%
    dplyr::mutate(series = dplyr::case_when(
      .data$series_id == "A84423349V" ~
        "Employed (persons)",
      .data$series_id == "A84423237A" ~
        "Employed (males)",
      .data$series_id == "A84423461V" ~
        "Employed (females)",
      .data$series_id == "A85223450L" ~
        "Underemployment rate",
      .data$series_id == "A84423354L" ~
        "Unemployment rate",
      .data$series_id == "A84433601W" ~
        "Youth unemployment rate",
      TRUE ~ .data$series
    ))

  # Calculate change over time -----
  summary_df <- summary_df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(
      is_level = if_else(grepl("000", .data$unit), TRUE, FALSE),
      value = dplyr::if_else(
        .data$is_level,
        1000 * .data$value,
        .data$value
      ),
      changeinmonth = (.data$value - dplyr::lag(.data$value)),
      changeinmonthpc = .data$changeinmonth / dplyr::lag(.data$value) * 100,
      changeinyear = (.data$value - dplyr::lag(.data$value, 12)),
      changeinyearpc = .data$changeinyear / dplyr::lag(.data$value, 12) * 100,
      changesince14 = (.data$value - .data$value[.data$date == as.Date("2014-11-01")])
    ) %>%
    dplyr::filter(.data$date >= startdate)

  # Reformat columns -----

  summary_df <- summary_df %>%
    dplyr::mutate(across(
      c(dplyr::ends_with("pc")),
      ~ dplyr::if_else(.data$is_level,
                       paste0(round2(.x, 1), "%"),
                       "-"
      )
    ),
    across(
      c(.data$changeinmonth, .data$changeinyear, .data$changesince14),
      ~ dplyr::if_else(.data$is_level,
                       pretty_round(.x),
                       sprintf("%.1f ppts", .x)
      )
    ),
    latest_value = dplyr::if_else(
      .data$is_level,
      pretty_round(.data$value),
      sprintf("%.1f%%", .data$value)
    )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$unit, .data$is_level)

  # If a number is -0.0, change to 0.0
  summary_df <- summary_df %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("changein"),
      ~ gsub("-0.0", "0.0", .x)
    ))

  ## Select only the latest changes

  changedf <- summary_df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(
      .data$date,
      .data$series,
      .data$series_id,
      .data$latest_value,
      .data$changeinmonth,
      .data$changeinmonthpc,
      .data$changeinyear,
      .data$changeinyearpc,
      .data$changesince14
    ) %>%
    dplyr::ungroup()

  ## Created df in format required for sparkline ----

  if (isTRUE(for_reactable)) {
    out <- summary_df %>%
      dplyr::group_by(.data$series) %>%
      dplyr::summarise(n = list(list(value = dplyr::c_across("value")))) %>%
      dplyr::left_join(changedf, by = "series") %>%
      dplyr::select(-.data$date)
  } else {
    nice_date <- format(unique(changedf$date), "%b %Y")

    out <- changedf %>%
      dplyr::select(-series_id, -date,
                    Indicator = series,
                    {{nice_date}} := latest_value,
                    `Change in month` = changeinmonth,
                    `Change in month (%)` = changeinmonthpc,
                    `Change in year` = changeinyear,
                    `Change in year (%)` = changeinyearpc,
                    `Change since Nov 2014` = changesince14)
  }

  out

}
