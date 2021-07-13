#' Create a summary dataframe for use in a table to be displayed
#' Either in a reactable or a static briefing table
#' @param data Dataframe of input data
#' @param for_reactable logical; if TRUE, series_id column is retained,
#' data for sparkline is included, column names are not nicely formatted
#' @param years_in_sparkline Number of years worth of data to include in
#' sparkline
#' @param row_var "Quoted" name of column that contains series names to be
#' used as the row names in the output table
#' @param period Period covered by most recent change, such as "month" or
#' "quarter". Used for column heading in the returned DF.
#' @param row_order If `NULL`, table will be sorted in alphabetical order.
#' Otherwise, `row_order` should be a vector of row names in the order in which
#' you want them to appear in the table.
#' @examples
#' create_summary_df(data = filter_dash_data(c(
#'   "A84423349V",
#'   "A84423357V",
#'   "A84423356T",
#'   "A84423244X",
#'   "A84423468K",
#'   "pt_emp_vic"
#' )))
#' @export
create_summary_df <- function(data,
                              for_reactable = TRUE,
                              pretty_names = FALSE,
                              years_in_sparklines = 2,
                              row_var = "indicator",
                              period = "month",
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

  changedf <- changedf %>%
    mutate(
      changeinmonth = ifelse(
        changeinmonthpc != "-",
        paste0(changeinmonth, "\n(", changeinmonthpc, ")"),
        changeinmonth
      ),
      changeinyear = ifelse(
        changeinyearpc != "-",
        paste0(changeinyear, "\n(", changeinyearpc, ")"),
        changeinyear
      )
    )

  changedf <- changedf %>%
    dplyr::select(!dplyr::ends_with("pc"))

  ## Created df in format required for sparkline ----

  # for_reactable is TRUE if the table is to be used on the
  # dashboard
  if (isTRUE(for_reactable)) {
    out <- summary_df %>%
      dplyr::group_by(.data$series) %>%
      dplyr::summarise(n = list(list(value = dplyr::c_across("value")))) %>%
      dplyr::left_join(changedf, by = "series") %>%
      dplyr::select(-.data$date)

    # for_reactable is FALSE if the table is intended for a use
    # other than the dashboard, eg. Word-based briefing tables
  } else {
    dates <- unique(summary_df$date) %>%
      sort()

    latest_date <- max(summary_df$date)
    prev_date <- dates[length(dates) - 1]
    prev_year <- dates[length(dates) - 12]

    nice_latest_date <- format(latest_date, "%b %Y")
    nice_prev_date <- format(prev_date, "%b %Y")
    nice_prev_year <- format(prev_year, "%b %Y")

    since_prev_date <- paste0("Since ", nice_prev_date)
    since_prev_year <- paste0("Since ", nice_prev_year)

    out <- changedf %>%
      dplyr::select(-series_id, -date,
        ` ` = series,
        {{ nice_latest_date }} := latest_value,
        {{ since_prev_date }} := changeinmonth,
        {{ since_prev_year }} := changeinyear,
        `Since Nov 2014` = changesince14
      )
  }

  out
}
