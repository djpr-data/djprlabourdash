#' Create a summary dataframe for use in a table to be displayed
#' Either in a reactable or a static briefing table
#' @param data Dataframe of input data
#' @param dashboard_or_briefing Character; either 'dashboard'
#' (if the table is intended for the dashboard),
#' or 'briefing' (if the table is intended for the briefing tables).
#' @param years_in_sparklines Number of years worth of data to include in
#' sparkline
#' @examples
#' \dontrun{
#' dash_data <- load_dash_data()
#' create_summary_df(data = filter_dash_data(c(
#'   "A84423349V",
#'   "A84423357V",
#'   "A84423356T",
#'   "A84423244X",
#'   "A84423468K",
#'   "pt_emp_vic"
#' )))
#' }
create_summary_df <- function(data,
                              dashboard_or_briefing = "dashboard",
                              years_in_sparklines = 2) {
  startdate <- subtract_years(max(data$date), years_in_sparklines)

  # Drop unneeded columns -----
  summary_df <- data %>%
    dplyr::select(
      .data$date, .data$series_id,
      .data$indicator, .data$value, .data$unit
    )


  # Calculate change over time -----
  summary_df <- summary_df %>%
    dplyr::group_by(.data$indicator, .data$series_id) %>%
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
    dplyr::filter(.data$date >= startdate) %>%
    dplyr::ungroup()

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

  # If a rounded number is -0.0, change to 0.0
  summary_df <- summary_df %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("changein"),
      ~ gsub("-0.0", "0.0", .x)
    ))

  ## Select only the latest changes

  changedf <- summary_df %>%
    dplyr::group_by(.data$indicator, .data$series_id) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(
      .data$date,
      .data$indicator,
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
        .data$changeinmonthpc != "-",
        paste0(.data$changeinmonth, "\n(", .data$changeinmonthpc, ")"),
        .data$changeinmonth
      ),
      changeinyear = ifelse(
        .data$changeinyearpc != "-",
        paste0(.data$changeinyear, "\n(", .data$changeinyearpc, ")"),
        .data$changeinyear
      )
    )

  changedf <- changedf %>%
    dplyr::select(!dplyr::ends_with("pc"))

  if (dashboard_or_briefing == "dashboard") {
    out <- summary_df %>%
      dplyr::group_by(.data$indicator, .data$series_id) %>%
      dplyr::summarise(n = list(list(value = dplyr::c_across("value")))) %>%
      dplyr::left_join(changedf, by = c("indicator", "series_id")) %>%
      dplyr::select(-.data$date)

    # for_reactable is FALSE if the table is intended for a use
    # other than the dashboard, eg. Word-based briefing tables
  } else {
    # Format column names
    dates <- unique(data$date) %>%
      sort()

    latest_date <- max(changedf$date)
    prev_date <- dates[length(dates) - 1]
    prev_year <- dates[length(dates) - 12]

    nice_latest_date <- format(latest_date, "%b %Y")
    nice_prev_date <- format(prev_date, "%b %Y")
    nice_prev_year <- format(prev_year, "%b %Y")

    since_prev_date <- paste0("Since ", nice_prev_date)
    since_prev_year <- paste0("Since ", nice_prev_year)

    out <- changedf %>%
      dplyr::select(-.data$date,
        indicator = .data$indicator,
        {{ nice_latest_date }} := .data$latest_value,
        {{ since_prev_date }} := .data$changeinmonth,
        {{ since_prev_year }} := .data$changeinyear,
        `Since Nov 2014` = .data$changesince14
      )
  }

  stopifnot(nrow(out) == length(unique(data$series_id)))

  # Add sparklines
  # sparklines <- summary_df %>%
  #   dplyr::filter(.data$date >= startdate) %>%
  #   make_sparklines(group_var = indicator)
  #
  # out <- out %>%
  #   mutate(sparklines = sparklines)

  out
}
