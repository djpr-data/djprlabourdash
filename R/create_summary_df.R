#' Create a summary dataframe for use in a table
#' @param data Dataframe of input data
#' @param years_in_sparklines Number of years worth of data to include in
#' sparkline
#' @param pretty_round Logical; `TRUE` by default. If `TRUE`, figures will be
#' rounded using `pretty_round()`. This means (for eg.) figures below 50
#' will be rounded to 0. If `FALSE`, figures will not be rounded with
#' `pretty_round()`.
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
                              years_in_sparklines = 3,
                              pretty_round = TRUE) {

  startdate <- subtract_years(max(data$date), years_in_sparklines)

  freq <- unique(data$frequency)
  if (length(freq) != 1) {
    stop("Cannot make a table with mixed frequency data (eg. monthly + quarterly")
  }

  num_in_year <- dplyr::case_when(
    freq == "Month" ~ 12,
    freq == "Quarter" ~ 4,
    TRUE ~ NA_real_
  )

  if (is.na(num_in_year)) {
    stop("Cannot make a table with data that is neither monthly or quarterly")
  }

  # Drop unneeded columns -----
  summary_df <- data %>%
    dplyr::select(
      .data$date, .data$series_id,
      .data$indicator, .data$value, .data$unit,
      .data$frequency
    )

  # Calculate change over time -----
  summary_df <- summary_df[order(summary_df$date), ]

  summary_df <- summary_df %>%
    dplyr::group_by(.data$indicator, .data$series_id) %>%
    dplyr::filter(.data$date <= as.Date("2020-03-31")) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      pre_covid_date = .data$date,
      .data$series_id
    ) %>%
    dplyr::right_join(summary_df, by = "series_id")

  summary_df <- summary_df %>%
    dplyr::group_by(.data$indicator, .data$series_id) %>%
    dplyr::mutate(
      is_level = if_else(grepl("000", .data$unit, fixed = TRUE), TRUE, FALSE),
      value = dplyr::if_else(
        .data$is_level,
        1000 * .data$value,
        .data$value
      ),
      min_date = min(.data$date),
      changeinmonth = (.data$value - dplyr::lag(.data$value)),
      changeinmonthpc = .data$changeinmonth / dplyr::lag(.data$value) * 100,
      changeinyear = (.data$value - dplyr::lag(.data$value, num_in_year)),
      changeinyearpc = .data$changeinyear / dplyr::lag(.data$value, num_in_year) * 100,
      changesincecovid = .data$value - .data$value[.data$date == .data$pre_covid_date],
      changesincecovidpc = (.data$changesincecovid / .data$value[.data$date == .data$pre_covid_date]) * 100,
      changesince14 = ifelse(.data$min_date >= as.Date("2014-11-01"),
        NA_real_,
        (.data$value - .data$value[.data$date == as.Date("2014-11-01")])),
      changesince14pc = (.data$changesince14 / .data$value[.data$date == as.Date("2014-11-01")]) * 100,
    ) %>%
    dplyr::select(-.data$min_date) %>%
    dplyr::filter(.data$date >= startdate) %>%
    dplyr::ungroup()

  rounding_function <- function(x, pretty_round) {
    if (pretty_round) {
      pretty_round(x)
    } else {
      scales::comma(round2(x, 1), accuracy = 1)
    }
  }

  # Reformat columns -----
  summary_df <- summary_df %>%
    dplyr::mutate(
      across(
        c(dplyr::ends_with("pc")),
        ~ dplyr::if_else(.data$is_level,
          paste0(round2(.x, 1), "%"),
          "-"
        )
      ),
      across(
        c(.data$changeinmonth, .data$changeinyear, .data$changesincecovid, .data$changesince14),
        ~ dplyr::if_else(.data$is_level,
          rounding_function(.x, pretty_round),
          sprintf("%.1f ppts", .x)
        )
      ),
      latest_value = dplyr::if_else(
        .data$is_level,
        rounding_function(.data$value, pretty_round),
        sprintf("%.1f%%", .data$value)
      )
    )

  summary_df <- summary_df %>%
    dplyr::mutate(changesince14 = ifelse(.data$changesince14 == "NA ppts", "-", .data$changesince14)) %>%
    dplyr::select(-.data$unit, .data$is_level, .data$pre_covid_date)

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
      .data$changesincecovid,
      .data$changesincecovidpc,
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
      ),
      changesincecovid = ifelse(
        .data$changesincecovidpc != "-",
        paste0(.data$changesincecovid, "\n(", .data$changesincecovidpc, ")"),
        .data$changesincecovid
      )
    )

  changedf <- changedf %>%
    dplyr::select(!dplyr::ends_with("pc"))

  # Format column names
  dates <- unique(data$date) %>%
    sort()

  latest_date <- dates[length(dates)]
  prev_date <- dates[length(dates) - 1]
  prev_year <- subtract_years(latest_date, 1)
  covid_date <- max(summary_df$pre_covid_date)

  nice_latest_date <- format(latest_date, "%b %Y")
  nice_prev_date <- format(prev_date, "%b %Y")
  nice_prev_year <- format(prev_year, "%b %Y")
  nice_covid_date <- format(covid_date, "%b %Y")

  since_prev_date <- paste0("Since ", nice_prev_date)
  since_prev_year <- paste0("Since ", nice_prev_year)
  since_covid <- paste0("Since ", nice_covid_date)

  out <- changedf %>%
    dplyr::select(-.data$date,
      indicator = .data$indicator,
      {{ nice_latest_date }} := .data$latest_value,
      {{ since_prev_date }} := .data$changeinmonth,
      {{ since_prev_year }} := .data$changeinyear,
      {{ since_covid }} := .data$changesincecovid,
      `Since Nov 2014` = .data$changesince14
    )

  stopifnot(nrow(out) == length(unique(data$series_id)))

  # Add sparklines
  sparklines <- summary_df %>%
    dplyr::filter(.data$date >= startdate) %>%
    make_sparklines(group_var = .data$series_id)

  sparklines <- dplyr::tibble(
    series_id = names(sparklines),
    sparklines = sparklines
  )

  out <- out %>%
    dplyr::left_join(sparklines, by = "series_id") %>%
    dplyr::select(.data$indicator, .data$sparklines, dplyr::everything())

  names(out)[2] <- paste0("Last ", years_in_sparklines, " years")

  out
}
