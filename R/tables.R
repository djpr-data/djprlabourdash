#' @rdname tables
#' Produce tables for LFS dashboard and associated briefing materials
#' @param data A dataframe containing data to be summarised and displayed;
#' a DF returned by `filter_dash_data()` is expected
#' @param dashboard_or_briefing Either "dashboard" or "briefing"
#' @noRd

table_overview <- function(data = filter_dash_data(series_ids = c(
                             "A84423354L",
                             "A84423242V",
                             "A84423466F",
                             "A84433601W",
                             "A84600079X",
                             "A84423350C",
                             "A84423349V",
                             "A84423357V",
                             "pt_emp_vic",
                             "A84423461V",
                             "A84423237A",
                             "A84424687C",
                             "A84423355R",
                             "A84423243W",
                             "A84423467J",
                             "A84433602X",
                             "A84426256L",
                             "A85223450L",
                             "A85223451R",
                             "A84423356T"
                           )),
                           dashboard_or_briefing = "dashboard") {

  # Youth data = 12m rolling average
  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(value = dplyr::if_else(.data$series_id %in% c("A84433601W",
                                                                "A84424687C",
                                                                "A84433602X"),
                                         slider::slide_mean(.data$value, before = 11, complete = TRUE),
                                         .data$value
    )) %>%
    dplyr::ungroup()

  # Regional data = 3m rolling average
  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(value = dplyr::if_else(.data$series_id %in% c("A84600079X"),
                                         slider::slide_mean(.data$value, before = 2, complete = TRUE),
                                         .data$value
    )) %>%
    dplyr::ungroup()

  make_table(
    data = data,
    dashboard_or_briefing = dashboard_or_briefing,
    row_order = c(
      "A84423354L",
      "A84423242V",
      "A84423466F",
      "A84433601W",
      "A84600079X",
      "A84423350C",
      "A84423349V",
      "A84423357V",
      "pt_emp_vic",
      "A84423461V",
      "A84423237A",
      "A84424687C",
      "A84423355R",
      "A84423243W",
      "A84423467J",
      "A84433602X",
      "A84426256L",
      "A85223450L",
      "A85223451R",
      "A84423356T"
    ),
    highlight_rows = c(1, 6, 7, 13, 17, 18, 19, 20)
  )
}

table_ind_employment <- function(data = filter_dash_data(c(
                                   "A84423349V",
                                   "A84423357V",
                                   "A84423356T",
                                   "A84423244X",
                                   "A84423468K",
                                   "pt_emp_vic"
                                 )),
                                 dashboard_or_briefing = "dashboard") {
  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_table(table_data,
             dashboard_or_briefing = dashboard_or_briefing
  )
}

table_ind_unemp_summary <- function(data = filter_dash_data(c(
                                      "A84423354L", # Unemp rate
                                      "A84423350C", # Unemp total
                                      "A85223451R", # Underut rate
                                      "A84433601W", # Youth unemp,
                                      "A84423242V", # Male unemp
                                      "A84423466F" # Female unemp
                                    )),
                                    dashboard_or_briefing = "dashboard") {

  # Youth unemployment = 12m rolling average
  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(value = dplyr::if_else(.data$series_id == "A84433601W",
      slider::slide_mean(.data$value, before = 11, complete = TRUE),
      .data$value
    )) %>%
    dplyr::ungroup()

  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_table(table_data,
             dashboard_or_briefing = dashboard_or_briefing
  )
}

table_ind_hours_summary <- function(data = filter_dash_data(c(
                                      "A84426256L" # , # Total hours
                                    )),
                                    dashboard_or_briefing = "dashboard") {
  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_table(table_data,
                 dashboard_or_briefing = dashboard_or_briefing
  )
}
