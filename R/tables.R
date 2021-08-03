#' @rdname tables
#' Produce tables for LFS dashboard and associated briefing materials
#' @param data A dataframe containing data to be summarised and displayed;
#' a DF returned by `filter_dash_data()` is expected
#' @noRd

table_overview <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                             unset = "dashboard"),
                           title = paste0("Victorian employment summary, ",
                                          format(max(data$date), "%B %Y"))
                           ) {
  data <- filter_dash_data(series_ids = c(
    "A84423354L",
    "A84423242V",
    "A84423466F",
    "A84424691V",
    "A84600079X", # Regional UR
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
    "A84424692W",
    "A84426256L",
    "A85223450L",
    "A85223451R",
    "A84423356T"
  ),
  df = dash_data)

  # Youth data = 12m rolling average
  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(value = dplyr::if_else(.data$series_id %in% c(
      "A84433601W",
      "A84424691V",
      "A84424687C",
      "A84424692W"
    ),
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
    destination = destination,
    title = title,
    row_order = c(
      "A84423354L",
      "A84423242V",
      "A84423466F",
      "A84424691V",
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
      "A84424692W",
      "A84426256L",
      "A85223450L",
      "A85223451R",
      "A84423356T"
    ),
    highlight_rows = c(
      "A84423354L",
      "A84423349V",
      "A84423355R",
      "A84426256L",
      "A85223450L",
      "A85223451R",
      "A84423356T"
    ),
    notes = " All data seasonally adjusted, other than youth figures, which are smoothed using a 12 month rolling average, and regional figures, which are smoothed using a 3 month rolling average."
  )
}

table_gr_sex <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                                  unset = "dashboard"),
                         title = paste0("Victorian employment summary by sex, ",
                                        format(max(data$date), "%B %Y"))) {
  data <- filter_dash_data(c("A84423237A",
                              "A84423461V",
                              "A84423238C",
                              "A84423462W",
                              "A84423242V",
                              "A84423466F",
                              "A84423243W",
                              "A84423467J")
)

  make_table(data,
             row_order = c("A84423237A",
                            "A84423461V",
                            "A84423238C",
                            "A84423462W",
                            "A84423242V",
                            "A84423466F",
                            "A84423243W",
                            "A84423467J"),
             title = title
)
}

table_ind_unemp_state <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                                           unset = "dashboard"),
                                  title = paste0("Unemployment rate by state, ",
                                                 format(max(data$date), "%B %Y"))) {
  data <- filter_dash_data(
    c("A84423270C",
      "A84423354L",
      "A84423284T",
      "A84423368A",
      "A84423326C",
      "A84423298F",
      "A84423050A")
  )

  data <- data %>%
    dplyr::mutate(indicator = dplyr::if_else(.data$state == "", "Australia", .data$state))# %>%
    # dplyr::mutate(indicator = paste0(.data$indicator, " unemployment rate"))

  make_table(data = data,
             row_order = c("A84423050A",
                           "A84423354L",
                           "A84423270C",
                           "A84423284T",
                           "A84423326C",
                           "A84423368A",
                           "A84423298F"),
             title = title,
             rename_indicators = F)
}

table_ind_employment <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                                          unset = "dashboard"
)) {
  data <- filter_dash_data(c(
    "A84423349V",
    "A84423357V",
    "A84423356T",
    "A84423244X",
    "A84423468K",
    "pt_emp_vic"
  ))

  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_table(table_data,
    destination = destination
  )
}

table_ind_unemp_summary <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                                             unset = "dashboard"
)) {
  data <- filter_dash_data(c(
    "A84423354L", # Unemp rate
    "A84423350C", # Unemp total
    "A85223451R", # Underut rate
    "A84433601W", # Youth unemp,
    "A84423242V", # Male unemp
    "A84423466F" # Female unemp
  ))

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
    destination = destination
  )
}

table_ind_hours_summary <- function(destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST",
                                                             unset = "dashboard"
)) {
  data <- filter_dash_data(c(
    "A84426256L" # , # Total hours
  ))

  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_table(table_data,
    destination = destination
  )
}
