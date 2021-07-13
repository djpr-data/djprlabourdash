#' Create a Reactable to summarise key ABS LFS indicators
#' @param data A dataframe containing tidied ABS Labour Force Survey
#' @param years_in_sparklines Numeric; indicating the number of years of data
#' to include in the sparkline charts in the returned reactable.
#' @param row_var String - variable name in data to use for row names,
#' as in "indicator"
#' @return A reactable (htmlwidget) including sparklines
#' @author Darren Wong, Duy Nguyen
#' @examples
#' # Using the data available to this dashboard
#' \dontrun{
#' dash_data <- load_dash_data()
#' table_ids <- c(
#'   "A84423349V",
#'   "A84423356T",
#'   "A84423355R",
#'   "A84423354L",
#'   "A84423350C",
#'   "A85223451R"
#' )
#'
#' table_data <- filter_dash_data(table_ids)
#'
#' table_overview()
#' }
#'
table_overview <- function(data = filter_dash_data(series_ids = c("A84423354L",
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
                                                                    "A84423356T")),
                           years_in_sparklines = 2,
                           row_var = "indicator") {

  data <- data %>%
    dplyr::mutate(
      indicator = dplyr::case_when(
        .data$series_id == "A84423242V" ~ "Male unemployment rate",
        .data$series_id == "A84423466F" ~ "Female unemployment rate",
        .data$series_id == "A84433601W" ~ "Youth unemployment rate",
        .data$series_id == "A84423243W" ~ "Male participation rate",
        .data$series_id == "A84423467J" ~ "Female participation rate",
        .data$series_id == "A84433602X" ~ "Youth participation rate",
        .data$series_id == "A84600079X" ~ "Regional unemployment rate",
        TRUE ~ .data$indicator
      )
    )

  make_reactable(
    data = data,
    years_in_sparklines = years_in_sparklines,
    row_var = row_var
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
                                 years_in_sparklines = 2,
                                 row_var = "indicator") {
  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_reactable_mem(table_data,
    years_in_sparklines = years_in_sparklines,
    row_var = row_var
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
                                    years_in_sparklines = 2,
                                    row_var = "indicator") {

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


  make_reactable_mem(table_data,
    years_in_sparklines = years_in_sparklines,
    row_var = row_var
  )
}

table_ind_hours_summary <- function(data = filter_dash_data(c(
                                      "A84426256L" # , # Total hours
                                    )),
                                    years_in_sparklines = 2,
                                    row_var = "indicator") {
  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_reactable_mem(table_data,
    years_in_sparklines = years_in_sparklines,
    row_var = row_var
  )
}
