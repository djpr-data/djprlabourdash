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
table_overview <- function(data = filter_dash_data(series_ids = c(
                             "A84423349V", # Employed total
                             "A84423355R", # Part rate
                             "A84423354L", # Unemp rate
                             "A84423350C", # Unemp total
                             "A85223451R", # Underut rate
                             "A84426256L", # Hours worked
                             "A85223450L", # Underemp rate
                             "A84423357V", # Emp FT
                             "pt_emp_vic" # Emp PT
                           )),
                           years_in_sparklines = 2,
                           row_var = "indicator") {
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
