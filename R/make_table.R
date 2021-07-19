#' Create a table for the dashboard or a briefing document
#' @param data A data frame containing data to summarise
#' @param dashboard_or_briefing either "dashboard" or "briefing", specifying
#' the destination for the output table
#' @param years_in_sparklines Period of time to include in the sparkline line
#' charts. Only relevant when `dashboard_or_briefing` =
#' "dashboard".
#' @param row_order Vector of series IDs, in the order in which you wish the
#' corresponding rows to be included in the output table
#' @param highlight_rows Numeric vector of rows in the table to highlight.
#' Highlighted rows are bolded and have a top border; non-highlighted rows
#' are indented. If `NULL` then all rows are non-bold, non-indented.
#' @return Either a `reactable` (if `dashboard_or_briefing` = "dashboard") or a
#' `flextable` (if `dashboard_or_briefing` = "briefing") summarising `data`.
#' @examples
#' \dontrun{
#' dash_data <- load_dash_data()
#'
#' make_table(
#' data = filter_dash_data(series_ids = c(
#'   "A84423354L",
#'     "A84423242V",
#'  "A84423466F",
#'  "A84433601W",
#'  "A84600079X",
#'  "A84423350C",
#'  "A84423349V",
#'  "A84423357V",
#'  "pt_emp_vic",
#'  "A84423461V",
#'  "A84423237A",
#'  "A84424687C",
#'  "A84423355R",
#'  "A84423243W",
#'  "A84423467J",
#'  "A84433602X",
#'  "A84426256L",
#'  "A85223450L",
#'  "A85223451R",
#'  "A84423356T"
#')),
#'dashboard_or_briefing = "dashboard",
#'row_order = c(
#'  "A84423354L",
#'  "A84423242V",
#'  "A84423466F",
#'  "A84433601W",
#'  "A84600079X",
#'  "A84423350C",
#'  "A84423349V",
#'  "A84423357V",
#'  "pt_emp_vic",
#'  "A84423461V",
#'  "A84423237A",
#'  "A84424687C",
#'  "A84423355R",
#'  "A84423243W",
#'  "A84423467J",
#'  "A84433602X",
#'  "A84426256L",
#'  "A85223450L",
#'  "A85223451R",
#'  "A84423356T"
#'),
#'highlight_rows = c(1, 6, 7, 13, 17, 18, 19, 20)
#')
#'
#' }

make_table <- function(data,
                       dashboard_or_briefing = c("dashboard", "briefing"),
                       years_in_sparklines = 2,
                       row_order = NULL,
                       highlight_rows = NULL) {

  dashboard_or_briefing <- match.arg(dashboard_or_briefing)

  # Change value of indicator column for specific series IDs
  df <- rename_indicators(data)

  # Create a summary dataframe with one row per unique indicator
  summary_df <- create_summary_df(df,
                                  dashboard_or_briefing = dashboard_or_briefing)

  # Reorder dataframe if row_order is specified
  if (!is.null(row_order)) {
      # Check that all series IDs in the data are in `row_order`
      if (!all(summary_df$series_id %in% row_order)) {
        stop("`row_order` was specified, but not all series IDs are included")
      }
    summary_df <- summary_df %>%
      dplyr::mutate(order = match(.data$series_id, row_order)) %>%
      dplyr::arrange(.data$order) %>%
      dplyr::select(-.data$order)
  }

  # Set highlight rows as numeric vector
  if (!is.null(highlight_rows)) {
    highlight_rows <- which(summary_df$series_id %in% highlight_rows)
  }

  # Add note about release date if earlier than rest of data
  date_notes <- df %>%
    dplyr::group_by(.data$series_id, .data$indicator) %>%
    dplyr::summarise(max_date = max(.data$date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(indicator = dplyr::if_else(
      .data$max_date == max(.data$max_date),
      .data$indicator,
      paste0(.data$indicator, " (", format(max_date, "%B %Y"), ")")
    )) %>%
    dplyr::select(.data$series_id, .data$indicator)

  summary_df <- summary_df %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$indicator) %>%
    dplyr::left_join(date_notes, by = "series_id") %>%
    dplyr::select(.data$indicator, dplyr::everything())

  if (dashboard_or_briefing == "briefing") {
    summary_df <- summary_df %>%
      dplyr::rename(` ` = .data$indicator)
  }

  # table_caption <- caption_auto(df)

  if (dashboard_or_briefing == "dashboard") {
    make_reactable(summary_df = summary_df,
                   raw_data = df,
                   years_in_sparklines = years_in_sparklines,
                   highlight_rows = highlight_rows)
  } else {
    make_briefing_table(summary_df,
                        highlight_rows = highlight_rows,
                        header_row = c(
                          "",
                          "Current figures",
                          "Change in past month",
                          "Change in past year",
                          "Change during govt"
                        ))
  }

}
