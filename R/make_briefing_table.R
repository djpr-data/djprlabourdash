#' Take a data frame summarising data and create a flextable for
#' a briefing table
#' @param df Dataframe created using `create_summary_df()`
#' @param highlight_rows numeric vector of rows in the table body (ie.
#' excluding column names / header row) to highlight. Non-highlighted rows
#' will be indented. When `NULL`, no row is highlighted.
#' @param header_row Character vector of names of additional header row to be
#' added above existing column names
#' @return A `flextable::flextable` object

make_briefing_table <- function(df,
                                highlight_rows = NULL,
                                header_row = c(
                                  "",
                                  "Current figures",
                                  "Change in past month",
                                  "Change in past year",
                                  "Change during govt"
                                )) {
  df <- df %>%
    dplyr::select(-.data$series_id)

  names(df) <- toupper(names(df))

  # Create a basic flextable using the supplied dataframe
  base_flex <- df %>%
    flextable::flextable()

  # Ensure the flextable fits the container (eg. Word doc) it is placed in
  flex <- base_flex %>%
    flextable::set_table_properties(layout = "autofit")

  # Add an extra header row
  flex <- flex %>%
    flextable::add_header_row(values = header_row)

  # Add borders
  flex <- flex %>%
    flextable::border_remove() %>%
    flextable::border(
      i = 1,
      border.top = flextable::fp_border_default()
    ) %>%
    flextable::border(i = 1, part = "header", border.top = flextable::fp_border_default()) %>%
    flextable::border(i = nrow(df), border.bottom = flextable::fp_border_default())

  # Ensure font, font size, and bolding is correct
  flex <- flex %>%
    flextable::font(fontname = "Arial") %>%
    flextable::font(fontname = "Arial", part = "header") %>%
    flextable::fontsize(size = 9) %>%
    flextable::fontsize(size = 9, i = 1, part = "header") %>%
    flextable::fontsize(size = 8, i = 2, part = "header") %>%
    flextable::bold(i = 1, part = "header")

  # Right align columns other than the first one (row label/indicator)
  flex <- flex %>%
    flextable::align(j = -1, align = "right") %>%
    flextable::align(j = -1, align = "right", part = "header")

  # Bold highlight rows, indent non-highlight rows
  if (!is.null(highlight_rows)) {
    flex <- flex %>%
      flextable::bold(i = highlight_rows, j = 1)

    all_rows <- 1:nrow(df)
    non_highlight_rows <- all_rows[!all_rows %in% highlight_rows]

    flex <- flex %>%
      flextable::padding(i = non_highlight_rows, j = 1, padding.left = 20)

    flex <- flex %>%
      flextable::border(
        i = highlight_rows,
        border.top = flextable::fp_border_default()
      )
  } else {
    # Bold all row labels if no highlight row chosen
    flex <- flex %>%
      flextable::bold(j = 1)
  }

  flex
}
