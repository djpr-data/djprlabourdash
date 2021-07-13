#' Make a reactable with standard formatting
#'
#' Each row is an indicator; columns are levels / change over particular periods
#' Conditional formatting is applied, sparklines are included.
#'
#' @param data a data frame
#' @param years_in_sparklines number of years prior to latest obs to include
#' in sparkline
#' @param row_var "Quoted" name of column in `data` to use as the row names in
#' the table - as in "indicator"
#' @param row_order If `NULL`, table will be sorted in alphabetical order.
#' Otherwise, `row_order` should be a vector of row names in the order in which
#' you want them to appear in the table.
#' @param highlight_rows numeric vector of rows in the table body (ie.
#' excluding column names / header row) to highlight. Non-highlighted rows
#' will be indented. When `NULL`, no row is highlighted.

make_reactable <- function(data,
                           years_in_sparklines = 2,
                           row_var = "indicator",
                           row_order = NULL,
                           highlight_rows = NULL) {

  sparklinelist <- create_summary_df(
    data = data,
    years_in_sparklines = years_in_sparklines,
    row_var = row_var,
    row_order = row_order
  )

  ## Define colour palette
  n_series <- nrow(sparklinelist)
  colpal <- grDevices::colorRampPalette(suppressWarnings(djpr_pal(10)))(n_series)

  ## Calculate colours for each row
  ts_summ <- djprshiny::ts_summarise(data)

  full_pal <- grDevices::colorRampPalette(c("#E95A6A", "white", "#62BB46"))(100)

  calc_cols <- function(series_ids, item, summ_df = ts_summ) {
    ptiles <- get_summ(series_ids, {{ item }},
      df = summ_df
    )

    # For some indicators, 20 pctile is "bad", for some it is "good"
    up_is_good <- get_summ(series_ids, up_is_good,
      df = summ_df
    )
    ptiles <- ifelse(up_is_good, ptiles, 1 - ptiles)

    ptiles <- round(ptiles * 100, 0)
    full_pal[ptiles]
  }

  cell_padding <- list(
    `padding-top` = "15px",
    `padding-right` = "0px",
    `padding-bottom` = "6px",
    `padding-left` = "0px"
  )

  recol <- function(value, index, item,
                    padding = cell_padding) {
    if (value == "-") {
      cols <- "#E6E6E680"
    } else {
      cols <- calc_cols(
        sparklinelist$series_id[index],
        {{ item }}
      )
    }

    c(
      list(
        background = cols,
        fontWeight = "normal",
        colour = "#000"
      ),
      list(border = "1px solid rgba(0, 0, 0, 0.03)"),
      padding
    )
  }

  recol_changeinmonth <- function(value, index) {
    recol(value, index, .data$ptile_d_period_abs)
  }


  recol_changeinyear <- function(value, index) {
    recol(value, index, .data$ptile_d_year_abs)
  }

  col_header_style <- list(
    `font-size` = "13px",
    `font-weight` = "400"
  )

  data_col_min_width <- 65
  data_col_max_width <- 80

  ## Create Reactable -----
  react_out <- sparklinelist %>%
    dplyr::select(-.data$series_id) %>%
    reactable::reactable(
      defaultPageSize = 50,
      columns = list(
        series = reactable::colDef(
          name = "",
          # Highlight rows are bolded; other rows are not
          style = function(value, index) {
            if (index %in% highlight_rows) {
              c(
                list(
                  fontWeight = "bold"
                ),
                cell_padding
              )
            } else {
              cell_padding
            }

          },
          minWidth = 100,
          # Highlight rows are not indented; non-highlight rows are
          cell = function(value, index) {
            if (is.null(highlight_rows)) {
              value
            } else {
              if (!index %in% highlight_rows) {
                shiny::div(style = "padding-left: 25px", value)
              } else {
                value
              }
            }
          }
        ),
        n = reactable::colDef(
          name = paste0("LAST ", years_in_sparklines, " YEARS"),
          align = "center",
          maxWidth = 250,
          minWidth = 50,
          headerStyle = col_header_style,
          cell = function(value, index) {
            dataui::dui_sparkline(
              data = value[[1]],
              height = 50,
              margin = list(
                top = 7, right = 5,
                bottom = 7, left = 5
              ),
              components = list(
                # Create actual sparkline
                dataui::dui_sparklineseries(
                  stroke = colpal[index],
                  showArea = F,
                  fill = colpal[index]
                )
              )
            )
          }
        ),
        changeinmonth = reactable::colDef(
          name = toupper("Change in month"),
          style = recol_changeinmonth,
          headerStyle = col_header_style,
          align = "center",
          minWidth = data_col_min_width,
          maxWidth = data_col_max_width,
        ),
        changeinyear = reactable::colDef(
          name = toupper("Change over year"),
          style = recol_changeinyear,
          headerStyle = col_header_style,
          align = "center",
          minWidth = data_col_min_width,
          maxWidth = data_col_max_width
        ),
        changesince14 = reactable::colDef(
          name = toupper("Change since Nov 2014"),
          headerStyle = col_header_style,
          style = c(
            cell_padding
          ),
          align = "center",
          minWidth = data_col_min_width,
          maxWidth = data_col_max_width
        ),
        latest_value = reactable::colDef(
          name = toupper(strftime(max(data$date), "%B %Y")),
          align = "center",
          headerStyle = col_header_style,
          style = c(
            cell_padding,
            list(`border-right` = "1px solid #000")
          ),
          minWidth = data_col_min_width,
          maxWidth = data_col_max_width
        )
      ),
      highlight = TRUE,
      resizable = FALSE,
      sortable = FALSE,
      theme = reactable::reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "7px 1px 1px 1px",
        tableStyle = list(`border-bottom` = "1px solid #000"),
        headerStyle = list(
          fontWeight = "normal",
          `border-bottom` = "1px solid #000"
        ),
        groupHeaderStyle = list(fontWeight = "normal"),
        style = list(fontFamily = "Roboto, sans-serif, -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial")
      )
    )

  react_out
}
