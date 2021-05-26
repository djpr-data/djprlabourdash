#' Create a Reactable to summarise key ABS LFS indicators
#' @param data A dataframe containing tidied ABS Labour Force Survey
#' @param years_in_sparklines Numeric; indicating the number of years of data
#' to include in the sparkline charts in the returned reactable.
#' @param row_var Unquoted variable name in data to use for row names
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
                             "pt_emp_vic"  # Emp PT

                           )),
                           years_in_sparklines = 2,
                           row_var = indicator) {
  data <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::arrange(.data$date) %>%
    # Youth unemployment = 3m rolling average
    dplyr::mutate(value = dplyr::if_else(.data$series_id == "A84433601W",
      zoo::rollmeanr(.data$value, 12, fill = NA),
      .data$value
    )) %>%
    dplyr::ungroup()

  make_reactable(
    data = data,
    years_in_sparklines = years_in_sparklines,
    row_var = {{ row_var }}
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
                                 row_var = indicator) {
  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_reactable(table_data)
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
                                    row_var = indicator) {
  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_reactable(table_data)
}

table_ind_hours_summary <- function(data = filter_dash_data(c(
                                      "A84426256L" # , # Total hours
                                    )),
                                    years_in_sparklines = 2,
                                    row_var = indicator) {
  table_data <- data %>%
    mutate(indicator = if_else(.data$sex != "",
      paste0(.data$indicator, " (", .data$sex, ")"),
      .data$indicator
    ))

  make_reactable(table_data)
}

#' Make a reactable with standard formatting
#'
#' Each row is an indicator; columns are levels / change over particular periods
#' Conditional formatting is applied, sparklines are included.
#'
#' @param data a data frame
#' @param years_in_sparklines number of years prior to latest obs to include
#' in sparkline
#' @param row_var unquoted name of column in `data` to use as the row names in
#' the table
#' @param row_order If `NULL`, table will be sorted in alphabetical order.
#' Otherwise, `row_order` should be a vector of row names in the order in which
#' you want them to appear in the table.

make_reactable <- function(data,
                           years_in_sparklines = 2,
                           row_var = indicator,
                           row_order = NULL) {
  startdate <- subtract_years(max(data$date), years_in_sparklines)

  # Drop unneeded columns -----
  summary_df <- data %>%
    dplyr::select(.data$date, .data$series_id,
      series = {{ row_var }}, .data$value, .data$unit
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
  # Function to return rounded numbers, with commas where appropriate
  # 6175 becomes 6,200. 3445443 becomes 3.445m.
  round_to_thousand <- function(x) {
    sign_x <- sign(x)
    x <- (x / 1000)
    x <- round2(x, 1)
    x <- x * 1000

    dplyr::case_when(
      # Over 10m, round to 1 decimal as in 123.1m
      abs(x) >= 1e8 ~ paste0(round2(x / 1e6, 1), "m"),
      # Over 1m, round to 3 decimals, as in 3.445m
      abs(x) >= 1e6 ~ paste0(round2(x / 1e6, 3), "m"),
      # Otherwise, format with commas as in 100,000
      TRUE ~ scales::comma(x)
    )
  }

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
        round_to_thousand(.x),
        sprintf("%.1f ppts", .x)
      )
    ),
    latest_value = dplyr::if_else(
      .data$is_level,
      round_to_thousand(.data$value),
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

  ## Created df in format required for sparkline ----

  sparklinelist <- summary_df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::summarise(n = list(list(value = dplyr::c_across("value")))) %>%
    dplyr::left_join(changedf, by = "series") %>%
    dplyr::select(-.data$date)

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

  recol_changeinmonthpc <- function(value, index) {
    c(
      recol(value, index, .data$ptile_d_period_perc),
      list(`border-right` = "1px solid #000")
    )
  }

  recol_changeinyear <- function(value, index) {
    recol(value, index, .data$ptile_d_year_abs)
  }

  recol_changeinyearpc <- function(value, index) {
    c(
      recol(value, index, .data$ptile_d_year_perc),
      list(`border-right` = "1px solid #000")
    )
  }

  col_header_style <- list(
    `font-size` = "13px",
    `font-weight` = "400"
  )

  ## Create Reactable -----
  rt1 <- sparklinelist %>%
    dplyr::select(-.data$series_id) %>%
    reactable::reactable(
      defaultPageSize = 50,
      columns = list(
        series = reactable::colDef(
          name = "",
          style = function(value, index) {
            c(
              list(
                fontWeight = "bold" # ,
                # color = colpal[index]
              ),
              cell_padding
            )
          },
          minWidth = 100,
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
                #                dataui::dui_tooltip(
                #                  components = list(
                #                    # Create moving tooltip
                #                  dataui::dui_sparkverticalrefline(
                #                    strokeDasharray = "0, 0",
                #                    strokeWidth = 1,
                #                    stroke = "#838383"
                #                  ) ,
                #                  # display tooltip value
                #                  dataui::dui_sparkpointseries(
                #                   stroke = colpal[index],
                #                   fill = "#fff",
                #                   renderLabel = htmlwidgets::JS("(d) => d.toFixed(1)")
              )
            )
          }
        ),
        changeinmonth = reactable::colDef(
          name = "NO.",
          style = recol_changeinmonth,
          headerStyle = col_header_style,
          align = "center",
          minWidth = 65,
          maxWidth = 90,
        ),
        changeinmonthpc = reactable::colDef(
          name = "PER CENT",
          style = recol_changeinmonthpc,
          headerStyle = col_header_style,
          align = "center",
          minWidth = 65,
          maxWidth = 90
        ),
        changeinyear = reactable::colDef(
          name = "NO.",
          style = recol_changeinyear,
          headerStyle = col_header_style,
          align = "center",
          minWidth = 65,
          maxWidth = 90
        ),
        changeinyearpc = reactable::colDef(
          name = "PER CENT",
          style = recol_changeinyearpc,
          headerStyle = col_header_style,
          align = "center",
          minWidth = 65,
          maxWidth = 90
        ),
        changesince14 = reactable::colDef(
          name = "NO.",
          #          style = recol_changeinyear,
          headerStyle = col_header_style,
          style = c(
            cell_padding
          ),
          align = "center",
          minWidth = 65,
          maxWidth = 90
        ),
        latest_value = reactable::colDef(
          name = toupper(strftime(max(data$date), "%B %Y")),
          align = "center",
          headerStyle = col_header_style,
          style = c(
            cell_padding,
            list(`border-right` = "1px solid #000")
          ),
          maxWidth = 90,
          minWidth = 65
        )
      ),
      columnGroups = list(
        reactable::colGroup(
          name = "Change in month", columns = c("changeinmonth", "changeinmonthpc"),
          headerStyle = list(`font-weight` = "600")
        ),
        reactable::colGroup(
          name = "Change over year", columns = c("changeinyear", "changeinyearpc"),
          headerStyle = list(`font-weight` = "600")
        ),
        reactable::colGroup(
          name = "Change since Nov 2014", columns = c("changesince14"),
          headerStyle = list(`font-weight` = "600")
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

  rt1
}
