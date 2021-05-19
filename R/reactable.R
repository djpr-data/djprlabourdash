#' Create a Reactable to summarise key ABS LFS indicators
#' @param data A dataframe containing tidied ABS Labour Force Survey
#' @param years_in_sparklines Numeric; indicating the number of years of data
#' to include in the sparkline charts in the returned reactable.
#' @param row_var Unquoted variable name in data to use for row names
#' @return A reactable (htmlwidget) including sparklines and sortable columns
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
#' overview_table()
#' }
#'
overview_table <- function(data = filter_dash_data(series_ids = c(
                             "A84423349V",
                             "A84423356T",
                             "A84423355R",
                             "A84423354L",
                             "A84423350C",
                             "A85223451R",
                             "A84426256L"
                           )),
                           years_in_sparklines = 2,
                           row_var = indicator) {
  make_reactable(
    data = data,
    years_in_sparklines = years_in_sparklines,
    row_var = {{ row_var }}
  )
}

indicators_table <- function(data = filter_dash_data(c(
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
    mutate(indicator = if_else(sex != "",
      paste0(indicator, " (", sex, ")"),
      indicator
    ))

  overview_table(table_data)
}


make_reactable <- function(data,
                           years_in_sparklines = 2,
                           row_var = indicator) {

  startdate <- subtract_years(max(data$date), years_in_sparklines)

  # Drop unneeded columns -----
  summary_df <- data %>%
    dplyr::select(.data$date, .data$series_id,
      series = {{ row_var }}, .data$value, .data$unit
    )

  # Calculate change over time -----
  summary_df <- summary_df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(
      is_level = if_else(grepl("000", .data$unit), TRUE, FALSE),
      value = dplyr::if_else(
        is_level,
        1000 * .data$value,
        .data$value
      ),
      changeinmonth = (.data$value - dplyr::lag(.data$value)),
      changeinmonthpc = .data$changeinmonth / dplyr::lag(.data$value) * 100,
      changeinyear = (.data$value - dplyr::lag(.data$value, 12)),
      changeinyearpc = .data$changeinyear / dplyr::lag(.data$value, 12) * 100,
      changesince14 = (.data$value - .data$value[.data$date == as.Date("2014-11-01")])) %>%
    dplyr::filter(.data$date >= startdate)

  # Reformat columns -----
  # Function to return rounded numbers, with commas where appropriate
  # 6175 becomes 6,200. 3445443 becomes 3.445m.
  round_to_thousand <- function(x) {
    x <- (x / 1000)
    x <- round2(x, 1)
    x <- x * 1000

    dplyr::if_else(x > 1e6,
                   paste0(round2(x / 1e6, 3), "m"),
                   scales::comma(x))

  }

  summary_df <- summary_df %>%
    dplyr::mutate(across(
      c(dplyr::ends_with("pc")),
      ~dplyr::if_else(is_level,
                      paste0(round2(.x, 1), "%"),
                      "-")
    ),
    across(
      c(changeinmonth, changeinyear, changesince14),
      ~dplyr::if_else(is_level,
                      round_to_thousand(.x),
                      sprintf("%.1f ppts", .x)
                      )
    ),
    latest_value = dplyr::if_else(
      is_level,
      round_to_thousand(value),
      sprintf("%.1f%%", value)
    )) %>%
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
    dplyr::group_by(series) %>%
    dplyr::summarise(n = list(list(value = dplyr::c_across("value")))) %>%
    dplyr::left_join(changedf, by = "series") %>%
    dplyr::select(-.data$date)

  ## Define colour palette
  colpal <- suppressWarnings(djprtheme::djpr_pal(nrow(sparklinelist)))

  ## Calculate colours for each row
  ts_summ <- djprshiny::ts_summarise(data)

  full_pal <- colorRampPalette(c("#E95A6A", "white", "#62BB46"))(100)

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
    `padding-top` = "8px",
    `padding-right` = "0px",
    `padding-bottom` = "7px",
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
    recol(value, index, ptile_d_period_abs)
  }

  recol_changeinmonthpc <- function(value, index) {
    c(
      recol(value, index, ptile_d_period_perc),
      list(`border-right` = "1px solid #000")
    )
  }

  recol_changeinyear <- function(value, index) {
    recol(value, index, ptile_d_year_abs)
  }

  recol_changeinyearpc <- function(value, index) {
    c(
      recol(value, index, ptile_d_year_perc),
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
          minWidth = 70
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
      resizable = TRUE,
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
