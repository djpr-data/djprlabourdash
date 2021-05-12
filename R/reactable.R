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
#'   "A84423350C"
#' )
#'
#' table_data <- filter_dash_data(table_ids)
#'
#' overview_table(table_data)
#' }
#'
overview_table <- function(data = filter_dash_data(series_ids = c(
                             "A84423349V",
                             "A84423356T",
                             "A84423355R",
                             "A84423354L",
                             "A84423350C"
                           )),
                           years_in_sparklines = 2,
                           row_var = indicator) {
  make_reactable(
    data = data,
    years_in_sparklines = years_in_sparklines,
    row_var = {{ row_var }}
  )
}


make_reactable <- function(data,
                           years_in_sparklines = 2,
                           row_var = indicator) {

  # Function to avoid adding a lubridate dependency
  subtract_years <- function(max_date, n_years) {
    seq(max_date,
      length = 2,
      by = paste0("-", n_years, " years")
    )[2]
  }

  startdate <- subtract_years(max(data$date), years_in_sparklines)

  summary_df <- data %>%
    dplyr::select(.data$date, .data$series_id,
                  series = {{ row_var }}, .data$value, .data$unit) %>%
    dplyr::filter(.data$date >= startdate) %>%
    dplyr::group_by(.data$series) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(
      value = dplyr::if_else(.data$unit == "000",
        1000 * .data$value,
        .data$value
      ),
      changeinmonth = (.data$value - dplyr::lag(.data$value)),
      changeinmonthpc = .data$changeinmonth / dplyr::lag(.data$value) * 100,
      changeinmonthpc = dplyr::if_else(
        unit == "000",
        sprintf("%0.1f %%", changeinmonthpc),
        "-"
      ),
      changeinmonth = ifelse(.data$unit == "000",
        format(round(changeinmonth), big.mark = ",", scientific = F, trim = T),
        sprintf("%.1f ppts", .data$changeinmonth)
      ),
      changeinyear = (.data$value - dplyr::lag(.data$value, 12)),
      changeinyearpc = .data$changeinyear / dplyr::lag(.data$value, 12) * 100,
      changeinyearpc = dplyr::if_else(
        unit == "000",
        sprintf("%0.1f %%", changeinyearpc),
        "-"
      ),
      changeinyear = ifelse(.data$unit == "000",
        format(round(changeinyear), big.mark = ",", scientific = F, trim = T),
        sprintf("%.1f ppts", .data$changeinyear)
      ),
      latest_value = dplyr::if_else(
        unit == "000",
        format(round(value), big.mark = ",", scientific = F, trim = T),
        sprintf("%.1f %%", value)
      )
    ) %>%
    dplyr::ungroup()

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
    ) %>%
    dplyr::ungroup()

  ## Create a dataframe in the format required for a sparkline with the list function

  sparklinelist <- summary_df %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(n = list(list(value = dplyr::c_across("value")))) %>%
    dplyr::left_join(changedf, by = "series") %>%
    dplyr::select(-.data$date)

  ## Define colour palette
  colpal <- djprtheme::djpr_pal(nrow(sparklinelist))


  ## Calculate colours for each row
  ts_summ <- djprshiny::ts_summarise(data)

  full_pal <- colorRampPalette(c("#EA4125", "white", "#61A951"))(100)

  calc_cols <- function(series_ids, item) {
    ptiles <- get_summ(series_ids, {{item}})
    ptiles <- round(ptiles * 100, 0)
    full_pal[ptiles]
  }

  recol <- function(value, index, item) {

    if (value == "-") {
      cols <- "#E6E6E680"
    } else {
      cols <- calc_cols(sparklinelist$series_id[index],
                        {{item}})
    }

    c(
      list(background = cols,
           fontWeight = "normal",
           colour = "#000"),
      list(border = "1px solid rgba(0, 0, 0, 0.03)")
    )

  }

  recol_changeinmonth <- function(value, index) {
    recol(value, index, ptile_d_period_abs)
  }

  recol_changeinmonthpc <- function(value, index) {
    recol(value, index, ptile_d_period_perc)
  }

  recol_changeinyear <- function(value, index) {
    recol(value, index, ptile_d_year_abs)
  }

  recol_changeinyearpc <- function(value, index) {
    recol(value, index, ptile_d_year_perc)
  }


  recolor_col <- function(value) {
    if (value > 0) {
      # Green
      color <- "#f0fff0"
    } else if (value < 0 & value != "-") {
      # Red
      color <- "#fff5ee"
    } else {
      # Grey
      color <- "#E6E6E680"
    }
    # Conditional format background based on value
    list(background = color, fontWeight = "normal", color = "#000")
  }

  ## Create Reactable
  rt1 <- sparklinelist %>%
    dplyr::select(-.data$series_id) %>%
    reactable::reactable(
    columns = list(
      series = reactable::colDef(
        name = "",
        style = function(value, index) {
          list( # color = colpal[index],
            fontWeight = "bold"
          )
        },
        minWidth = 100,
      ),
      n = reactable::colDef(
        name = paste0("Last ", years_in_sparklines, " years"),
        align = "center",
        maxWidth = 250,
        minWidth = 50,
        cell = function(value, index) {
          dataui::dui_sparkline(
            data = value[[1]],
            height = 40,
            margin = list(
              top = 7, right = 3,
              bottom = 7, left = 3
            ),
            components = list(
              dataui::dui_sparklineseries(
                stroke = colpal[index],
                showArea = F,
                fill = colpal[index] # Create actual sparkline
              ),
              dataui::dui_tooltip(components = list(
                dataui::dui_sparkverticalrefline(
                  strokeDasharray = "4,4",
                  stroke = "#838383" # Create moving tooltip
                ),
                dataui::dui_sparkpointseries(
                  stroke = colpal[index],
                  fill = "#fff",
                  renderLabel = htmlwidgets::JS("(d) => d.toFixed(1)") # display tooltip value
                )
              ))
            )
          )
        }
      ),
      changeinmonth = reactable::colDef(
        name = "No.",
        style = recol_changeinmonth,
        align = "center",
        minWidth = 50,
        maxWidth = 90,
      ),
      changeinmonthpc = reactable::colDef(
        name = "%",
        style = recol_changeinmonthpc,
        align = "center",
        minWidth = 50,
        maxWidth = 90
      ),
      changeinyear = reactable::colDef(
        name = "No.",
        style = recol_changeinyear,
        align = "center",
        minWidth = 50,
        maxWidth = 90
      ),
      changeinyearpc = reactable::colDef(
        name = "%",
        style = recol_changeinyearpc,
        align = "center",
        minWidth = 50,
        maxWidth = 90
      ),
      latest_value = reactable::colDef(
        name = strftime(max(data$date), "%B %Y"),
        align = "center",
        maxWidth = 100,
        minWidth = 65
      )
    ),
    columnGroups = list(
      reactable::colGroup(name = "Change in month", columns = c("changeinmonth", "changeinmonthpc")),
      reactable::colGroup(name = "Change over year", columns = c("changeinyear", "changeinyearpc"))
    ),
    highlight = TRUE,
    resizable = TRUE,
    theme = reactable::reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "7px 1px 1px",
      headerStyle = list(fontWeight = "normal"),
      groupHeaderStyle = list(fontWeight = "normal"),
      style = list(fontFamily = "Roboto, sans-serif, -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial")
    )
  )

  rt1
}
