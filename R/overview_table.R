#' Create a Reactable to summarise key ABS LFS indicators
#' @param data A dataframe containing tidied ABS Labour Force Survey
#' @param years_in_sparklines Numeric; indicating the number of years of data
#' to include in the sparkline charts in the returned reactable.
#' @return A reactable (htmlwidget) including sparklines and sortable columns
#' @author Darren Wong
#' @examples
#' # Using the data available to this dashboard
#' \dontrun{
#' dash_data <- load_dash_data()
#' series_ids <- c(
#'   "A84423349V",
#'   "A84423356T",
#'   "A84423355R",
#'   "A84423354L",
#'   "A84423350C"
#' )
#'
#' data <- dash_data %>%
#'   dplyr::filter(.data$series_id %in% series_ids) %>%
#'   tidyr::unnest(cols = everything())
#'
#' overview_table(data = data)
#' }
#'
overview_table <- function(data,
                           years_in_sparklines = 2) {

  # Function to avoid adding a lubridate dependency
  subtract_years <- function(max_date, n_years) {
    seq(max_date,
      length = 2,
      by = paste0("-", n_years, " years")
    )[2]
  }

  startdate <- subtract_years(max(data$date), years_in_sparklines)

  template <- data %>%
    dplyr::group_by(.data$series) %>%
    dplyr::count()

  labourforceclean <- data %>%
    dplyr::select(.data$date, series = .data$series, .data$value, .data$unit) %>%
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
        format(round(changeinmonth), big.mark=",", scientific=F, trim=T),
        sprintf("%.1f ppts", .data$changeinmonth)
      ),
      changeinyear = (.data$value - dplyr::lag(.data$value, 4)),
      changeinyearpc = .data$changeinyear / dplyr::lag(.data$value, 4) * 100,
      changeinyearpc = dplyr::if_else(
        unit == "000",
        sprintf("%0.1f %%", changeinyearpc),
        "-"
      ),
      changeinyear = ifelse(.data$unit == "000",
        format(round(changeinyear), big.mark=",", scientific=F, trim=T),
        sprintf("%.1f ppts", .data$changeinyear)
      ),
      latest_value=dplyr::if_else(
        unit == "000",
        format(round(value), big.mark=",", scientific=F, trim=T),
        sprintf("%.1f %%", value)

      )
    ) %>%
    dplyr::filter(.data$date > startdate) %>%
    dplyr::ungroup()

  ## Select only the latest changes in quarter and year

  changedf <- labourforceclean %>%
    dplyr::group_by(.data$series) %>%
    dplyr::slice(which.max(.data$date)) %>%
    dplyr::select(
      .data$date, .data$series, .data$changeinmonth,
      .data$changeinmonthpc, .data$changeinyear,
      .data$changeinyearpc, latest_value
    ) %>%
    dplyr::ungroup()


  ## Create function to change time series to a list

  fun1 <- function(x) {
    labourforceclean %>%
      dplyr::filter(.data$series == x) %>%
      dplyr::select(.data$value) %>%
      as.list()
  }


  ## Create a dataframe in the format required for a sparkline with the list function

  sparklinelist <- template %>%
    dplyr::mutate(n = purrr::map(.data$series, fun1)) %>%
    dplyr::left_join(changedf, by = "series") %>%
    dplyr::select(-.data$date)


  ## Define colour palette

  colpal <- djprtheme::djpr_pal(6)


  ## Create Reactable
  recolor_col <- function(value) {
    if (value > 0) {
      color <- "#f0fff0"
    } else if (value < 0 & value != "-") {
      color <- "#fff5ee"
    } else {
      color <- "#ddd"
    }
    list(background = color, fontWeight = "bold", color = "#000") # Conditional format background based on value
  }

  rt1 <- reactable::reactable(
    sparklinelist, # Specify dataframe to use
    columns = list(
      series = reactable::colDef(
        name = ""
      ),
      n = reactable::colDef(
        name = "",
        cell = function(value, index) {
          dataui::dui_sparkline(
            data = value[[1]],
            height = 80,
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
                  renderLabel = htmlwidgets::JS("(d) => d.toFixed(2)") # display tooltip value
                )
              ))
            )
          )
        }
      ),
      changeinmonth = reactable::colDef(
        name = "No.",
        style = recolor_col,
        align = "center"
      ),
      changeinmonthpc = reactable::colDef(
        name = "%",
        style = recolor_col,
        align = "center"
      ),
      changeinyear = reactable::colDef(
        name = "No.",
        style = recolor_col,
        align = "center"
      ),
      changeinyearpc = reactable::colDef(
        name = "%",
        style = recolor_col,
        align = "center"
      ),
      latest_value = reactable::colDef(
        name = strftime(max(data$date), "%b %Y"),
        style = recolor_col,
        align = "center"
      )
    ),
    columnGroups = list(
      reactable::colGroup(name="Change in month", columns=c("changeinmonth", "changeinmonthpc")),
      reactable::colGroup(name="Change over year", columns=c("changeinyear", "changeinyearpc"))
    ),
    highlight = TRUE,
    resizable = TRUE,
    theme = reactable::reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
    )
  )

  rt1
}
