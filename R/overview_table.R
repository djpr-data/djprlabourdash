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
      changeinquarter = (.data$value - dplyr::lag(.data$value)),
      changeinquarterpc = .data$changeinquarter / dplyr::lag(.data$value) * 100,
      changeinquarterpc = sprintf("%.1f %%", .data$changeinquarterpc),
      changeinquarter = ifelse(.data$unit == "000",
        sprintf("%1.0f", .data$changeinquarter),
        sprintf("%.1f %%", .data$changeinquarter)
      ),
      changeinyear = (.data$value - dplyr::lag(.data$value, 4)),
      changeinyearpc = .data$changeinyear / dplyr::lag(.data$value, 4) * 100,
      changeinyearpc = sprintf("%.1f %%", .data$changeinyearpc),
      changeinyear = ifelse(.data$unit == "000",
        sprintf("%1.0f", .data$changeinyear),
        sprintf("%.1f %%", .data$changeinyear)
      )
    ) %>%
    dplyr::filter(.data$date > startdate) %>%
    dplyr::ungroup()

  ## Select only the latest changes in quarter and year

  changedf <- labourforceclean %>%
    dplyr::group_by(.data$series) %>%
    dplyr::slice(which.max(.data$date)) %>%
    dplyr::select(
      .data$date, .data$series, .data$changeinquarter,
      .data$changeinquarterpc, .data$changeinyear,
      .data$changeinyearpc
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

  rt1 <- reactable::reactable(
    sparklinelist, # Specify dataframe to use
    columns = list(
      changeinquarter = reactable::colDef(
        style = function(value) {
          if (value > 0) {
            color <- "#008000"
          } else if (value < 0) {
            color <- "#e00000"
          } else {
            color <- "#777"
          }
          list(background = color, fontWeight = "bold", color = "#ffffff") # Conditional format background based on value
        }
      ),
      changeinquarterpc = reactable::colDef(
        style = function(value) {
          if (value > 0) {
            color <- "#008000"
          } else if (value < 0) {
            color <- "#e00000"
          } else {
            color <- "#777"
          }
          list(background = color, fontWeight = "bold", color = "#ffffff") # Conditional format background based on value
        }
      ),
      changeinyear = reactable::colDef(
        style = function(value) {
          if (value > 0) {
            color <- "#008000"
          } else if (value < 0) {
            color <- "#e00000"
          } else {
            color <- "#777"
          }
          list(background = color, fontWeight = "bold", color = "#ffffff") # Conditional format background based on value
        }
      ),
      changeinyearpc = reactable::colDef(
        style = function(value) {
          if (value > 0) {
            color <- "#008000"
          } else if (value < 0) {
            color <- "#e00000"
          } else {
            color <- "#777"
          }
          list(background = color, fontWeight = "bold", color = "#ffffff") # Conditional format background based on value
        }
      ),
      n = reactable::colDef(
        cell = function(value, index) {
          dataui::dui_sparkline(
            data = value[[1]],
            height = 80,
            components = list(
              dataui::dui_sparklineseries(
                stroke = colpal[index],
                showArea = TRUE,
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
      )
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
