#' Function to make graphs that will go on the 'Indicators' page of the dashboard,
#' @examples
#' \dontrun{
#'
#' dash_data <- load_dash_data()
#' data <- filter_dash_data(c("A84423043C", "A84423349V"),
#'                          df = dash_data) %>%
#'   dplyr::filter(date >= as.Date("2020-01-01"))
#'
#'  )}


# Define the djpr_ts_linechart() function

viz_empgrowth_sincecovid <- function(data) {

  df <- data %>%
  dplyr::mutate(state = dplyr::if_else(state == "", "Australia", state))

  df %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(value = 100* ((value / value[date == as.Date("2020-03-01")]) - 1)) %>%
    djpr_ts_linechart(col_var = .data$state,
                      label_num = paste0(round(.data$value, 1), "%"),
                      y_labels = function(x) paste0(x, "%"))
  }

