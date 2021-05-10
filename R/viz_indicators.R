#' Line chart of cumulative employment change since March 2020
#' in Victoria and Australia
viz_empgrowth_sincecovid <- function(data) {

  df <- data %>%
    dplyr::mutate(state = dplyr::if_else(state == "", "Australia", state))

  df %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(value = 100 * ((value / value[date == as.Date("2020-03-01")]) - 1)) %>%
    # djpr_ts_linechart(col_var = .data$state,
    #                   label_num = paste0(round(.data$value, 1), "%"),
    #                   y_labels = function(x) paste0(x, "%"))
    ggplot(aes(x = date, y = value, col = state)) +
    geom_line()
}
