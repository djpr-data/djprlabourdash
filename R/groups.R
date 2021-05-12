#' Function to create the graphs for the 'Groups' subpage on the dashboard.
#' @param data the dataframe containing data to visualise
#' @examples
#' \dontrun{
#'
#' ids <- c("A84423355R",
#'          "A84423243W",
#'          "A84423467J")
#'

Viz_gr_gen_partrate_line <- function(data) {

  df <- data %>%
    dplyr::group_by(series)

  df %>%
    djpr_ts_linechart() +
    labs(title = title, subtitile = subtitle)
}
