#' Function to create the graphs for the 'Groups' subpage on the dashboard.
#' @param data the dataframe containing data to visualise
#' @examples
#' \dontrun{
#'
#' for Viz_gr_gen_partrate_line:
#' ids <- c("A84423355R",
#'          "A84423243W",
#'          "A84423467J")
#'
#' for viz_gr_gen_unemp_line:
#' ids <- c("A84423354L",
#'          "A84423242V",
#'          "A84423466F")


Viz_gr_gen_partrate_line <- function(data, title = "") {

  df <- data %>%
    dplyr::group_by(series)

  df %>%
    djpr_ts_linechart() +
    labs(title = title)
}

viz_gr_gen_unemp_line <- function(data, title = "") {

  df <- data %>%
    dplyr::group_by(series)

  df %>%
    djpr_ts_linechart() +
    labs(title = title)
}

