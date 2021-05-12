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
#'
#' for viz_gr_yth_lfpart_line:
#' ids <- will need smoothing and extracting data from LM1.
#'
#' for viz_gr_yth_emp_line:
#'


Viz_gr_gen_partrate_line <- function(data = filter_dash_data(c("A84423355R",
                                                               "A84423243W",
                                                               "A84423467J"),
                                                               df = dash_data),
                                                               title = "") {

  data %>%
    djpr_ts_linechart() +
    labs(title = title,
         subtitle = "Participation rate by sex, Victoria",
         caption = "Source: ABS Labour Force. Note: seasonally adjusted data.")
}

viz_gr_gen_unemp_line <- function(data = filter_dash_data(c("A84423354L",
                                                            "A84423242V",
                                                            "A84423466F"),
                                                            df = dash_data,
                                                            title = "")) {

  data %>%
    djpr_ts_linechart() +
    labs(title = title,
         subtitle = "Unemployment by sex, Victoria",
         caption = "Source: ABS Labour Force. Note: seasonally adjusted data.")
}

