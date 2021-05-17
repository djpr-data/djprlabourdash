#' Function to create the graphs for the 'Groups' subpage on the dashboard.
#' @param data the dataframe containing data to visualise
#' @examples
#' \dontrun{
#'
#' for viz_gr_gen_emp_bar:
#' ids <- c("A84423349V",
#'          "A84423237A",
#'          "A84423461V",
#'          "A84423357V",
#'          "A84423245A",
#'          "A84423469L",
#'          "A84423350C",
#'          "A84423238C",
#'          "A84423462W")
#'  (also parttime figures and
#'  not in the labour force)
#'
#' for viz_gr_gen_partrate_line:
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

viz_gr_gen_emp_bar <- function(data = filter_dash_data(c("A84423349V",
                                                         "A84423237A",
                                                         "A84423461V",
                                                         "A84423357V",
                                                         "A84423245A",
                                                         "A84423469L",
                                                         "A84423350C",
                                                         "A84423238C",
                                                         "A84423462W",
                                                         "pt_emp_vic",
                                                         "A84423689R",
                                                         "A84423351F",
                                                         "A84423577W",
                                                         "A84423239F",
                                                         "A84423801C",
                                                         "A84423463X"), df = dash_data) %>%
                                 dplyr::group_by(.data$series) %>%
                                 dplyr::filter(.data$date == max(.data$date)),
                               title = "") {

    # create time series for not in labour force and part-time
    data <- data %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(value = value[series_id == "A84423689R"] -
                         value[series_id == "A84423351F"]) %>%
      dplyr::mutate(series = "Not in the labour force ; Persons ; Victoria",
                    series_id = "nilf_vic",
                    indicator = "Not in labour force") %>%
      dplyr::bind_rows(data)

    data <- data %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(value = value[series_id == "A84423577W"] -
                         value[series_id == "A84423239F"]) %>%
      dplyr::mutate(series = "Not in the labour force ; Male ; Victoria",
                    series_id = "nilf_vic_male",
                    indicator = "Not in labour force",
                    sex = "Males") %>%
      dplyr::bind_rows(data)

    data <- data %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(value = value[series_id == "A84423801C"] -
                         value[series_id == "A84423463X"]) %>%
      dplyr::mutate(series = "Not in the labour force ; Female ; Victoria",
                    series_id = "nilf_vic_female",
                    indicator = "Not in labour force",
                    sex = "Females") %>%
      dplyr::bind_rows(data)

    data <- data %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(value = value[series_id == "A84423237A"] -
                         value[series_id == "A84423245A"]) %>%
      dplyr::mutate(series = "Part time employed ; Male ; Victoria",
                    series_id = "pt_emp_vic_male",
                    indicator = "Employed part-time",
                    sex = "Males") %>%
      dplyr::bind_rows(data)

    data <- data %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(value = value[series_id == "A84423461V"] -
                         value[series_id == "A84423469L"]) %>%
      dplyr::mutate(series = "Part time employed ; Female ; Victoria",
                    series_id = "pt_emp_vic_female",
                    indicator = "Employed part-time",
                    sex = "Females") %>%
      dplyr::bind_rows(data)

    #drop rows we don't need
    data <- dplyr::filter(data, data$indicator %in% c("Employed part-time",
                                                "Not in labour force",
                                                "Unemployed total",
                                                "Employed full-time"))

    # draw stacked box plot
    data %>%
      dplyr::mutate(indicator = factor(indicator,
                                levels = c("Not in labour force",
                                           "Unemployed total",
                                           "Employed part-time",
                                           "Employed full-time"))) %>%
      dplyr::filter(.data$sex != "") %>%
      ggplot(aes(x = sex, y = value, fill = indicator)) +
      geom_bar(stat="identity", position = "fill") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      djprtheme::theme_djpr(flipped = TRUE, legend = "top") +
      djprtheme::djpr_fill_manual(4) +
      theme(axis.title.x = element_blank(),
            panel.grid = element_blank(),
            axis.text.x = element_blank()) +
      labs(title = "",
           subtitle = "Employment by Gender",
           caption = "Source: ABS Labour Force.")
}


viz_gr_gen_partrate_line <- function(data = filter_dash_data(c("A84423355R",
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
                                                            df = dash_data), title = "") {

  data %>%
    djpr_ts_linechart() +
    labs(title = title,
         subtitle = "Unemployment by sex, Victoria",
         caption = "Source: ABS Labour Force. Note: seasonally adjusted data.")
}

viz_gr_yth_lfpart_line <- function(data = filter_dash_data(c("A84424692W",
                                                             "[part_rate 25-54]",
                                                             "[part_rate 55+]",
                                                             "[part_rate 15-24 Australia]"),
                                                           df = dash_data), title = "") {
  data %>%
    djpr_ts_linechart() +
    labs(title = title,
         subtitle = "Labour force participation rate by age",
         caption = "Source: ABS Labour Force. Note: 12 month average.")
}

viz_gr_yth_emp_line <- function(data = filter_dash_data(c("[employment 15-24]",
                                                             "[employment 25-54]",
                                                             "[employment 55+]"),
                                                           df = dash_data), title = "") {

  data %>%
    djpr_ts_linechart() +
    labs(title = title,
         subtitle = "Employment in Victoria by age",
         caption = "Source: ABS Labour Force. Note: 12 month average.")
}


viz_gr_yth_unemp_line <- function(data = filter_dash_data(c("A84424691V",   # unemployment rate vic 15-24
                                                          "[unemployment rate 25-54]",
                                                          "[unemployment rate 55+]",
                                                          "A84424621L"),    # unemployment rate Aus 15-24
                                                        df = dash_data), title = "") {

  data %>%
    djpr_ts_linechart() +
    labs(title = title,
         subtitle = "Unemployment by age",
         caption = "Source: ABS Labour Force. Note: 12 month average.")
}

viz_gr_yth_mostvuln_line <- function(data = filter_dash_data(c("A84424600A",
                                                               "A84424601C",
                                                               "A84424691V"), df = dash_data) %>%
                                        dplyr::group_by(series_id) %>%
                                        dplyr::mutate(value = zoo::rollmeanr(value, 12, fill = NA)) %>%
                                        dplyr::filter(!is.na(value)),
                                      title = "") {

  data %>%
    djpr_ts_linechart() +
    labs(title = title,
         subtitle = "Most vulnerable youth cohort",
         caption = "Source: ABS Labour Force. Note: 12 month average.")
}


