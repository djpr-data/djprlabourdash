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
                                                         "A84423463X"), df = dash_data),
                                                title = "") {

    # create time series for not in labour force and part-time
    data <- data %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(value = value[series_id == "A84423689R"] -
                         value[series_id == "A84423351F"]) %>%
      dplyr::mutate(series = "Not in the labour force ; Persons ; Victoria",
                    series_id = "nilf_vic") %>%
      dplyr::bind_rows(data)

    data <- data %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(value = value[series_id == "A84423577W"] -
                         value[series_id == "A84423239F"]) %>%
      dplyr::mutate(series = "Not in the labour force ; Male ; Victoria",
                    series_id = "nilf_vic_male") %>%
      dplyr::bind_rows(data)

    data <- data %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(value = value[series_id == "A84423801C"] -
                         value[series_id == "A84423463X"]) %>%
      dplyr::mutate(series = "Not in the labour force ; Female ; Victoria",
                    series_id = "nilf_vic_female") %>%
      dplyr::bind_rows(data)

    data <- data %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(value = value[series_id == "A84423237A"] -
                         value[series_id == "A84423245A"]) %>%
      dplyr::mutate(series = "Part time employed ; Male ; Victoria",
                    series_id = "pt_emp_vic_male") %>%
      dplyr::bind_rows(data)

    data <- data %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(value = value[series_id == "A84423461V"] -
                         value[series_id == "A84423469L"]) %>%
      dplyr::mutate(series = "Part time employed ; Female ; Victoria",
                    series_id = "pt_emp_vic_female") %>%
      dplyr::bind_rows(data)

    # draw stacked box plot
    data %>%
    ggplot(aes(x = series, fill = indicator),
               y = value) +
          geom_bar(stat = "count") +
          coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05)) ,
                       breaks = seq(0, 10, 2),
                       labels = function(x) paste0(x, "%")
    ) +
    djprtheme::theme_djpr(flipped = TRUE) +
    labs(title = "",
         subtitle = "Unemployment rate (%) in Victorian regions",
         caption = "Source: ABS Labour Force.")                                                }



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

