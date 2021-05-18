
#' Function to create the graphs for the 'Groups' subpage on the dashboard.
#' @param data the dataframe containing data to visualise
#' @examples
#' \dontrun{
#'
#' # for viz_gr_gen_emp_bar:
#' ids <- c(
#'   "A84423349V",
#'   "A84423237A",
#'   "A84423461V",
#'   "A84423357V",
#'   "A84423245A",
#'   "A84423469L",
#'   "A84423350C",
#'   "A84423238C",
#'   "A84423462W"
#' )
#' # (also parttime figures and
#' # not in the labour force)
#'
#' # for viz_gr_gen_partrate_line:
#' ids <- c(
#'   "A84423355R",
#'   "A84423243W",
#'   "A84423467J"
#' )
#'
#' # for viz_gr_gen_unemp_line:
#' ids <- c(
#'   "A84423354L",
#'   "A84423242V",
#'   "A84423466F"
#' )
#'
#' # for viz_gr_yth_lfpart_line:
#' # ids <- will need smoothing and extracting data from LM1.
#'
#' # for viz_gr_yth_emp_line:
#' }
viz_gr_gen_emp_bar <- function(data = filter_dash_data(c(
                                 "A84423469L",
                                 "A84423245A",
                                 "A84423801C",
                                 "A84423577W",
                                 "A84423461V",
                                 "A84423237A",
                                 "A84423463X",
                                 "A84423239F",
                                 "A84423462W",
                                 "A84423238C"
                               ), df = dash_data) %>%
                                 dplyr::group_by(.data$series) %>%
                                 dplyr::filter(.data$date == max(.data$date)),
                               title = "") {
  df <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(sex, indicator, value) %>%
    tidyr::spread(key = indicator, value = value) %>%
    dplyr::mutate(
      `Not in the labour force` = `Civilian population aged 15 years and over` -
        `Labour force total`,
      `Employed part-time` = `Employed total` - `Employed full-time`
    ) %>%
    dplyr::select(
      -`Civilian population aged 15 years and over`,
      -`Employed total`,
      -`Labour force total`
    ) %>%
    dplyr::rename(Unemployed = `Unemployed total`) %>%
    tidyr::gather(
      key = indicator, value = value,
      -sex
    ) %>%
    dplyr::mutate(indicator = factor(indicator,
      levels = c(
        "Not in the labour force",
        "Unemployed",
        "Employed part-time",
        "Employed full-time"
      )
    ))

  df <- df %>%
    dplyr::arrange(sex, desc(indicator)) %>%
    dplyr::group_by(sex) %>%
    dplyr::mutate(perc = value / sum(value),
                  label_y = cumsum(perc) - (perc / 2)) %>%
    dplyr::ungroup()

  label_df <- df %>%
    dplyr::filter(sex == "Males") %>%
    dplyr::mutate(label_y = case_when(indicator == "Employed part-time" ~
                                        label_y - 0.1,
                                      indicator == "Not in the labour force" ~
                                        0.92,
                                      TRUE ~ label_y))

  emp_tot <- df %>%
    dplyr::filter(grepl("Employed", .data$indicator)) %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::summarise(emp_tot = sum(.data$perc)) %>%
    tidyr::spread(key = .data$sex, value = .data$emp_tot)

  title <- paste0(round2(emp_tot$Males * 100, 0), " per cent of Victorian men are in paid work, but only ",
                  round2(emp_tot$Females * 100, 0), " per cent of women")

  df %>%
    ggplot(aes(x = sex, y = value, fill = indicator)) +
    geom_col(position = "fill",
             alpha = 1,
             col = "grey70") +
    geom_text(
      aes(y = label_y, label = round2(perc * 100, 1)),
      size = 16 / .pt,
      colour = "white") +
    # ggrepel::geom_text_repel(
    geom_text(
      data = label_df,
              aes(y = label_y,
                  col = indicator,
                  label = stringr::str_wrap(indicator, 12)),
      size = 14 / .pt,
              vjust = 0,
              x = 2.5) +
    coord_flip() +
    theme_djpr() +
    djpr_fill_manual(4) +
    djpr_colour_manual(4) +
    scale_x_discrete(expand = expansion(add = c(0.25, 0.85))) +
    theme(axis.text.x = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank() ) +
    labs(subtitle = paste0("Labour force status by sex, Victoria, per cent of civilian population aged 15+, ",
                           format(max(data$date), "%B %Y"), "."),
         caption = caption_lfs(),
         title = title)
}


viz_gr_gen_partrate_line <- function(data = filter_dash_data(c(
                                       "A84423355R",
                                       "A84423243W",
                                       "A84423467J"
                                     ),
                                     df = dash_data
                                     )) {

  df <- data %>%
    dplyr::mutate(sex = dplyr::if_else(sex == "", "Persons", sex))

  change_by_sex <- df %>%
    dplyr::filter(sex != "Persons") %>%
    dplyr::group_by(sex) %>%
    dplyr::mutate(d_annual = value - lag(value, 12)) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::select(sex, d_annual) %>%
    tidyr::spread(key = sex, value  = d_annual)

  max_date <- format(max(df$date), "%B %Y")

  title <- dplyr::case_when(
    change_by_sex$Females > 0 & change_by_sex$Males > 0 ~
      paste0("Labour force participation rose for both men and women in the year to ",
             max_date),
    change_by_sex$Females > 0 & change_by_sex$Males < 0 ~
      paste0("Labour force participation rose for women but fell for men in the year to ",
             max_date),
    change_by_sex$Females < 0 & change_by_sex$Males < 0 ~
      paste0("Labour force participation fell for both women and men in the year to ",
             max_date),
    change_by_sex$Females < 0 & change_by_sex$Males > 0 ~
      paste0("Labour force participation rose for men but fell for women in the year to ",
             max_date)
  )

  df %>%
    djpr_ts_linechart(col_var = sex) +
    labs(
      title = title,
      subtitle = "Participation rate by sex, Victoria",
      caption = caption_lfs()
    )
}

viz_gr_gen_unemp_line <- function(data = filter_dash_data(c(
                                    "A84423354L",
                                    "A84423242V",
                                    "A84423466F"
                                  ),
                                  df = dash_data
                                  )
                                  ) {

  df <- data %>%
    dplyr::mutate(sex = dplyr::if_else(sex == "", "Persons", sex))

  current_ur <- df %>%
    dplyr::filter(sex != "Persons", date == max(date)) %>%
    dplyr::select(value, sex) %>%
    tidyr::spread(key = sex, value = value)

  max_date <- format(max(df$date), "%B %Y")

  title <- dplyr::case_when(
    current_ur$Females < current_ur$Males ~
      paste0("The unemployment rate for women was a little lower than the rate for men in ",
             max_date),
    current_ur$Females > current_ur$Males ~
      paste0("The unemployment rate for women was a little higher than the rate for men in ",
             max_date),
    TRUE ~ paste0("The unemployment rate for men and women was around the same level in ",
                  max_date)
  )

  df %>%
    djpr_ts_linechart(col = sex) +
    labs(
      title = title,
      subtitle = "Unemployment by sex, Victoria",
      caption = caption_lfs()
    )
}

# viz_gr_yth_table - not done yet
# big table with demographics figures and %

viz_gr_yth_lfpartrate_line <- function(data = filter_dash_data(c(
                                         "A84424692W",
                                         "15-24_greater melbourne_employed",
                                         "25-54_greater melbourne_employed",
                                         "55+_greater melbourne_employed",
                                         "15-24_rest of vic._employed",
                                         "25-54_rest of vic._employed",
                                         "55+_rest of vic._employed",
                                         "15-24_greater melbourne_nilf",
                                         "25-54_greater melbourne_nilf",
                                         "55+_greater melbourne_nilf",
                                         "15-24_rest of vic._nilf",
                                         "25-54_rest of vic._nilf",
                                         "55+_rest of vic._nilf",
                                         "15-24_greater melbourne_unemployed",
                                         "25-54_greater melbourne_unemployed",
                                         "55+_greater melbourne_unemployed",
                                         "15-24_rest of vic._unemployed",
                                         "25-54_rest of vic._unemployed",
                                         "55+_rest of vic._unemployed",
                                         "A84424622R"
                                       ), df = dash_data) %>%
                                         dplyr::group_by(series_id) %>%
                                         dplyr::mutate(value = zoo::rollmeanr(value, 12, fill = NA)),
                                       title = "") {
  data <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = (100 * (value[series_id == "15-24_greater melbourne_employed"] +
      value[series_id == "15-24_rest of vic._employed"] +
      value[series_id == "15-24_greater melbourne_unemployed"] +
      value[series_id == "15-24_rest of vic._unemployed"]) /
      (value[series_id == "15-24_greater melbourne_employed"] +
        value[series_id == "15-24_rest of vic._employed"] +
        value[series_id == "15-24_greater melbourne_unemployed"] +
        value[series_id == "15-24_rest of vic._unemployed"] +
        value[series_id == "15-24_greater melbourne_nilf"] +
        value[series_id == "15-24_rest of vic._nilf"]))) %>%
    dplyr::mutate(
      series = "Participation rate; 15-24; Victoria",
      series_id = "partrate_15-14_vic",
      indicator = "Participation rate",
      age = "15-24"
    ) %>%
    dplyr::bind_rows(data)

  data <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = (100 * (value[series_id == "25-54_greater melbourne_employed"] +
      value[series_id == "25-54_rest of vic._employed"] +
      value[series_id == "25-54_greater melbourne_unemployed"] +
      value[series_id == "25-54_rest of vic._unemployed"]) /
      (value[series_id == "25-54_greater melbourne_employed"] +
        value[series_id == "25-54_rest of vic._employed"] +
        value[series_id == "25-54_greater melbourne_unemployed"] +
        value[series_id == "25-54_rest of vic._unemployed"] +
        value[series_id == "25-54_greater melbourne_nilf"] +
        value[series_id == "25-54_rest of vic._nilf"]))) %>%
    dplyr::mutate(
      series = "Participation rate; 25-54; Victoria",
      series_id = "partrate_25-54_vic",
      indicator = "Participation rate",
      age = "25-54"
    ) %>%
    dplyr::bind_rows(data)

  data <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = (100 * (value[series_id == "55+_greater melbourne_employed"] +
      value[series_id == "55+_rest of vic._employed"] +
      value[series_id == "55+_greater melbourne_unemployed"] +
      value[series_id == "55+_rest of vic._unemployed"]) /
      (value[series_id == "55+_greater melbourne_employed"] +
        value[series_id == "55+_rest of vic._employed"] +
        value[series_id == "55+_greater melbourne_unemployed"] +
        value[series_id == "55+_rest of vic._unemployed"] +
        value[series_id == "55+_greater melbourne_nilf"] +
        value[series_id == "55+_rest of vic._nilf"]))) %>%
    dplyr::mutate(
      series = "Participation rate; 55+; Victoria",
      series_id = "partrate_55+_vic",
      indicator = "Participation rate",
      age = "55+"
    ) %>%
    dplyr::bind_rows(data)

  # drop rows we don't need
  data <- dplyr::filter(data, .data$indicator == "Participation rate")

  # draw line graph
  data %>%
    ungroup() %>%
    djpr_ts_linechart() +
    scale_y_continuous(
      breaks = scales::breaks_pretty(5),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = title,
      subtitle = "Labour force participation rate by age",
      caption = "Source: ABS Labour Force. Note: 12 month average."
    )
}


viz_gr_yth_emp_sincecovid_line <- function(data = filter_dash_data(c(
                                             "15-24_greater melbourne_employed",
                                             "25-54_greater melbourne_employed",
                                             "55+_greater melbourne_employed",
                                             "15-24_rest of vic._employed",
                                             "25-54_rest of vic._employed",
                                             "55+_rest of vic._employed"
                                           ),
                                           df = dash_data
                                           ) %>%
                                             dplyr::group_by(series_id) %>%
                                             dplyr::mutate(value = zoo::rollmeanr(value, 12, fill = NA)) %>%
                                             dplyr::filter(date >= as.Date("2020-01-01"))
                                           ) {

  data <- data %>%
    group_by(age, date) %>%
    summarise(value = sum(value))

  # Indexing to Covid start
  data <- data %>%
    dplyr::group_by(age) %>%
    dplyr::mutate(value = 100 * ((value / value[date == as.Date("2020-03-01")]) - 1))

  latest <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::select(-date)

  latest <- latest %>%
    tidyr::spread(key = age, value = value)

  # draw line graph
  data %>%
    djpr_ts_linechart(
      col_var = age,
      label_num = paste0(round(.data$value, 1), "%"),
      hline = 0
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(5),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = "Employment for young people fell much faster after the COVID shock than employment for other Victorians",
      subtitle = "Cumulative change in employment for different age groups since March 2020, per cent",
      caption = paste0(caption_lfs_det_m(), "Data smoothed using a 12 month rolling average.")
    )
}

viz_gr_yth_unemprate_line <- function(data = filter_dash_data(c(
                                        "A84424691V",
                                        "15-24_greater melbourne_unemployed",
                                        "25-54_greater melbourne_unemployed",
                                        "55+_greater melbourne_unemployed",
                                        "15-24_rest of vic._unemployed",
                                        "25-54_rest of vic._unemployed",
                                        "55+_rest of vic._unemployed"
                                      ),
                                      df = dash_data
                                      ) %>%
                                        dplyr::group_by(series_id) %>%
                                        dplyr::mutate(value = zoo::rollmeanr(value, 12, fill = NA)),
                                      title = "") {
  data <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = (value[series_id == "15-24_greater melbourne_unemployed"] +
      value[series_id == "15-24_rest of vic._unemployed"])) %>%
    dplyr::mutate(
      series = "Unemployed; 15-24; Victoria",
      series_id = "unemp_15-24_vic",
      indicator = "Unemployed_vic",
      age = "15-24"
    ) %>%
    dplyr::bind_rows(data)

  data <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = (value[series_id == "25-54_greater melbourne_unemployed"] +
      value[series_id == "25-54_rest of vic._unemployed"])) %>%
    dplyr::mutate(
      series = "Unemployed; 25-54; Victoria",
      series_id = "unemp_25-54_vic",
      indicator = "Unemployed_vic",
      age = "25-54"
    ) %>%
    dplyr::bind_rows(data)

  data <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = (value[series_id == "55+_greater melbourne_unemployed"] +
      value[series_id == "55+_rest of vic._unemployed"])) %>%
    dplyr::mutate(
      series = "Unemployed; 55+; Victoria",
      series_id = "unemp_55+_vic",
      indicator = "Unemployed_vic",
      age = "55+"
    ) %>%
    dplyr::bind_rows(data)

  # drop rows we don't need
  data <- dplyr::filter(data, .data$indicator == "Unemployed_vic")

  # draw line chart
  data %>%
    ungroup() %>%
    djpr_ts_linechart() +
    scale_y_continuous(
      limits = function(x) c(0, x[2]),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = title,
      subtitle = "Unemployment in Victoria by age",
      caption = "Source: ABS Labour Force. Note: 12 month average."
    )
}
