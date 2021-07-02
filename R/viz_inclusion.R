
#' Function to create the graphs for the 'Inclusion' subpage on the dashboard.
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
#' @noRd
# Bar chart -- LF status by sex ----
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
                                 dplyr::filter(.data$date == max(.data$date))) {
  df <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$sex, .data$indicator, .data$value) %>%
    tidyr::spread(key = .data$indicator, value = .data$value) %>%
    dplyr::mutate(
      `Not in the labour force` = .data$`Civilian population aged 15 years and over` -
        .data$`Labour force total`,
      `Employed part-time` = .data$`Employed total` - .data$`Employed full-time`
    ) %>%
    dplyr::select(
      -.data$`Civilian population aged 15 years and over`,
      -.data$`Employed total`,
      -.data$`Labour force total`
    ) %>%
    dplyr::rename(Unemployed = .data$`Unemployed total`) %>%
    tidyr::gather(
      key = "indicator", value = "value",
      -.data$sex
    ) %>%
    dplyr::mutate(indicator = factor(.data$indicator,
      levels = c(
        "Not in the labour force",
        "Unemployed",
        "Employed part-time",
        "Employed full-time"
      )
    ))

  df <- df %>%
    dplyr::arrange(.data$sex, desc(.data$indicator)) %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::mutate(
      perc = .data$value / sum(.data$value),
      label_y = cumsum(.data$perc) - (.data$perc / 2)
    ) %>%
    dplyr::ungroup()

  label_df <- df %>%
    dplyr::filter(.data$sex == "Males") %>%
    dplyr::mutate(label_y = case_when(
      .data$indicator == "Employed part-time" ~
      .data$label_y - 0.1,
      .data$indicator == "Not in the labour force" ~
      0.92,
      TRUE ~ .data$label_y
    ))

  emp_tot <- df %>%
    dplyr::filter(grepl("Employed", .data$indicator)) %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::summarise(emp_tot = sum(.data$perc)) %>%
    tidyr::spread(key = .data$sex, value = .data$emp_tot)

  title <- paste0(
    round2(emp_tot$Males * 100, 0), " per cent of Victorian men are in paid work, but only ",
    round2(emp_tot$Females * 100, 0), " per cent of women"
  )

  df %>%
    ggplot(aes(x = .data$sex, y = .data$value, fill = .data$indicator)) +
    geom_col(
      position = "fill",
      alpha = 1,
      col = "grey70"
    ) +
    geom_text(
      aes(y = .data$label_y, label = round2(.data$perc * 100, 1)),
      size = 16 / .pt,
      colour = "white"
    ) +
    # ggrepel::geom_text_repel(
    geom_text(
      data = label_df,
      aes(
        y = .data$label_y,
        col = .data$indicator,
        label = stringr::str_wrap(.data$indicator, 12)
      ),
      size = 14 / .pt,
      vjust = 0,
      x = 2.5
    ) +
    coord_flip() +
    theme_djpr() +
    djpr_fill_manual(4) +
    djpr_colour_manual(4) +
    scale_x_discrete(expand = expansion(add = c(0.25, 0.85))) +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank()
    ) +
    labs(
      subtitle = paste0(
        "Labour force status by sex, Victoria, per cent of civilian population aged 15+, ",
        format(max(data$date), "%B %Y"), "."
      ),
      caption = caption_lfs(),
      title = title
    )
}

# Line chart -- LF participation by sex -----
viz_gr_gen_partrate_line <- function(data = filter_dash_data(c(
                                       "A84423355R",
                                       "A84423243W",
                                       "A84423467J"
                                     ),
                                     df = dash_data
                                     )) {
  df <- data %>%
    dplyr::mutate(sex = dplyr::if_else(.data$sex == "", "Persons", .data$sex))

  change_by_sex <- df %>%
    dplyr::filter(.data$sex != "Persons") %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::mutate(d_annual = .data$value - lag(.data$value, 12)) %>%
    dplyr::filter(date == max(.data$date)) %>%
    dplyr::select(.data$sex, .data$d_annual) %>%
    tidyr::spread(key = .data$sex, value = .data$d_annual)

  max_date <- format(max(df$date), "%B %Y")

  title <- dplyr::case_when(
    change_by_sex$Females > 0 & change_by_sex$Males > 0 ~
    paste0(
      "Labour force participation rose for both men and women in the year to ",
      max_date
    ),
    change_by_sex$Females > 0 & change_by_sex$Males < 0 ~
    paste0(
      "Labour force participation rose for women but fell for men in the year to ",
      max_date
    ),
    change_by_sex$Females < 0 & change_by_sex$Males < 0 ~
    paste0(
      "Labour force participation fell for both women and men in the year to ",
      max_date
    ),
    change_by_sex$Females < 0 & change_by_sex$Males > 0 ~
    paste0(
      "Labour force participation rose for men but fell for women in the year to ",
      max_date
    )
  )

  df %>%
    djpr_ts_linechart(col_var = .data$sex) +
    labs(
      title = title,
      subtitle = "Participation rate by sex, Victoria",
      caption = caption_lfs()
    )
}

# Line chart -- unemployment rate by sex ------
viz_gr_gen_unemp_line <- function(data = filter_dash_data(c(
                                    "A84423354L",
                                    "A84423242V",
                                    "A84423466F"
                                  ),
                                  df = dash_data
                                  )) {
  df <- data %>%
    dplyr::mutate(sex = dplyr::if_else(.data$sex == "", "Persons", .data$sex))

  current_ur <- df %>%
    dplyr::filter(.data$sex != "Persons", date == max(.data$date)) %>%
    dplyr::select(.data$value, .data$sex) %>%
    tidyr::pivot_wider(names_from = .data$sex)

  max_date <- format(max(df$date), "%B %Y")

  title <- dplyr::case_when(
    current_ur$Females < current_ur$Males ~
    paste0(
      "The unemployment rate for women was a little lower than the rate for men in ",
      max_date
    ),
    current_ur$Females > current_ur$Males ~
    paste0(
      "The unemployment rate for women was a little higher than the rate for men in ",
      max_date
    ),
    TRUE ~ paste0(
      "The unemployment rate for men and women was around the same level in ",
      max_date
    )
  )

  df %>%
    djpr_ts_linechart(col_var = .data$sex) +
    labs(
      title = title,
      subtitle = "Unemployment by sex, Victoria",
      caption = caption_lfs()
    )
}

# viz_gr_yth_table - not done yet
# big table with demographics figures and %


# Line chart -- youth unemp in Greater Melb v Rest of State -----
viz_gr_yth_melbvrest_line <- function(data = filter_dash_data(
                                        c(
                                          "15-24_greater melbourne_employed",
                                          "15-24_rest of vic._employed",
                                          "15-24_greater melbourne_nilf",
                                          "15-24_rest of vic._nilf",
                                          "15-24_greater melbourne_unemployed",
                                          "15-24_rest of vic._unemployed"
                                        ),
                                        df = dash_data
                                      ),
                                      selected_indicator = "unemp_rate") {
  df <- data %>%
    dplyr::filter(.data$age == "15-24") %>%
    dplyr::select(
      .data$gcc_restofstate, .data$date, .data$value,
      .data$indicator
    )

  # Take 12m rolling ave
  df <- df %>%
    dplyr::group_by(.data$gcc_restofstate, .data$indicator) %>%
    dplyr::mutate(value = zoo::rollmeanr(.data$value, 12, na.pad = TRUE)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()


  # Go from long to wide
  df <- df %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value
    )

  # Calculate ratios
  df <- df %>%
    dplyr::mutate(
      emp_pop = .data$Employed /
        (.data$Employed + .data$NILF + .data$Unemployed),
      unemp_rate = .data$Unemployed /
        (.data$Employed + .data$Unemployed),
      part_rate = (.data$Employed + .data$Unemployed) /
        (.data$Employed + .data$Unemployed + .data$NILF)
    )


  df <- df %>%
    dplyr::rename(value = {{ selected_indicator }}) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::select(
      .data$gcc_restofstate, .data$date,
      .data$value
    )

  df %>%
    dplyr::mutate(gcc_restofstate = gsub("Melbourne", "Melb.", .data$gcc_restofstate,
      fixed = TRUE
    )) %>%
    djpr_ts_linechart(col_var = .data$gcc_restofstate) +
    scale_colour_manual(values = suppressWarnings(
      djpr_pal(10)[c(5, 10)]
    ))
}

# Line chart --- unemployment rate by age, Victoria ------
viz_gr_ages_line <- function(data = filter_dash_data(
                               c(
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
                                 "55+_rest of vic._unemployed"
                               ),
                               df = dash_data
                             ),
                             selected_indicator = "unemp_rate") {
  df <- data %>%
    dplyr::select(
      .data$gcc_restofstate, .data$date, .data$value,
      .data$age, .data$indicator
    )

  # Take 12m rolling ave
  df <- df %>%
    dplyr::group_by(.data$gcc_restofstate, .data$age, .data$indicator) %>%
    dplyr::mutate(value = zoo::rollmeanr(.data$value, 12, na.pad = TRUE)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  # Collapse Greater Melb + Rest of Vic into one series
  df <- df %>%
    dplyr::group_by(.data$date, .data$age, .data$indicator) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup()

  # Go from long to wide
  df <- df %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value
    )

  # Calculate ratios
  df <- df %>%
    dplyr::mutate(
      emp_pop = .data$Employed /
        (.data$Employed + .data$NILF + .data$Unemployed),
      unemp_rate = .data$Unemployed /
        (.data$Employed + .data$Unemployed),
      part_rate = (.data$Employed + .data$Unemployed) /
        (.data$Employed + .data$Unemployed + .data$NILF)
    ) %>%
    dplyr::rename(value = {{ selected_indicator }}) %>%
    dplyr::select(.data$date, .data$age, .data$value)

  df %>%
    djpr_ts_linechart(col_var = .data$age)
}

# Dot plot -- youth unemployment rate by state -------
viz_gr_youth_states_dot <- function(data = filter_dash_data(c(
                                      "A84433601W",
                                      "A84433602X",
                                      "A84433603A",
                                      "A84433505W",
                                      "A84433503T",
                                      "A84433504V",
                                      "A84433519K",
                                      "A84433517F",
                                      "A84433518J",
                                      "A84433533F",
                                      "A84433531A",
                                      "A84433532C",
                                      "A84433617R",
                                      "A84433615K",
                                      "A84433616L",
                                      "A84433575C",
                                      "A84433573X",
                                      "A84433574A",
                                      "A84433547V",
                                      "A84433545R",
                                      "A84433546T",
                                      "A84433589T",
                                      "A84433587L",
                                      "A84433588R",
                                      "A84433561R",
                                      "A84433559C",
                                      "A84433560L"
                                    ), df = dash_data),
                                    selected_indicator = "unemp_rate") {
  df <- data %>%
    mutate(indicator_short = dplyr::case_when(
      .data$indicator == "Unemployment rate" ~ "unemp_rate",
      .data$indicator == "Participation rate" ~ "part_rate",
      .data$indicator == "Employment to population ratio" ~ "emp_pop"
    ))

  df <- df %>%
    dplyr::filter(.data$indicator_short == selected_indicator)

  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::mutate(
      value = zoo::rollmeanr(.data$value, 12, fill = NA),
      geog = dplyr::if_else(.data$state == "",
        "Australia",
        .data$state
      ),
      geog_long = .data$geog,
      geog = strayr::clean_state(.data$geog)
    ) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::filter(.data$date %in% c(
      max(.data$date),
      subtract_years(max(.data$date), 1)
    ))

  df <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::mutate(rank = dplyr::min_rank(-.data$value)) %>%
    dplyr::select(.data$rank, .data$geog) %>%
    dplyr::right_join(df, by = "geog")

  df_wide <- df %>%
    dplyr::mutate(date_type = dplyr::if_else(.data$date == min(.data$date),
      "min_date",
      "max_date"
    )) %>%
    dplyr::select(.data$date_type, .data$value, .data$geog, .data$rank) %>%
    tidyr::spread(key = .data$date_type, value = .data$value) %>%
    dplyr::mutate(arrow_end = dplyr::if_else(.data$max_date > .data$min_date,
      .data$max_date - 0.08,
      .data$max_date + 0.08
    ))


  df %>%
    ggplot(aes(
      x = stats::reorder(.data$geog, .data$rank),
      y = .data$value,
      col = factor(.data$date)
    )) +
    geom_segment(
      data = df_wide,
      aes(
        x = stats::reorder(.data$geog, .data$rank),
        xend = stats::reorder(.data$geog, .data$rank),
        y = .data$min_date, yend = .data$arrow_end
      ),
      arrow = arrow(
        angle = 25,
        length = unit(0.5, "lines"),
        type = "closed"
      ),
      inherit.aes = FALSE
    ) +
    ggiraph::geom_point_interactive(
      size = 4,
      aes(tooltip = paste0(
        .data$geog,
        "\n",
        .data$date,
        "\n",
        round2(.data$value, 1)
      ))
    ) +
    ggrepel::geom_text_repel(
      data = df %>%
        dplyr::filter(.data$geog == "Vic"),
      aes(label = format(.data$date, "%b %Y")),
      size = 14 / .pt,
      direction = "y",
      min.segment.length = unit(10, "lines"),
      nudge_x = 0.33
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      breaks = scales::breaks_pretty(4),
      expand = expansion(add = 0.5)
    ) +
    scale_colour_manual(
      values = suppressWarnings(djpr_pal(10)[c(1, 8)])
    ) +
    theme_djpr(flipped = T) +
    theme(axis.title = element_blank())
}


# viz_gr_yth_lfpartrate_line <- function(data = filter_dash_data(c(
#                                          "A84424692W",
#                                          "15-24_greater melbourne_employed",
#                                          "25-54_greater melbourne_employed",
#                                          "55+_greater melbourne_employed",
#                                          "15-24_rest of vic._employed",
#                                          "25-54_rest of vic._employed",
#                                          "55+_rest of vic._employed",
#                                          "15-24_greater melbourne_nilf",
#                                          "25-54_greater melbourne_nilf",
#                                          "55+_greater melbourne_nilf",
#                                          "15-24_rest of vic._nilf",
#                                          "25-54_rest of vic._nilf",
#                                          "55+_rest of vic._nilf",
#                                          "15-24_greater melbourne_unemployed",
#                                          "25-54_greater melbourne_unemployed",
#                                          "55+_greater melbourne_unemployed",
#                                          "15-24_rest of vic._unemployed",
#                                          "25-54_rest of vic._unemployed",
#                                          "55+_rest of vic._unemployed",
#                                          "A84424622R"
#                                        ), df = dash_data) %>%
#                                          dplyr::group_by(.data$series_id) %>%
#                                          dplyr::mutate(value = zoo::rollmeanr(.data$value, 12, fill = NA))) {
#   data <- data %>%
#     dplyr::group_by(.data$date) %>%
#     dplyr::summarise(value = (100 * (value[series_id == "15-24_greater melbourne_employed"] +
#       value[series_id == "15-24_rest of vic._employed"] +
#       value[series_id == "15-24_greater melbourne_unemployed"] +
#       value[series_id == "15-24_rest of vic._unemployed"]) /
#       (value[series_id == "15-24_greater melbourne_employed"] +
#         value[series_id == "15-24_rest of vic._employed"] +
#         value[series_id == "15-24_greater melbourne_unemployed"] +
#         value[series_id == "15-24_rest of vic._unemployed"] +
#         value[series_id == "15-24_greater melbourne_nilf"] +
#         value[series_id == "15-24_rest of vic._nilf"]))) %>%
#     dplyr::mutate(
#       series = "Participation rate; 15-24; Victoria",
#       series_id = "partrate_15-14_vic",
#       indicator = "Participation rate",
#       age = "15-24"
#     ) %>%
#     dplyr::bind_rows(data)
#
#   data <- data %>%
#     dplyr::group_by(.data$date) %>%
#     dplyr::summarise(value = (100 * (value[series_id == "25-54_greater melbourne_employed"] +
#       value[series_id == "25-54_rest of vic._employed"] +
#       value[series_id == "25-54_greater melbourne_unemployed"] +
#       value[series_id == "25-54_rest of vic._unemployed"]) /
#       (value[series_id == "25-54_greater melbourne_employed"] +
#         value[series_id == "25-54_rest of vic._employed"] +
#         value[series_id == "25-54_greater melbourne_unemployed"] +
#         value[series_id == "25-54_rest of vic._unemployed"] +
#         value[series_id == "25-54_greater melbourne_nilf"] +
#         value[series_id == "25-54_rest of vic._nilf"]))) %>%
#     dplyr::mutate(
#       series = "Participation rate; 25-54; Victoria",
#       series_id = "partrate_25-54_vic",
#       indicator = "Participation rate",
#       age = "25-54"
#     ) %>%
#     dplyr::bind_rows(data)
#
#   data <- data %>%
#     dplyr::group_by(.data$date) %>%
#     dplyr::summarise(value = (100 * (value[series_id == "55+_greater melbourne_employed"] +
#       value[series_id == "55+_rest of vic._employed"] +
#       value[series_id == "55+_greater melbourne_unemployed"] +
#       value[series_id == "55+_rest of vic._unemployed"]) /
#       (value[series_id == "55+_greater melbourne_employed"] +
#         value[series_id == "55+_rest of vic._employed"] +
#         value[series_id == "55+_greater melbourne_unemployed"] +
#         value[series_id == "55+_rest of vic._unemployed"] +
#         value[series_id == "55+_greater melbourne_nilf"] +
#         value[series_id == "55+_rest of vic._nilf"]))) %>%
#     dplyr::mutate(
#       series = "Participation rate; 55+; Victoria",
#       series_id = "partrate_55+_vic",
#       indicator = "Participation rate",
#       age = "55+"
#     ) %>%
#     dplyr::bind_rows(data)
#
#   # drop rows we don't need
#   data <- dplyr::filter(data, .data$indicator == "Participation rate")
#
#   # draw line graph
#   data %>%
#     dplyr::filter(!is.na(.data$value)) %>%
#     dplyr::ungroup() %>%
#     djpr_ts_linechart() +
#     scale_y_continuous(
#       breaks = scales::breaks_pretty(5),
#       labels = function(x) paste0(x, "%")
#     ) +
#     labs(
#       title = title,
#       subtitle = "Labour force participation rate by age",
#       caption = "Source: ABS Labour Force. Note: 12 month average."
#     )
# }


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
                                             dplyr::group_by(.data$series_id) %>%
                                             dplyr::mutate(value = zoo::rollmeanr(.data$value, 12, fill = NA)) %>%
                                             dplyr::filter(.data$date >= as.Date("2020-01-01"))) {
  data <- data %>%
    dplyr::group_by(.data$age, .data$date) %>%
    dplyr::summarise(value = sum(.data$value))

  # Indexing to Covid start
  data <- data %>%
    dplyr::group_by(.data$age) %>%
    dplyr::mutate(value = 100 * ((.data$value /
      .data$value[.data$date == as.Date("2020-03-01")]) - 1))

  latest <- data %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(-.data$date)

  latest <- latest %>%
    tidyr::spread(key = .data$age, value = .data$value)

  # draw line graph
  data %>%
    djpr_ts_linechart(
      col_var = .data$age,
      label_num = paste0(round2(.data$value, 1), "%"),
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
                                        dplyr::group_by(.data$series_id) %>%
                                        dplyr::mutate(value = zoo::rollmeanr(.data$value, 12, fill = NA))) {
  data <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = (.data$value[.data$series_id == "15-24_greater melbourne_unemployed"] +
      .data$value[.data$series_id == "15-24_rest of vic._unemployed"])) %>%
    dplyr::mutate(
      series = "Unemployed; 15-24; Victoria",
      series_id = "unemp_15-24_vic",
      indicator = "Unemployed_vic",
      age = "15-24"
    ) %>%
    dplyr::bind_rows(data)

  data <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = (.data$value[.data$series_id == "25-54_greater melbourne_unemployed"] +
      .data$value[.data$series_id == "25-54_rest of vic._unemployed"])) %>%
    dplyr::mutate(
      series = "Unemployed; 25-54; Victoria",
      series_id = "unemp_25-54_vic",
      indicator = "Unemployed_vic",
      age = "25-54"
    ) %>%
    dplyr::bind_rows(data)

  data <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = (.data$value[.data$series_id == "55+_greater melbourne_unemployed"] +
      .data$value[.data$series_id == "55+_rest of vic._unemployed"])) %>%
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
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup() %>%
    djpr_ts_linechart() +
    scale_y_continuous(
      limits = function(x) c(0, x[2]),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = "",
      subtitle = "Unemployment in Victoria by age",
      caption = "Source: ABS Labour Force. Note: 12 month average."
    )
}

viz_gr_emppopratio_line <- function(data = filter_dash_data(c(
                                      "A84423356T",
                                      "A84423244X",
                                      "A84423468K"
                                    ))) {
  df <- data %>%
    dplyr::mutate(sex = dplyr::if_else(.data$sex == "",
      "Persons",
      .data$sex
    ))

  latest_year <- df %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::mutate(d_year = .data$value - dplyr::lag(.data$value, 12)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(.data$date, .data$sex, .data$d_year) %>%
    tidyr::spread(key = .data$sex, value = .data$d_year)

  nice_date <- format(latest_year$date, "%B %Y")

  title <- dplyr::case_when(
    latest_year$Females > 0 &
      latest_year$Males > 0 ~
    paste0(
      "A larger proportion of Victorian men and women are in work in ",
      nice_date, " than a year earlier"
    ),
    latest_year$Females > 0 &
      latest_year$Males < 0 ~
    paste0(
      "The proportion of Victorian women in work rose over the year to ",
      nice_date, " but the male employment to population ratio fell"
    ),
    latest_year$Females < 0 &
      latest_year$Males > 0 ~
    paste0(
      "The proportion of Victorian men in work rose over the year to ",
      nice_date, " but the female employment to population ratio fell"
    ),
    TRUE ~ "Employment to population ratio for Victorian men and women"
  )

  df %>%
    djpr_ts_linechart(
      col_var = .data$sex,
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = title,
      subtitle = "Employment to population ratio by sex, Victoria",
      caption = caption_lfs()
    )
}


# long-term unemployment

viz_gr_ltunemp_line <- function(data = filter_dash_data(c(
                                  "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
                                  "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
                                  "A84423687K",
                                  "A84423089K",
                                  "A84597681W"
                                ),
                                df = dash_data
                                )) {

  # selecting data for Victoria and rename data
  data_vic <- data %>%
    dplyr::filter(grepl("Victoria", series)) %>%
    dplyr::select(series, value, date, state)

  data_vic <- data_vic %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = slider::slide_mean(value,
      before = 2,
      complete = TRUE
    )) %>%
    dplyr::ungroup()

  data_vic <- data_vic %>%
    tidyr::pivot_wider(
      names_from = series,
      values_from = value
    ) %>%
    dplyr::rename(
      Un_2years_over = "Unemployed total ('000) ; Victoria ; 104 weeks and over (2 years and over)",
      Un_1_2years = "Unemployed total ('000) ; Victoria ; 52 weeks and under 104 weeks (1-2 years)",
      labour_force = "Labour force total ;  Persons ;  > Victoria ;"
    ) %>%
    dplyr::filter(!is.na(.data$Un_2years_over)) %>%
    dplyr::mutate(lt_unemp = Un_2years_over + Un_1_2years) %>%
    dplyr::select(date, state, labour_force, lt_unemp)

  # create data frame for Australia
  data_Aus <- data %>%
    dplyr::filter(!grepl("Victoria", series)) %>%
    dplyr::mutate(state = "Australia") %>%
    dplyr::select(series, value, date, state) %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(value = slider::slide_mean(value,
      before = 2,
      complete = TRUE
    )) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = series,
      values_from = value
    ) %>%
    dplyr::select(date, state, "Labour force total ;  Persons ;  Australia ;", "52 weeks and over (Long-term unemployed) ;  Unemployed total ;  Persons ;") %>%
    dplyr::rename(
      lt_unemp = "52 weeks and over (Long-term unemployed) ;  Unemployed total ;  Persons ;",
      labour_force = "Labour force total ;  Persons ;  Australia ;"
    ) %>%
    dplyr::filter(!is.na(lt_unemp))

  data_lr_un <- dplyr::bind_rows(
    data_vic, data_Aus
  )

  data_lr_un <- data_lr_un %>%
    dplyr::mutate(value = 100 * (lt_unemp) / labour_force) %>%
    dplyr::select(date, state, value)

  latest_values <- data_lr_un %>%
    filter(date == max(date)) %>%
    mutate(
      value = round2(value, 1),
      date = format(date, "%B %Y")
    ) %>%
    select(state, value, date) %>%
    tidyr::pivot_wider(names_from = state, values_from = value)

  title <- dplyr::case_when(
    latest_values$Victoria > latest_values$Australia ~
    paste0("Victoria's long-term unemployment rate in ", latest_values$date, " was higher than Australia's"),
    latest_values$Victoria < latest_values$Australia ~
    paste0("Victoria's long-term unemployment rate in ", latest_values$date, " was lower than Australia's"),
    latest_values$Victoria == latest_values$Australia ~
    paste0("Victoria's long-term unemployment rate in ", latest_values$date, " was the same as Australia's"),
    TRUE ~ "Long-term unemployment rate in Victoria and Australia"
  )

  data_lr_un %>%
    djpr_ts_linechart(
      col_var = state,
      label_num = paste0(round2(.data$value, 1), "%")
    ) +
    labs(
      subtitle = "Long-term unemployment rate in Victoria and Australia, per cent of labour force",
      caption = caption_lfs_det_m(),
      title = title
    ) +
    scale_y_continuous(
      limits = function(x) c(0, x[2]),
      labels = function(x) paste0(x, "%"),
      breaks = scales::breaks_pretty(5),
      expand = expansion(mult = c(0, 0.05))
    )
}


viz_gr_ltunvic_bar <- function(data = filter_dash_data(c(
                                 "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
                                 "unemployed total ('000)_victoria_13 weeks and under 26 weeks (3-6 months)",
                                 "unemployed total ('000)_victoria_26 weeks and under 52 weeks (6-12 months)",
                                 "unemployed total ('000)_victoria_4 weeks and under 13 weeks (1-3 months)",
                                 "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
                                 "unemployed total ('000)_victoria_under 4 weeks (under 1 month)"
                               ),
                               df = dash_data
                               )) {
  # select the series you need for the bar chart
  data <- data %>%
    dplyr::select(duration, value, date)

  # take 3 month moving average
  data <- data %>%
    dplyr::group_by(.data$duration) %>%
    dplyr::mutate(value = slider::slide_mean(value,
      before = 2,
      complete = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(value))

  data <- data %>%
    tidyr::pivot_wider(
      names_from = duration,
      values_from = value
    )

  # create short name
  data <- data %>%
    dplyr::rename(
      "2+ years" = "104 weeks and over (2 years and over)",
      "1-2 years" = "52 weeks and under 104 weeks (1-2 years)",
      "6-12 months" = "26 weeks and under 52 weeks (6-12 months)",
      "3-6 months" = "13 weeks and under 26 weeks (3-6 months)",
      "1-3 months" = "4 weeks and under 13 weeks (1-3 months)",
      "<1 month" = "Under 4 weeks (under 1 month)"
    )

  # arrange the latest and the previous period data
  df_data <- data %>%
    tidyr::pivot_longer(!date,
      names_to = "duration",
      values_to = "value"
    )

  df_data <- df_data %>%
    dplyr::filter(date %in% c(max(date),
                              subtract_years(max(date), 1)))

  df_data <- df_data %>%
    dplyr::mutate(duration = factor(duration,
                                    levels = c(
                                      "<1 month",
                                      "1-3 months",
                                      "3-6 months",
                                      "6-12 months",
                                      "1-2 years",
                                      "2+ years"
                                    ),
                                    ordered = TRUE
                                    ))

  title_df <- df_data %>%
    dplyr::group_by(.data$duration) %>%
    dplyr::mutate(d_month = 100 * ((.data$value / dplyr::lag(.data$value, 1)) - 1)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(date, duration, d_month) %>%
    dplyr::mutate(
      value = round2(.data$d_month, 1),
      date = format(.data$date, "%B %Y")
    ) %>%
    dplyr::select(
      .data$duration,
      .data$value,
      .data$date
    ) %>%
    tidyr::pivot_wider(
      names_from = "duration",
      values_from = "value"
    )


  latest_month <- format(max(data$date), "%B %Y")

  # create a title

  title <- dplyr::case_when(
    title_df$`2+ years` < title_df$`<1 month` &
      title_df$`2+ years` < title_df$`1-3 months` & title_df$`2+ years` < title_df$`3-6 months` &
      title_df$`2+ years` < title_df$`6-12 months` & title_df$`2+ years` < title_df$`1-2 years` ~
    paste0("Two years and over unemployed Victorians had a high rate of percentage decline in ", latest_month, "."),
    title_df$`1-2 years` < title_df$`<1 month` &
      title_df$`1-2 years` < title_df$`1-3 months` & title_df$`1-2 years` < title_df$`3-6 months` &
      title_df$`1-2 years` < title_df$`6-12 months` & title_df$`1-2 years` < title_df$`2+ years` ~
    paste0("One years and under two years Victorians had a high rate of percentage decline in ", latest_month, "."),
    title_df$`6-12 months` < title_df$`<1 month` &
      title_df$`6-12 months` < title_df$`1-3 months` & title_df$`6-12 months` < title_df$`3-6 months` &
      title_df$`6-12 months` < title_df$`1-2 years` & title_df$`6-12 months` < title_df$`2+ years` ~
    paste0("Six months and under 12 months unemployed Victorians had a high rate of percentage decline in ", latest_month, "."),
    title_df$`3-6 months` < title_df$`<1 month` &
      title_df$`3-6 months` < title_df$`1-3 months` & title_df$`3-6 months` < title_df$`6-12 months` &
      title_df$`3-6 months` < title_df$`1-2 years` & title_df$`3-6 months` < title_df$`2+ years` ~
    paste0("Three months and under six months unemployed Victorians had a high rate of percentage decline in ", latest_month, "."),
    title_df$`1-3 months` < title_df$`<1 month` &
      title_df$`1-3 months` < title_df$`3-6 months` & title_df$`1-3 months` < title_df$`6-12 months` &
      title_df$`1-3 months` < title_df$`1-2 years` & title_df$`1-3 months` < title_df$`2+ years` ~
    paste0("One month and under three months unemployed Victorians had a high rate of percentage decline in ", latest_month, "."),
    title_df$`<1 month` < title_df$`1-3 months` &
      title_df$`<1 month` < title_df$`3-6 months` & title_df$`<1 month` < title_df$`6-12 months` &
      title_df$`<1 month` < title_df$`1-2 years` & title_df$`<1 month` < title_df$`2+ years` ~
    paste0("Under one month unemployed Victorians had a high rate of percentage decline in ", latest_month, "."),
    TRUE ~ "The proportion Unemployed Victorian by duration of unemployment"
  )


  # create chart
  df_data %>%
    dplyr::mutate(date = format(.data$date, "%B %Y"),
                  date = factor(.data$date,
                                levels = rev(sort(unique(.data$date))),
                                ordered = TRUE)) %>%
    ggplot(aes(x = .data$duration, y = .data$value,
               fill = .data$date
               )
           ) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_djpr(flipped = TRUE) +
    djpr_fill_manual(2) +
    geom_text(
      # nudge_y = 1.5,
      # stat = "identity",
      position = position_dodge(width = 1),
      aes(label = paste0(round2(.data$value, 1))),
      colour = "black",
      vjust = 0.5,
      hjust = 0,
      size = 12 / .pt
    ) +
    scale_x_discrete(expand = expansion(add = c(0.25, 0.85))) +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      legend.position = c(0.9, 0.9),
      legend.key.height  = unit(1.5, "lines"),
      legend.key.width = unit(1.5, "lines"),
      legend.direction = "vertical",
      axis.ticks = element_blank()
    ) +
    labs(
      subtitle = paste0(
        " Unemployed Victorians in ('000) by duration of unemployment ",
        format(max(data$date), "%B %Y"), "."
      ),
      caption = paste0(caption_lfs_det_m(), " Data not seasonally adjusted. Smoothed using a 3 month rolling average."),
      title = title
    )
}


# area chart for the proportion of unemployed by duration
viz_gr_ltunvic_area <- function(data = filter_dash_data(c(
                                  "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
                                  "unemployed total ('000)_victoria_13 weeks and under 26 weeks (3-6 months)",
                                  "unemployed total ('000)_victoria_26 weeks and under 52 weeks (6-12 months)",
                                  "unemployed total ('000)_victoria_4 weeks and under 13 weeks (1-3 months)",
                                  "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
                                  "unemployed total ('000)_victoria_under 4 weeks (under 1 month)"
                                ),
                                df = dash_data
                                )) {
  # select the series you need for the bar chart

  data <- data %>%
    dplyr::select(duration, value, date)

  # 3 month moving average
  data <- data %>%
    dplyr::group_by(.data$duration) %>%
    dplyr::mutate(value = slider::slide_mean(value,
      before = 2,
      complete = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(value))


  # Convert duration to factor to order area chart
  data <- data %>%
    dplyr::mutate(
      duration =
        factor(.data$duration,
          levels = c(
            "Under 4 weeks (under 1 month)",
            "4 weeks and under 13 weeks (1-3 months)",
            "13 weeks and under 26 weeks (3-6 months)",
            "26 weeks and under 52 weeks (6-12 months)",
            "52 weeks and under 104 weeks (1-2 years)",
            "104 weeks and over (2 years and over)"
          ),
          ordered = TRUE
        )
    )

  label_df <- data %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::arrange(duration) %>%
    dplyr::mutate(
      perc = value / sum(value),
      cum_perc = cumsum(perc) - (perc / 2),
      label_no_num =
        case_when(
          duration == "Under 4 weeks (under 1 month)" ~
          "<1 month",
          duration == "4 weeks and under 13 weeks (1-3 months)" ~
          "1-3 months",
          duration == "13 weeks and under 26 weeks (3-6 months)" ~
          "3-6 months",
          duration == "26 weeks and under 52 weeks (6-12 months)" ~
          "6-12 months",
          duration == "52 weeks and under 104 weeks (1-2 years)" ~
          "1-2 years",
          duration == "104 weeks and over (2 years and over)" ~
          "2+ years",
          TRUE ~ NA_character_
        ),
      label = paste0(label_no_num, " ", round2(100 * perc, 1), "%")
    )

  title_df <- label_df %>%
    dplyr::select(.data$perc, .data$date, .data$label_no_num) %>%
    tidyr::pivot_wider(names_from = label_no_num, values_from = perc)

  latest_month <- format(max(label_df$date), "%B %Y")



  title <- dplyr::case_when(
    title_df$`2+ years` > title_df$`<1 month` &
      title_df$`2+ years` > title_df$`1-3 months` & title_df$`2+ years` > title_df$`3-6 months` &
      title_df$`2+ years` > title_df$`6-12 months` & title_df$`2+ years` > title_df$`1-2 years` ~
    paste0("The proportion of two years and over unemployed in ", latest_month, " was higher
        than other catagories of duration"),
    title_df$`1-2 years` > title_df$`<1 month` &
      title_df$`1-2 years` > title_df$`1-3 months` & title_df$`1-2 years` > title_df$`3-6 months` &
      title_df$`1-2 years` > title_df$`6-12 months` & title_df$`1-2 years` > title_df$`2+ years` ~
    paste0("The proportion of one years and under two years unemployed in ", latest_month, " was higher
          than other catagories of duration"),
    title_df$`6-12 months` > title_df$`<1 month` &
      title_df$`6-12 months` > title_df$`1-3 months` & title_df$`6-12 months` > title_df$`3-6 months` &
      title_df$`6-12 months` > title_df$`1-2 years` & title_df$`6-12 months` > title_df$`2+ years` ~
    paste0("The proportion of six months and under 12 months unemployed in ", latest_month, " was higher
           than other catagories of duration"),
    title_df$`3-6 months` > title_df$`<1 month` &
      title_df$`3-6 months` > title_df$`1-3 months` & title_df$`3-6 months` > title_df$`6-12 months` &
      title_df$`3-6 months` > title_df$`1-2 years` & title_df$`3-6 months` > title_df$`2+ years` ~
    paste0("The proportion of three months and under six months unemployed in ", latest_month, " was higher than other catagories of duration"),
    title_df$`1-3 months` > title_df$`<1 month` &
      title_df$`1-3 months` > title_df$`3-6 months` & title_df$`1-3 months` > title_df$`6-12 months` &
      title_df$`1-3 months` > title_df$`1-2 years` & title_df$`1-3 months` > title_df$`2+ years` ~
    paste0("The proportion of one month and under three months unemployed in ", latest_month, " was higher than other catagories of duration"),
    title_df$`<1 month` > title_df$`1-3 months` &
      title_df$`<1 month` > title_df$`3-6 months` & title_df$`<1 month` > title_df$`6-12 months` &
      title_df$`<1 month` > title_df$`1-2 years` & title_df$`<1 month` > title_df$`2+ years` ~
    paste0("The proportion of under one months unemployed in ", latest_month, " was higher
           than other catagories of duration"),
    TRUE ~ "The proportion Unemployed Victorian by duration of unemployment")



  data %>%
    ggplot(aes(
      x = .data$date,
      y = .data$value,
      fill = rev(.data$duration)
    )) +
    geom_area(
      colour = NA,
      position = "fill"
    ) +
    theme_djpr() +
    geom_label(
      data = label_df,
      aes(
        x = date,
        y = cum_perc,
        colour = rev(.data$duration),
        label = stringr::str_wrap(label, 11)
      ),
      inherit.aes = FALSE,
      hjust = 0,
      label.padding = unit(0.05, "lines"),
      label.size = 0,
      lineheight = 0.9,
      size = 12 / .pt,
      fill = "white"
    ) +
    djpr_fill_manual(6) +
    djpr_colour_manual(6) +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_text(size = 12)
    ) +
    scale_x_date(
      expand = expansion(mult = c(.02, .2)),
      date_labels = "%b\n %Y",
      breaks = djprtheme::breaks_right(
        limits = c(
          min(data$date),
          max(data$date)
        )
      )
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x * 100, "%"),
      expand = expansion(add = c(0, 0.01))
    ) +
    labs(
      subtitle = "Proportion of unemployed Victorians by unemployment duration",
      caption = paste0(caption_lfs_det_m(), " Data not seasonally adjusted. Smoothed using a 3 month rolling average."),
      title = title
    )
}

# Full-time and part-time  employment growth pattern by gender
viz_gr_full_part_line <- function(data = filter_dash_data(c(
                                    "A84423349V",
                                    "A84423237A",
                                    "A84423461V",
                                    "A84423357V",
                                    "A84423245A",
                                    "A84423469L"
                                  ),
                                  df = dash_data
                                  )) {
  df <- data %>%
    dplyr::select(.data$date, .data$sex, .data$indicator, .data$value) %>%
    tidyr::pivot_wider(names_from = .data$indicator, values_from = .data$value) %>%
    group_by(.data$sex) %>%
    dplyr::mutate(
      `Employed part-time` = .data$`Employed total` - .data$`Employed full-time`
    ) %>%
    dplyr::select(!.data$`Employed total`) %>%
    ungroup()

  # create annual growth Df
  annual_g <- df %>%
    tidyr::pivot_longer(!c(1:2), names_to = "indicator", values_to = "value") %>%
    dplyr::arrange(.data$date) %>%
    dplyr::group_by(.data$indicator, .data$sex) %>%
    dplyr::mutate(annual_g = 100 * ((.data$value / lag(.data$value, 12) - 1))) %>%
    dplyr::filter(.data$sex != "") %>%
    dplyr::select(.data$date, .data$sex, .data$indicator, .data$annual_g) %>%
    dplyr::rename(value = annual_g) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()


  latest_month <- format(max(annual_g$date), "%B %Y")

  # create latest data by gender
  female_latest_f <- annual_g %>%
    dplyr::filter(.data$sex == "Females" & .data$indicator == "Employed full-time" &
      +.data$date == max(.data$date)) %>%
    dplyr::pull(.data$value)
  female_latest_p <- annual_g %>%
    dplyr::filter(.data$sex == "Females" & .data$indicator == "Employed part-time" &
      +.data$date == max(.data$date)) %>%
    dplyr::pull(.data$value)

  male_latest_f <- annual_g %>%
    dplyr::filter(.data$sex == "Males" & .data$indicator == "Employed full-time" &
      +.data$date == max(.data$date)) %>%
    dplyr::pull(.data$value)

  male_latest_p <- annual_g %>%
    dplyr::filter(.data$sex == "Males" & .data$indicator == "Employed part-time" &
      +.data$date == max(.data$date)) %>%
    dplyr::pull(.data$value)


  # create title

  title <- dplyr::if_else(
    female_latest_f > male_latest_f,
    paste0("Female full-time employment growth outpaced males in the 12 months to ", latest_month),
    paste0(" Female full-time employment growth lagged behind males in the 12 months to ", latest_month)
  )

  # female_latest_p > male_latest_p,
  # paste0("Female part-time employment growth outpaced male in the 12 months to ", latest_month),
  # paste0("Female part-time employment growth lagged behind male in the 12 months to ", latest_month))

  # create chart
  annual_g %>%
    djpr_ts_linechart(
      col_var = sex,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%"),
      hline = 0
    ) +
    labs(
      title = title,
      subtitle = "Full-time and part-time employment by sex, Victoria",
      caption = caption_lfs()
    ) +
    facet_wrap(~indicator, ncol = 1, scales = "free_y")
}
