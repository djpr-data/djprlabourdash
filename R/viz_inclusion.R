
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

# Reusable code for labour force status by Greater Melb/rest of Vic
calc_lfs_age_state_gcc <- function(df = dash_data,
                                   data = filter_dash_data(
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
                                     df
                                   )) {
  data %>%
    dplyr::select(
      .data$gcc_restofstate, .data$date, .data$value,
      .data$age, .data$indicator
    ) %>%
    tidyr::spread(key = .data$indicator, value = .data$value)
}

# Line chart -- youth unemp in Greater Melb v Rest of State -----
viz_gr_yth_melbvrest_line <- function(data = calc_lfs_age_state_gcc(),
                                      selected_indicator = "unemp_rate") {
  vic <- calc_lfs_age_state_gcc() %>%
    # Take a 12m rolling average
    dplyr::group_by(.data$gcc_restofstate, .data$age) %>%
    dplyr::mutate(across(
      c(.data$Employed, .data$NILF, .data$Unemployed),
      ~ zoo::rollmeanr(.x, 12, na.pad = TRUE)
    )) %>%
    dplyr::ungroup() %>%
    # Calculate rates from LF totals
    dplyr::mutate(
      emp_pop = .data$Employed /
        (.data$Employed + .data$NILF + .data$Unemployed),
      unemp_rate = .data$Unemployed /
        (.data$Employed + .data$Unemployed),
      part_rate = (.data$Employed + .data$Unemployed) /
        (.data$Employed + .data$Unemployed + .data$NILF)
    )

  vic <- vic %>%
    dplyr::rename(value = {{ selected_indicator }}) %>%
    dplyr::filter(!is.na(.data$value))

  vic %>%
    dplyr::filter(.data$age == "15-24") %>%
    djpr_ts_linechart(col_var = .data$gcc_restofstate)
}

# Line chart --- unemployment rate by age, Victoria ------
viz_gr_ages_line <- function(data = calc_lfs_age_state_gcc(),
                             selected_indicator = "unemp_rate") {


  # Take 12m rolling ave

  data %>%
    dplyr::group_by(.data$date, .data$age) %>%
    dplyr::summarise(
      Employed = sum(.data$Employed),
      NILF = sum(.data$NILF),
      Unemployed = sum(.data$Unemployed)
    ) %>%
    dplyr::mutate(
      emp_pop = .data$Employed /
        (Employed + NILF + Unemployed),
      unemp_rate = Unemployed / (Employed + Unemployed),
      part_rate = (Employed + Unemployed) /
        (Employed + Unemployed + NILF)
    ) %>%
    dplyr::rename(value = {{ selected_indicator }}) %>%
    dplyr::ungroup() %>%
    djpr_ts_linechart(col_var = .data$age) +
    scale_colour_manual(values = suppressWarnings(djpr_pal(10))[c(2, 5, 10)])
}

# Dot plot -- youth unemployment rate by state -------
viz_gr_youth_states_dot <- function(data = dash_data,
                                    selected_indicator = "unemp_rate") {
  data <- filter_dash_data(c(
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
  ))

  df <- data %>%
    mutate(indicator_short = dplyr::case_when(
      indicator == "Unemployment rate" ~ "unemp_rate",
      indicator == "Participation rate" ~ "part_rate",
      indicator == "Employment to population ratio" ~ "emp_pop"
    ))

  df <- df %>%
      dplyr::filter(indicator_short == selected_indicator)

  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::mutate(
      value = zoo::rollmeanr(value, 12, fill = NA),
      geog = dplyr::if_else(state == "", "Australia", .data$state),
      geog_long = geog,
      geog = strayr::clean_state(geog)
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
    right_join(df, by = "geog")

  df_wide <- df %>%
    dplyr::mutate(date_type = dplyr::if_else(date == min(date),
      "min_date",
      "max_date"
    )) %>%
    select(date_type, value, geog, rank) %>%
    tidyr::spread(key = date_type, value = value) %>%
    dplyr::mutate(arrow_end = dplyr::if_else(max_date > min_date,
      max_date - 0.08,
      max_date + 0.08
    ))


  df %>%
    ggplot(aes(
      x = stats::reorder(geog, rank),
      y = value, col = factor(date)
    )) +
    geom_segment(
      data = df_wide,
      aes(
        x = stats::reorder(geog, rank),
        xend = stats::reorder(geog, rank),
        y = min_date, yend = arrow_end
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
        dplyr::filter(geog == "Vic"),
      aes(label = format(.data$date, "%b %Y")),
      size = 14 / .pt,
      direction = "y",
      min.segment.length = unit(10, "lines"),
      nudge_x = 0.33
    ) +
    coord_flip() +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       breaks = scales::breaks_pretty(4),
                       expand = expansion(add = 0.5)) +
    djpr_colour_manual(2) +
    theme_djpr(flipped = T) +
    theme(axis.title = element_blank())
}


viz_gr_yth_lfpartrate_line <- function(data = filter_dash_data(c(
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
                                         "A84424622R",
                                         "A84424692W"
                                       ), df = dash_data) %>%
                                         dplyr::group_by(.data$series_id) %>%
                                         dplyr::mutate(value = slider::slide_mean(.data$value,
                                                                                 before = 11,
                                                                                 complete = TRUE))) {

  aus_vic_yth <- data %>%
    dplyr::filter(series_id %in% c("A84424622R", "A84424692W")) %>%
    ungroup()

  df <- data %>%
    dplyr::filter(!series_id %in% c("A84424622R", "A84424692W"))

  df <- df %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::group_by(.data$age, .data$indicator, .data$date) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup()

  df <- df %>%
    tidyr::pivot_wider(names_from = .data$indicator,
                       values_from = .data$value) %>%
    dplyr::mutate(value = 100 * ((Employed + Unemployed) /
                    (Employed + Unemployed + NILF))) %>%
    dplyr::select(age, date, value)

  chart_1 <- df %>%
    djpr_ts_linechart(col_var = age)


  chart_2 <- aus_vic_yth %>%
    dplyr::mutate(geog = dplyr::if_else(state == "", "Australia", .data$state)) %>%
    djpr_ts_linechart(col_var = .data$geog,
                      label_num = paste0(round(.data$value, 1), "%"),
                      y_labels = function(x) paste0(x, '%'))


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
                                        dplyr::mutate(value = zoo::rollmeanr(value, 12, fill = NA))) {
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

# viz_gr_ltunemp_line <- function(data = filter_dash_data(c(
#                                   "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
#                                   "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
#                                   "A84423687K"
#                                 )) %>%
#                                   dplyr::group_by(.data$series_id) %>%
#                                   dplyr::mutate(value = zoo::rollmeanr(.data$value, 3, fill = NA)) %>%
#                                   dplyr::ungroup()
#                                 ) {
#   df <- data %>%
#     dplyr::mutate(series = dplyr::if_else(grepl("Unemployed", .data$series),
#       "long-term unemployed",
#       "labour force"
#     )) %>%
#     dplyr::group_by(.data$date, .data$series) %>%
#     dplyr::summarise(value = sum(.data$value))
# }

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

