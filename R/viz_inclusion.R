
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
                                 dplyr::filter(.data$date == max(.data$date))) {
  df <- data %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::summarise(
      `Unemployed` = sum(.data$value[.data$indicator == "Unemployed total"]),
      `Employed part-time` = sum(.data$value[.data$indicator == "Employed total"]) -
        sum(.data$value[.data$indicator == "Employed full-time"]),
      `Employed full-time` = sum(.data$value[.data$indicator == "Employed full-time"]),
      `Not in the labour force` = sum(.data$value[.data$indicator == "Civilian population aged 15 years and over"]) -
        sum(.data$value[.data$indicator == "Labour force total"])
    ) %>%
    tidyr::pivot_longer(
      names_to = "indicator", values_to = "value",
      cols = -.data$sex
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
    dplyr::group_by(.data$sex) %>%
    dplyr::arrange(desc(.data$indicator)) %>%
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
    dplyr::filter(grepl("Employed", .data$indicator, fixed = TRUE)) %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::summarise(emp_tot = sum(.data$perc)) %>%
    tidyr::pivot_wider(names_from = .data$sex, values_from = .data$emp_tot)

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
      axis.line = element_blank(),
      axis.ticks = element_blank()
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
    djpr_ts_linechart(
      col_var = .data$sex,
      y_labels = function(x) paste0(x, "%"),
      label_num = paste0(round2(.data$value, 1), "%")
    ) +
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
      "The unemployment rate for women was lower than the rate for men in ",
      max_date
    ),
    current_ur$Females > current_ur$Males ~
    paste0(
      "The unemployment rate for women was higher than the rate for men in ",
      max_date
    ),
    TRUE ~ paste0(
      "The unemployment rate for men and women was around the same level in ",
      max_date
    )
  )

  df %>%
    djpr_ts_linechart(
      col_var = .data$sex,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = title,
      subtitle = "Unemployment by sex, Victoria",
      caption = caption_lfs()
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
                                             dplyr::group_by(.data$series_id) %>%
                                             dplyr::mutate(value = slider::slide_mean(.data$value, before = 11, complete = TRUE)) %>%
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
      subtitle = "Cumulative change in employment for different age groups since March 2020",
      caption = paste0(caption_lfs_det_m(), "Data smoothed using a 12 month rolling average.")
    )
}

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
    dplyr::mutate(value = slider::slide_mean(.data$value, before = 11, complete = TRUE)) %>%
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
    dplyr::rename(value = .env$selected_indicator) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::select(
      .data$gcc_restofstate, .data$date,
      .data$value
    )

  latest_values <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::mutate(value = round2(.data$value * 100, 1)) %>%
    tidyr::pivot_wider(
      names_from = .data$gcc_restofstate,
      values_from = .data$value
    )

  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "unemployment rate",
    selected_indicator == "part_rate" ~ "participation rate",
    selected_indicator == "emp_pop" ~ "employment to population ratio",
    TRUE ~ NA_character_
  )

  title <- paste0(
    "The",
    dplyr::case_when(
      latest_values$`Greater Melbourne` > latest_values$`Rest of Vic.` ~
      paste0(" youth ", indic_long, " was higher in Greater Melbourne than in the rest of Victoria in "),
      latest_values$`Greater Melbourne` < latest_values$`Rest of Vic.` ~
      paste0(" youth ", indic_long, " was higher in rural and regional Victoria than in Greater Melbourne in "),
      latest_values$`Greater Melbourne` == latest_values$`Rest of Vic.` ~
      paste0(" youth ", indic_long, " was the same in Greater Melbourne and the rest of Victoria in "),
      TRUE ~ paste0(" youth ", indic_long, " in Greater Melbourne and the rest of Victoria in ")
    )
  )

  title <- paste0(title, format(latest_values$date, "%B %Y"))

  df %>%
    dplyr::mutate(
      gcc_restofstate = gsub("Melbourne",
        "Melb.",
        .data$gcc_restofstate,
        fixed = TRUE
      ),
      tooltip = paste0(
        .data$gcc_restofstate, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value * 100, 1), "%"
      )
    ) %>%
    djpr_ts_linechart(
      col_var = .data$gcc_restofstate,
      y_labels = function(x) paste0(x * 100, "%"),
      label_num = paste0(round2(.data$value * 100, 1), "%")
    ) +
    scale_colour_manual(values = suppressWarnings(
      djpr_pal(10)[c(5, 10)]
    )) +
    labs(
      title = title,
      subtitle = paste0(stringr::str_to_sentence(indic_long), " for people aged 15-24"),
      caption = paste0(caption_lfs_det_m(), " Smoothed using a 12 month rolling average.")
    )
}

# Line chart --- unemployment rate by age, Victoria ------
youth_focus_box_data <- function() {
  df <- filter_dash_data(
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
  ) %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value, before = 11, complete = TRUE)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$gcc_restofstate, .data$date, .data$value,
      .data$age, .data$indicator
    )

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
    )

  df
}


viz_gr_ages_line <- function(data = youth_focus_box_data(),
                             selected_indicator = "unemp_rate") {
  df <- data %>%
    dplyr::rename(value = selected_indicator) %>%
    dplyr::select(.data$date, .data$age, .data$value)

  latest <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::mutate(value = round2(.data$value * 100, 1)) %>%
    tidyr::pivot_wider(
      names_from = .data$age,
      values_from = .data$value
    )

  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "unemployment rate",
    selected_indicator == "part_rate" ~ "participation rate",
    selected_indicator == "emp_pop" ~ "employment to population ratio",
    TRUE ~ NA_character_
  )

  higher_or_lower <- dplyr::case_when(
    latest$`15-24` > latest$`25-54` ~ "higher than",
    latest$`15-24` < latest$`25-54` ~ "lower than",
    latest$`15-24` == latest$`25-54` ~ "the same as",
    TRUE ~ NA_character_
  )

  diff <- dplyr::if_else(latest$`15-24` != latest$`25-54`,
    paste0(abs(latest$`15-24` - latest$`25-54`), " percentage points "),
    ""
  )

  title <- paste0(
    "The youth ",
    indic_long,
    " is ",
    diff,
    higher_or_lower,
    " the rate for people aged 25-54"
  )

  df %>%
    dplyr::mutate(
      tooltip = paste0(
        .data$age, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value * 100, 1), "%"
      )
    ) %>%
    djpr_ts_linechart(
      col_var = .data$age,
      label_num = paste0(round2(.data$value * 100, 1), "%")
    ) +
    djpr_y_continuous(
      limits = function(x) c(0, x[2]),
      labels = function(x) paste0(x * 100, "%")
    ) +
    labs(
      title = title,
      subtitle = paste0(stringr::str_to_sentence(indic_long), " by age, Victoria"),
      caption = paste0(caption_lfs(), " Smoothed using 12 month moving average.")
    )
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
      value = slider::slide_mean(.data$value, before = 11, complete = TRUE),
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

  latest_values <- df %>%
    dplyr::filter(
      .data$date == max(.data$date),
      .data$geog %in% c("Vic", "Aus")
    ) %>%
    dplyr::select(.data$geog, .data$value, .data$date) %>%
    tidyr::pivot_wider(names_from = .data$geog, values_from = .data$value)

  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "unemployment rate",
    selected_indicator == "part_rate" ~ "participation rate",
    selected_indicator == "emp_pop" ~ "employment to population ratio",
    TRUE ~ NA_character_
  )

  title <- paste0(
    "The youth ",
    indic_long,
    " in Victoria was ",
    round2(latest_values$Vic, 1),
    " per cent in ",
    format(latest_values$date, "%B %Y"),
    ", compared to ",
    round2(latest_values$Aus, 1),
    " per cent across Australia"
  )


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
        format(.data$date, "%B %Y"),
        "\n",
        round2(.data$value, 1), "%"
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
    labs(
      title = title,
      subtitle = paste0(stringr::str_to_sentence(indic_long), " for people aged 15-24, by state and territory"),
      caption = paste0(caption_lfs(), "Data smoothed using a 12 month rolling average."),
      y = paste0("Youth ", indic_long)
    )
}

viz_gr_yth_lfpartrate_vicaus_line <- function(data = filter_dash_data(c(
                                                "A84424622R",
                                                "A84424692W"
                                              ), df = dash_data) %>%
                                                dplyr::group_by(.data$series_id) %>%
                                                dplyr::mutate(value = slider::slide_mean(.data$value,
                                                  before = 11, complete = TRUE
                                                ))) {
  latest <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$date == max(.data$date)
    ) %>%
    dplyr::select(.data$value, .data$series, ) %>%
    dplyr::mutate(value = paste0(round2(.data$value, 1), " per cent")) %>%
    tidyr::spread(key = .data$series, value = .data$value)

  title <- paste0(
    "The participation rate for Victorian youth was ",
    latest$`> Victoria ;  Participation rate ;`,
    " while the rate for youth in Australia was ",
    latest$`Australia ;  Participation rate ;`,
    " in ",
    format(max(data$date), "%B %Y")
  )

  data %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::mutate(geog = dplyr::if_else(.data$state == "", "Australia", .data$state)) %>%
    djpr_ts_linechart(
      col_var = .data$geog,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = title,
      subtitle = "Labour force participation rate, per cent of civilians aged 15-24",
      caption = paste0(caption_lfs(), " Data not seasonally adjusted. Smoothed using a 12 month rolling average.")
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

# viz_gr_yth_unemprate_line <- function(data = filter_dash_data(c(
#                                         "A84424691V",
#                                         "15-24_greater melbourne_unemployed",
#                                         "25-54_greater melbourne_unemployed",
#                                         "55+_greater melbourne_unemployed",
#                                         "15-24_rest of vic._unemployed",
#                                         "25-54_rest of vic._unemployed",
#                                         "55+_rest of vic._unemployed"
#                                       ),
#                                       df = dash_data
#                                       ) %>%
#                                         dplyr::group_by(series_id) %>%
#                                         dplyr::mutate(value = zoo::rollmeanr(value, 12, fill = NA))) {
#   data <- data %>%
#     dplyr::group_by(.data$date) %>%
#     dplyr::summarise(value = (value[series_id == "15-24_greater melbourne_unemployed"] +
#       value[series_id == "15-24_rest of vic._unemployed"])) %>%
#     dplyr::mutate(
#       series = "Unemployed; 15-24; Victoria",
#       series_id = "unemp_15-24_vic",
#       indicator = "Unemployed_vic",
#       age = "15-24"
#     ) %>%
#     dplyr::bind_rows(data)
#
#   data <- data %>%
#     dplyr::group_by(.data$date) %>%
#     dplyr::summarise(value = (value[series_id == "25-54_greater melbourne_unemployed"] +
#       value[series_id == "25-54_rest of vic._unemployed"])) %>%
#     dplyr::mutate(
#       series = "Unemployed; 25-54; Victoria",
#       series_id = "unemp_25-54_vic",
#       indicator = "Unemployed_vic",
#       age = "25-54"
#     ) %>%
#     dplyr::bind_rows(data)
#
#   data <- data %>%
#     dplyr::group_by(.data$date) %>%
#     dplyr::summarise(value = (value[series_id == "55+_greater melbourne_unemployed"] +
#       value[series_id == "55+_rest of vic._unemployed"])) %>%
#     dplyr::mutate(
#       series = "Unemployed; 55+; Victoria",
#       series_id = "unemp_55+_vic",
#       indicator = "Unemployed_vic",
#       age = "55+"
#     ) %>%
#     dplyr::bind_rows(data)
#
#   # drop rows we don't need
#   data <- dplyr::filter(data, .data$indicator == "Unemployed_vic")
#
#   # draw line chart
#   data %>%
#     dplyr::filter(!is.na(.data$value)) %>%
#     dplyr::ungroup() %>%
#     djpr_ts_linechart() +
#     scale_y_continuous(
#       limits = function(x) c(0, x[2]),
#       expand = expansion(mult = c(0, 0.05))
#     ) +
#     labs(
#       title = "",
#       subtitle = "Unemployment in Victoria by age",
#       caption = "Source: ABS Labour Force. Note: 12 month average."
#     )
# }

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
  df <- data %>%
    dplyr::select(.data$series, .data$value, .data$date)

  df <- df %>%
    tidyr::pivot_wider(
      names_from = .data$series,
      values_from = .data$value
    ) %>%
    dplyr::mutate(
      Australia = 100 * (.data$`52 weeks and over (Long-term unemployed) ;  Unemployed total ;  Persons ;` /
                           .data$`Labour force total ;  Persons ;  Australia ;`),
      Victoria = 100 * ((.data$`Unemployed total ('000) ; Victoria ; 104 weeks and over (2 years and over)` +
                           .data$`Unemployed total ('000) ; Victoria ; 52 weeks and under 104 weeks (1-2 years)`) /
                          .data$`Labour force total ;  Persons ;  > Victoria ;`)
    ) %>%
    dplyr::select(.data$date, .data$Australia, .data$Victoria) %>%
    tidyr::pivot_longer(
      names_to = "state",
      cols = !.data$date
    )

  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2,
      complete = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$value))


  latest_values <- df %>%
    dplyr::filter(date == max(.data$date)) %>%
    dplyr::mutate(
      value = round2(.data$value, 1),
      date = format(.data$date, "%B %Y")
    ) %>%
    tidyr::pivot_wider(names_from = .data$state, values_from = .data$value)

  title <- dplyr::case_when(
    latest_values$Victoria > latest_values$Australia ~
    paste0("Victoria's long-term unemployment rate in ", latest_values$date, " was higher than Australia's"),
    latest_values$Victoria < latest_values$Australia ~
    paste0("Victoria's long-term unemployment rate in ", latest_values$date, " was lower than Australia's"),
    latest_values$Victoria == latest_values$Australia ~
    paste0("Victoria's long-term unemployment rate in ", latest_values$date, " was the same as Australia's"),
    TRUE ~ "Long-term unemployment rate in Victoria and Australia"
  )

  df %>%
    djpr_ts_linechart(
      col_var = .data$state,
      label_num = paste0(round2(.data$value, 1), "%")
    ) +
    labs(
      subtitle = "Long-term unemployment rate in Victoria and Australia, per cent of labour force",
      caption = paste0(caption_lfs_det_m(), " Data not seasonally adjusted. Smoothed using a 3 month rolling average."),
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
    dplyr::select(.data$duration, .data$value, .data$date)

  # take 3 month moving average
  data <- data %>%
    dplyr::group_by(.data$duration) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2,
      complete = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$value))

  data <- data %>%
    tidyr::pivot_wider(
      names_from = .data$duration,
      values_from = .data$value
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
    tidyr::pivot_longer(
      cols = !.data$date,
      names_to = "duration",
      values_to = "value"
    )

  df_data <- df_data %>%
    dplyr::filter(.data$date %in% c(
      max(.data$date),
      subtract_years(max(.data$date), 1)
    ))

  df_data <- df_data %>%
    dplyr::mutate(duration = factor(.data$duration,
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

  latest_month <- format(max(data$date), "%B %Y")

  title_df <- df_data %>%
    dplyr::mutate(
      duration_type =
        case_when(
          duration %in% c(
            "2+ years",
            "1-2 years"
          ) ~ "LT",
          duration %in% c("6-12 months") ~ "mid",
          duration %in% c(
            "<1 month",
            "1-3 months",
            "3-6 months"
          ) ~ "ST",
          TRUE ~ NA_character_
        )
    ) %>%
    dplyr::filter(.data$duration_type %in% c("ST", "LT")) %>%
    dplyr::group_by(.data$duration_type, .data$date) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(change = .data$value - lag(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$change)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$duration_type, .data$change) %>%
    tidyr::pivot_wider(
      names_from = .data$duration_type,
      values_from = .data$change
    )


  title <- dplyr::case_when(
    title_df$LT > 0 &
      title_df$ST > 0 ~
    "The number of long-term and short-term unemployed Victorians both rose over the year to ",
    title_df$LT < 0 &
      title_df$ST > 0 ~
    "The number of long-term unemployed Victorians fell, but the number of people unemployed short-term rose over the year to ",
    title_df$LT > 0 &
      title_df$ST < 0 ~
    "The number of short-term unemployed Victorians fell, but the number of people long-term unemployed rose over the year to ",
    title_df$LT < 0 &
      title_df$ST ~
    "The number of long-term and short-term unemployed Victorians both fell over the year to ",
    TRUE ~ "Long-term and short-term unemployment in Victoria over the year to "
  )

  title <- paste0(title, latest_month)

  # create chart
  df_data <- df_data %>%
    dplyr::mutate(
      date = format(.data$date, "%B %Y"),
      date = factor(.data$date,
        levels = rev(sort(unique(.data$date))),
        ordered = TRUE
      )
    )

  df_data %>%
    ggplot(aes(
      x = .data$duration,
      y = .data$value,
      fill = .data$date
    )) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_djpr(flipped = TRUE) +
    scale_fill_manual(
      values = djpr_pal(2),
      breaks = c(
        max(df_data$date),
        min(df_data$date)
      ),
      aesthetics = c("fill", "colour")
    ) +
    geom_text(
      position = position_dodge(width = 0.9),
      aes(
        label = paste0(round2(.data$value, 1)),
        y = .data$value - 1
      ),
      vjust = 0.5,
      colour = "white",
      hjust = 1,
      size = 12 / .pt
    ) +
    scale_x_discrete(expand = expansion(add = c(0.25, 0.85))) +
    djpr_y_continuous() +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      legend.position = c(0.85, 0.89),
      legend.key.height = unit(1.5, "lines"),
      legend.key.width = unit(1.5, "lines"),
      legend.direction = "vertical",
      axis.ticks = element_blank()
    ) +
    labs(
      subtitle = paste0(
        " Unemployed Victorians ('000s) by duration of unemployment in ",
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
    dplyr::select(.data$duration, .data$value, .data$date)

  # 3 month moving average
  data <- data %>%
    dplyr::group_by(.data$duration) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2,
      complete = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$value))


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
    dplyr::arrange(.data$duration) %>%
    dplyr::mutate(
      perc = .data$value / sum(.data$value),
      cum_perc = cumsum(.data$perc) - (.data$perc / 2),
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
      label = paste0(
        .data$label_no_num, " ",
        round2(100 * .data$perc, 1), "%"
      )
    )

  title_df <- label_df %>%
    dplyr::select(.data$perc, .data$date, .data$label_no_num) %>%
    tidyr::pivot_wider(names_from = .data$label_no_num, values_from = .data$perc)

  # latest_month <- format(max(label_df$date), "%B %Y")

  title <- paste0(
    round2(title_df$`2+ years` * 100, 1),
    " per cent of unemployed Victorians had been unemployed for 2 years or more as at ",
    format(title_df$date, "%B %Y")
  )

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
        x = .data$date,
        y = .data$cum_perc,
        colour = rev(.data$duration),
        label = stringr::str_wrap(.data$label, 11)
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
                                    "A84423237A",
                                    "A84423461V",
                                    "A84423245A",
                                    "A84423469L"
                                  ),
                                  df = dash_data
                                  )) {

  # We calculate part time employment using total + FT employment
  df <- data %>%
    dplyr::select(.data$date, .data$sex, .data$indicator, .data$value) %>%
    tidyr::pivot_wider(names_from = .data$indicator, values_from = .data$value) %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::mutate(
      `Employed part-time` = .data$`Employed total` - .data$`Employed full-time`
    ) %>%
    dplyr::select(!.data$`Employed total`) %>%
    dplyr::ungroup()

  # Calculate annual employment growth within each emp type - sex combination
  df <- df %>%
    tidyr::pivot_longer(
      cols = !c(.data$date, .data$sex),
      names_to = "indicator",
      values_to = "value"
    ) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::group_by(.data$indicator, .data$sex) %>%
    dplyr::mutate(value = 100 * ((.data$value / lag(.data$value, 12) - 1))) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  latest_month <- format(max(df$date), "%B %Y")

  # create latest data by gender
  female_latest_f <- df %>%
    dplyr::filter(.data$sex == "Females" &
      .data$indicator == "Employed full-time" &
      .data$date == max(.data$date)) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  male_latest_f <- df %>%
    dplyr::filter(.data$sex == "Males" &
      .data$indicator == "Employed full-time" &
      .data$date == max(.data$date)) %>%
    dplyr::pull(.data$value) %>%
    round2(1)


  # create title

  title <- dplyr::case_when(
    female_latest_f > male_latest_f ~
    paste0("Full-time employment grew faster for women than men in the year to ", latest_month),
    female_latest_f < male_latest_f ~
    paste0("Full-time employment grew faster for men than women in the year to ", latest_month),
    female_latest_f == male_latest_f ~
    paste0("Full-time employment grew at around the same pace for women and men in the year to ", latest_month),
    TRUE ~ paste0("Full-time and part-time annual employment growth for men and women")
  )

  # create chart
  df %>%
    djpr_ts_linechart(
      col_var = .data$sex,
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

viz_gr_youth_eduemp_waterfall <- function(data = filter_dash_data(c(
                                            "A84424598A",
                                            "A84424778K",
                                            "A84424597X",
                                            "A84424777J",
                                            "A84424600A",
                                            "A84424780W",
                                            "A84424694A"
                                          ),
                                          df = dash_data
                                          )) {
  # select the necessary column
  df <- data %>%
    dplyr::select(.data$date, .data$series, .data$value)

  # 12 month moving average
  df <- df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$value))

  # create short name and pull the latest data
  df <- df %>%
    dplyr::mutate(indicator = dplyr::case_when(
      .data$series == "> Victoria ;  Not attending full-time education ;  Unemployed total ;" ~ "nafte_un",
      .data$series == "> Victoria ;  Attending full-time education ;  Unemployed total ;" ~ "afe_un_total",
      .data$series == "> Victoria ;  Not attending full-time education ;  Not in the labour force (NILF) ;" ~ "nafte_nilf",
      .data$series == "> Victoria ;  Attending full-time education ;  Not in the labour force (NILF) ;" ~ "afe_nilf",
      .data$series == "> Victoria ;  Attending full-time education ;  Employed total ;" ~ "afe_emplo_total",
      .data$series == "> Victoria ;  Not attending full-time education ;  Employed total ;" ~ "nafl_emplo_total",
      .data$series == "> Victoria ;  Civilian population aged 15-24 years ;" ~ "civ_pop",
    )) %>%
    dplyr::filter(.data$date == max(.data$date))

  df <- df %>%
    dplyr::mutate(perc = 100 * (.data$value /
      .data$value[.data$indicator == "civ_pop"]))


  # data for title &label
  df_title <- df %>%
    dplyr::select(.data$indicator, .data$perc, .data$date) %>%
    tidyr::pivot_wider(names_from = .data$indicator, values_from = .data$perc) %>%
    dplyr::mutate(
      vulnerable = (.data$nafte_un + .data$nafte_nilf)
    ) %>%
    dplyr::select(.data$date, .data$vulnerable)


  title <- paste0(
    "In ",
    format(df_title$date, "%B %Y"),
    ", ",
    round2(df_title$vulnerable, 1),
    " per cent of Victorians aged 15-24 years were not in education and not in work, a group most at risk of becoming long term unemployed"
  )

  df <- df %>%
    dplyr::mutate(
      indicator =
        factor(.data$indicator,
          levels = c(
            "nafte_un",
            "nafte_nilf",
            "afe_un_total",
            "afe_nilf",
            "afe_emplo_total",
            "nafl_emplo_total",
            "civ_pop"
          ),
          ordered = TRUE
        )
    ) %>%
    dplyr::arrange(.data$indicator)

  # label name
  df <- df %>%
    dplyr::mutate(label = case_when(
      .data$indicator == "nafte_un" ~ "Unemployed & not in education",
      .data$indicator == "afe_un_total" ~ "Unemployed & in education",
      .data$indicator == "nafte_nilf" ~ "Not in labour force or education",
      .data$indicator == "afe_nilf" ~ "Studying full time & not in labour force",
      .data$indicator == "afe_emplo_total" ~ "Full time education & employed",
      .data$indicator == "nafl_emplo_total" ~ "Not in education & employed",
      .data$indicator == "civ_pop" ~ "Civilian population",
    ))

  # use the same colour for the vulnerable group and the rest grey
  df <- df %>%
    dplyr::mutate(indicator_group = dplyr::if_else(
      .data$indicator %in% c("nafte_un", "nafte_nilf"),
      "Vulnerable",
      "Other"
    ))

  df <- df %>%
    dplyr::mutate(
      y_start = cumsum(.data$value) - .data$value,
      y_end = cumsum(.data$value),
      id = row_number(),
      label = stringr::str_wrap(.data$label, 10)
    )

  df <- df %>%
    dplyr::mutate(
      y_start = dplyr::if_else(.data$indicator == "civ_pop", 0, .data$y_start),
      y_end = dplyr::if_else(.data$indicator == "civ_pop", .data$value, .data$y_end)
    )

  df <- df %>%
    mutate(bar_label = dplyr::if_else(
      .data$indicator == "civ_pop",
      as.character(round2(.data$value, 1)),
      paste0(round2(.data$value, 1), "\n(", round2(.data$perc, 1), "%)")
    ))

  df %>%
    ggplot(aes(
      x = stats::reorder(.data$label, .data$id),
      xend = stats::reorder(.data$label, .data$id),
      y = .data$y_start,
      yend = .data$y_end,
      colour = .data$indicator_group
    )) +
    geom_segment(size = 25) +
    geom_text(aes(
      y = .data$y_end,
      label = .data$bar_label
    ),
    nudge_y = 35,
    lineheight = 0.9,
    size = 12 / .pt
    ) +
    geom_label(
      data = data.frame(x = 1.55, y = 230, label = "Victorian youths most at risk of \n becoming long-term unemployed"),
      mapping = aes(x = .data$x, y = .data$y, label = .data$label),
      size = 4.41, colour = djprtheme::djpr_royal_blue, inherit.aes = FALSE,
      label.size = 0
    ) +
    theme_djpr() +
    scale_colour_manual(values = c(
      "Vulnerable" = djprtheme::djpr_royal_blue,
      "Other" = "grey65"
    )) +
    djpr_y_continuous(expand_top = 0.075) +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_text(size = 12)
    ) +
    labs(
      title = title,
      subtitle = "Education and labour force status of Victorian youths ('000s)",
      caption = paste0(caption_lfs(), " Data not seasonally adjusted. Smoothed using a 12 month rolling average.")
    )
}

# engagement in education and employment

viz_gr_yth_mostvuln_line <- function(data = filter_dash_data(c(
                                       "A84433475V",
                                       "A84424781X"
                                     ),
                                     df = dash_data
                                     )) {
  # select the necessary column
  df <- data %>%
    dplyr::select(.data$date, .data$series, .data$value)

  # 12 month moving average
  df <- df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$value))

  df <- df %>%
    dplyr::mutate(indicator = dplyr::case_when(
      .data$series == "15-24 years ;  > Victoria ;  Not attending full-time education ;  Unemployment rate ;" ~ "Youth not in education",
      .data$series == "> Victoria ;  Attending full-time education ;  Unemployment rate ;" ~ "Youth in education"
    )) %>%
    dplyr::select(-.data$series)

  # calculate annual growth
  change_df <- df %>%
    dplyr::arrange(.data$date) %>%
    dplyr::group_by(.data$indicator) %>%
    dplyr::mutate(value = (.data$value - lag(.data$value, 12) - 1)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  # create title
  title_df <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value
    )

  title_change <- dplyr::case_when(
    title_df$`Youth not in education` < title_df$`Youth in education` ~
    "lower than",
    title_df$`Youth not in education` > title_df$`Youth in education` ~
    "higher than",
    title_df$`Youth not in education` == title_df$`Youth in education` ~
    "the same as"
  )

  title <- paste0(
    "The unemployment rate for Victorian youth not in education was ",
    title_change,
    " the rate for youth in education in ",
    format(title_df$date, "%B %Y")
  )

  level_plot <- df %>%
    djpr_ts_linechart(
      col_var = .data$indicator,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(subtitle = "Unemployment rate for Victorian youth (aged 15-24)")

  change_plot <- change_df %>%
    djpr_ts_linechart(
      hline = 0,
      col_var = .data$indicator,
      label_num = paste0(round2(.data$value, 1), " ppts"),
      y_labels = function(x) paste0(x, " ppts")
    ) +
    labs(
      subtitle = "Annual change in unemployment rate for Victorian youth (aged 15-24)"
    )

  patchwork::wrap_plots(level_plot, change_plot,
    ncol = 1
  ) +
    patchwork::plot_annotation(
      title = title,
      theme = djprtheme::theme_djpr(),
      caption = paste0(caption_lfs(), " Data not seasonally adjusted. Smoothed using a 12 month rolling average.")
    )
}

viz_gr_gen_emppopratio_line <- function(data = filter_dash_data(c(
                                          "A84423244X",
                                          "A84423468K"
                                        ),
                                        df = dash_data
                                        )) {
  df <- data %>%
    dplyr::select(.data$date, .data$value, .data$sex, .data$indicator) %>%
    dplyr::mutate(series = .data$indicator)

  min_year <- format(min(df$date), "%Y")
  max_year <- format(max(df$date), "%Y")

  ave_df <- df %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::summarise(
      ave = mean(.data$value),
      min_date = min(.data$date),
      max_date = max(.data$date)
    )

  # Create title
  title_df <- df %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::summarise(latest_value = .data$value[.data$date == max(.data$date)]) %>%
    dplyr::left_join(ave_df, by = "sex") %>%
    dplyr::mutate(
      diff = .data$latest_value - .data$ave,
      diff_desc = dplyr::case_when(
        .data$diff > 1 ~ "well above",
        .data$diff < -1 ~ "well below",
        .data$diff > 0 ~ "slightly above",
        .data$diff < 0 ~ "slightly below",
        .data$diff == 0 ~ "equal to"
      )
    )

  title <- paste0(
    "The proportion of Victorian women in work was ",
    title_df$diff_desc[title_df$sex == "Females"],
    " its long-run average in ",
    format(unique(title_df$max_date), "%B %Y"),
    ", while the rate for men was ",
    title_df$diff_desc[title_df$sex == "Males"],
    " its long-run average"
  )


  df <- df %>%
    group_by(.data$sex) %>%
    mutate(value = mean(.data$value)) %>%
    mutate(indicator = "Average") %>%
    bind_rows(df) %>%
    dplyr::mutate(
      tooltip =
        dplyr::if_else(
          .data$indicator == "Average",
          paste0("Average\n", round2(.data$value, 1), "%"),
          paste0(
            .data$sex, "\n",
            format(.data$date, "%b %Y"), "\n",
            round2(.data$value, 1), "%"
          )
        ),
      series = paste0(.data$indicator, " ", .data$sex)
    )

  max_date <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::mutate(
      label =
        dplyr::if_else(.data$indicator == "Average",
          paste0("Average\n", round2(.data$value, 1), "%"),
          paste0(
            format(.data$date, "%b %Y"), "\n",
            round2(.data$value, 1), "%"
          )
        )
    )

  days_in_data <- as.numeric(max(df$date) - min(df$date))

  df %>%
    ggplot(aes(
      x = .data$date,
      y = .data$value,
      col = .data$sex,
      group = .data$series
    )) +
    geom_line(aes(linetype = .data$indicator)) +
    geom_point(
      data = max_date %>%
        dplyr::filter(.data$indicator != "Average"),
      fill = "white",
      stroke = 1.5,
      size = 2.5,
      shape = 21
    ) +
    geom_text(
      data = df %>%
        dplyr::group_by(.data$sex) %>%
        dplyr::summarise(
          date = stats::median(.data$date),
          value = max(.data$value)
        ),
      aes(
        label = .data$sex,
        x = .data$date,
        y = .data$value,
        col = .data$sex
      ),
      vjust = 0,
      size = 14 / .pt,
      inherit.aes = F
    ) +
    ggrepel::geom_label_repel(
      data = max_date,
      aes(label = .data$label),
      hjust = 0,
      nudge_x = days_in_data * 0.033,
      label.padding = 0.01,
      label.size = NA,
      lineheight = 0.9,
      point.padding = unit(0, "lines"),
      direction = "y",
      seed = 123,
      show.legend = FALSE,
      min.segment.length = unit(5, "lines"),
      size = 14 / .pt
    ) +
    ggiraph::geom_point_interactive(aes(tooltip = .data$tooltip),
      size = 3,
      colour = "white",
      alpha = 0.01
    ) +
    theme_djpr() +
    scale_linetype_manual(values = c(
      "Average" = 2,
      "Employment to population ratio" = 1
    )) +
    djpr_colour_manual(2) +
    scale_x_date(
      expand = expansion(mult = c(0, 0.2)),
      breaks = djprtheme::breaks_right(
        limits = c(
          min(df$date),
          max(df$date)
        ),
        n_breaks = 5
      ),
      date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      breaks = scales::breaks_pretty(5)
    ) +
    theme(axis.title.x = element_blank()) +
    coord_cartesian(clip = "off") +
    labs(
      subtitle = "Employment to population ratio by sex for Victoria ",
      caption = caption_lfs(),
      title = title
    )
}

viz_gr_youth_full_part_line <- function(data = filter_dash_data(c(
                                          "A84424687C",
                                          "A84424695C",
                                          "A84424696F"
                                        ),
                                        df = dash_data
                                        )) {
  df <- data %>%
    dplyr::select(.data$date, .data$value, .data$indicator)

  # 12 month moving average
  df <- df %>%
    dplyr::group_by(.data$indicator) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$value))

  # calculate proportion of youth employment by type
  df <- df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(perc = 100 * (.data$value / .data$value[.data$indicator == "Employed total"])) %>%
    dplyr::select(.data$date, .data$perc, .data$indicator) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::filter(!grepl("Employed total", .data$indicator, fixed = TRUE)) %>%
    dplyr::mutate(value = .data$perc)



  title_df <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(-.data$perc) %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value
    )



  title <- dplyr::case_when(
    title_df$`Employed part-time` > title_df$`Employed full-time` ~
    "More Victorian youth were employed part-time than full-time in ",
    title_df$`Employed part-time` < title_df$`Employed full-time` ~
    "More Victorian youth were employed full-time than part-time in ",
    title_df$`Employed part-time` == title_df$`Employed full-time` ~
    "The same proportion of young Victorian workers were employed full-time and part-time"
  )

  title <- paste0(title, format(title_df$date, "%B %Y"))

  df %>%
    djpr_ts_linechart(
      col_var = .data$indicator,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = title,
      subtitle = "The proportion of Victorian young (aged 15-24) workers who are employed full-time and part-time",
      caption = paste0(caption_lfs(), " Data not seasonally adjusted. Smoothed using a 12 month rolling average.")
    )
}

title_youth_unemp_emppop_partrate_vic <- function(data = filter_dash_data(c(
                                                    "15-24_unemployed_melbourne - inner",
                                                    "15-24_unemployed_melbourne - inner east",
                                                    "15-24_unemployed_melbourne - inner south",
                                                    "15-24_unemployed_melbourne - north east",
                                                    "15-24_unemployed_melbourne - north west",
                                                    "15-24_unemployed_melbourne - outer east",
                                                    "15-24_unemployed_melbourne - south east",
                                                    "15-24_unemployed_melbourne - west",
                                                    "15-24_unemployed_mornington peninsula",
                                                    "15-24_unemployed_ballarat",
                                                    "15-24_unemployed_bendigo",
                                                    "15-24_unemployed_geelong",
                                                    "15-24_unemployed_hume",
                                                    "15-24_unemployed_latrobe - gippsland",
                                                    "15-24_unemployed_victoria - north west",
                                                    "15-24_unemployed_shepparton",
                                                    "15-24_unemployed_warrnambool and south west",
                                                    "15-24_employed_melbourne - inner",
                                                    "15-24_employed_melbourne - inner east",
                                                    "15-24_employed_melbourne - inner south",
                                                    "15-24_employed_melbourne - north east",
                                                    "15-24_employed_melbourne - north west",
                                                    "15-24_employed_melbourne - outer east",
                                                    "15-24_employed_melbourne - south east",
                                                    "15-24_employed_melbourne - west",
                                                    "15-24_employed_mornington peninsula",
                                                    "15-24_employed_ballarat",
                                                    "15-24_employed_bendigo",
                                                    "15-24_employed_geelong",
                                                    "15-24_employed_hume",
                                                    "15-24_employed_latrobe - gippsland",
                                                    "15-24_employed_victoria - north west",
                                                    "15-24_employed_shepparton",
                                                    "15-24_employed_warrnambool and south west",
                                                    "15-24_nilf_melbourne - inner",
                                                    "15-24_nilf_melbourne - inner east",
                                                    "15-24_nilf_melbourne - inner south",
                                                    "15-24_nilf_melbourne - north east",
                                                    "15-24_nilf_melbourne - north west",
                                                    "15-24_nilf_melbourne - outer east",
                                                    "15-24_nilf_melbourne - south east",
                                                    "15-24_nilf_melbourne - west",
                                                    "15-24_nilf_mornington peninsula",
                                                    "15-24_nilf_ballarat",
                                                    "15-24_nilf_bendigo",
                                                    "15-24_nilf_geelong",
                                                    "15-24_nilf_hume",
                                                    "15-24_nilf_latrobe - gippsland",
                                                    "15-24_nilf_victoria - north west",
                                                    "15-24_nilf_shepparton",
                                                    "15-24_nilf_warrnambool and south west"
                                                  ),
                                                  df = dash_data
                                                  ),
                                                  selected_indicator = "unemp_rate") {
  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "The youth unemployment rate",
    selected_indicator == "part_rate" ~ "The youth participation rate",
    selected_indicator == "emp_pop" ~ "The youth employment to population ratio",
    TRUE ~ NA_character_
  )

  # 12 month smoothing and only latest date
  df <- data %>%
    group_by(.data$series_id) %>%
    mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::filter(.data$date == max(.data$date))

  # combining value for each date, sa4 and indicator
  df <- df %>%
    dplyr::group_by(.data$date, .data$sa4, .data$indicator) %>%
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
      emp_pop = 100 * .data$Employed /
        (.data$Employed + .data$NILF + .data$Unemployed),
      unemp_rate = 100 * .data$Unemployed /
        (.data$Employed + .data$Unemployed),
      part_rate = 100 * (.data$Employed + .data$Unemployed) /
        (.data$Employed + .data$Unemployed + .data$NILF)
    )

  # Go from wide to long again
  df <- df %>%
    dplyr::select(.data$date, .data$sa4, .data$emp_pop, .data$unemp_rate, .data$part_rate) %>%
    tidyr::pivot_longer(
      cols = !c(.data$date, .data$sa4),
      names_to = "indicator",
      values_to = "value"
    )

  # Reduce to selected_indicator
  df <- df %>%
    dplyr::filter(.data$indicator == selected_indicator)

  high_low <- df %>%
    dplyr::ungroup() %>%
    summarise(
      min_sa4 = .data$sa4[.data$value == min(.data$value)],
      min_val = .data$value[.data$value == min(.data$value)],
      max_sa4 = .data$sa4[.data$value == max(.data$value)],
      max_val = .data$value[.data$value == max(.data$value)],
      date = unique(.data$date)
    )

  paste0(
    indic_long,
    " across Victoria ranged from ",
    round2(high_low$min_val, 1),
    " per cent in ",
    high_low$min_sa4,
    " to ",
    round2(high_low$max_val, 1),
    " per cent in ",
    high_low$max_sa4,
    " as at ",
    format(high_low$date, "%B %Y")
  )
}

map_youth_unemp_emppop_partrate_vic <- function(data = filter_dash_data(c(
                                                  "15-24_unemployed_melbourne - inner",
                                                  "15-24_unemployed_melbourne - inner east",
                                                  "15-24_unemployed_melbourne - inner south",
                                                  "15-24_unemployed_melbourne - north east",
                                                  "15-24_unemployed_melbourne - north west",
                                                  "15-24_unemployed_melbourne - outer east",
                                                  "15-24_unemployed_melbourne - south east",
                                                  "15-24_unemployed_melbourne - west",
                                                  "15-24_unemployed_mornington peninsula",
                                                  "15-24_unemployed_ballarat",
                                                  "15-24_unemployed_bendigo",
                                                  "15-24_unemployed_geelong",
                                                  "15-24_unemployed_hume",
                                                  "15-24_unemployed_latrobe - gippsland",
                                                  "15-24_unemployed_victoria - north west",
                                                  "15-24_unemployed_shepparton",
                                                  "15-24_unemployed_warrnambool and south west",
                                                  "15-24_employed_melbourne - inner",
                                                  "15-24_employed_melbourne - inner east",
                                                  "15-24_employed_melbourne - inner south",
                                                  "15-24_employed_melbourne - north east",
                                                  "15-24_employed_melbourne - north west",
                                                  "15-24_employed_melbourne - outer east",
                                                  "15-24_employed_melbourne - south east",
                                                  "15-24_employed_melbourne - west",
                                                  "15-24_employed_mornington peninsula",
                                                  "15-24_employed_ballarat",
                                                  "15-24_employed_bendigo",
                                                  "15-24_employed_geelong",
                                                  "15-24_employed_hume",
                                                  "15-24_employed_latrobe - gippsland",
                                                  "15-24_employed_victoria - north west",
                                                  "15-24_employed_shepparton",
                                                  "15-24_employed_warrnambool and south west",
                                                  "15-24_nilf_melbourne - inner",
                                                  "15-24_nilf_melbourne - inner east",
                                                  "15-24_nilf_melbourne - inner south",
                                                  "15-24_nilf_melbourne - north east",
                                                  "15-24_nilf_melbourne - north west",
                                                  "15-24_nilf_melbourne - outer east",
                                                  "15-24_nilf_melbourne - south east",
                                                  "15-24_nilf_melbourne - west",
                                                  "15-24_nilf_mornington peninsula",
                                                  "15-24_nilf_ballarat",
                                                  "15-24_nilf_bendigo",
                                                  "15-24_nilf_geelong",
                                                  "15-24_nilf_hume",
                                                  "15-24_nilf_latrobe - gippsland",
                                                  "15-24_nilf_victoria - north west",
                                                  "15-24_nilf_shepparton",
                                                  "15-24_nilf_warrnambool and south west"
                                                ),
                                                df = dash_data
                                                ),
                                                selected_indicator = "unemp_rate",
                                                zoom = 6) {
  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "Unemployment rate",
    selected_indicator == "part_rate" ~ "Participation rate",
    selected_indicator == "emp_pop" ~ "Employment to population ratio",
    TRUE ~ NA_character_
  )

  # 12 month smoothing and only latest date
  df <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::filter(.data$date == max(.data$date))

  # combining value for each date, sa4 and indicator
  df <- df %>%
    dplyr::group_by(.data$date, .data$sa4, .data$indicator) %>%
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
      emp_pop = 100 * .data$Employed /
        (.data$Employed + .data$NILF + .data$Unemployed),
      unemp_rate = 100 * .data$Unemployed /
        (.data$Employed + .data$Unemployed),
      part_rate = 100 * (.data$Employed + .data$Unemployed) /
        (.data$Employed + .data$Unemployed + .data$NILF)
    )

  # Go from wide to long again
  df <- df %>%
    dplyr::select(.data$date, .data$sa4, .data$emp_pop, .data$unemp_rate, .data$part_rate) %>%
    tidyr::pivot_longer(
      cols = !c(.data$date, .data$sa4),
      names_to = "indicator",
      values_to = "value"
    )

  # Reduce to selected_indicator
  df <- df %>%
    dplyr::filter(.data$indicator == selected_indicator)

  # Call SA4 shape file, but only load Victoria and exclude 'weird' areas (migratory and other one)
  sa4_shp <- sa42016 %>%
    dplyr::filter(.data$state_name_2016 == "Victoria") %>%
    dplyr::filter(.data$sa4_code_2016 < 297)

  # Fix issue with different naming for North West region in Victoria
  df <- df %>%
    dplyr::mutate(
      sa4 = dplyr::if_else(.data$sa4 == "Victoria - North West",
        "North West",
        .data$sa4
      )
    )

  # Join shape file with data to create mapdata ----
  mapdata <- sa4_shp %>%
    dplyr::left_join(df, by = c("sa4_name_2016" = "sa4"))

  # Create colour palette
  # Switched here from binned to continuous colours
  # pal <- leaflet::colorBin("Blues", mapdata$value, 3) # last object is number of bins
  pal <- leaflet::colorNumeric("Blues", c(min(mapdata$value), max(mapdata$value)), alpha = T)

  # Create metro boundary (Greater Melbourne) ----
  metro_boundary_sa4 <- c(
    "Melbourne - Inner", "Melbourne - Inner East", "Melbourne - Inner South", "Melbourne - North East",
    "Melbourne - North West", "Melbourne - Outer East", "Melbourne - South East", "Melbourne - West",
    "Mornington Peninsula"
  )

  mapdata <- mapdata %>%
    sf::st_transform("+proj=longlat +datum=WGS84")

  metro_outline <- mapdata %>%
    dplyr::filter(.data$sa4_name_2016 %in% metro_boundary_sa4) %>%
    dplyr::summarise(areasqkm_2016 = sum(.data$areasqkm_2016))

  label_title <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ paste0("Youth unemployment<br/> rate (per cent)"),
    selected_indicator == "part_rate" ~ paste0("Youth participation<br/> rate (per cent)"),
    selected_indicator == "emp_pop" ~ paste0("Youth employment to<br/> population ratio<br/> (per cent)"),
    TRUE ~ NA_character_
  )

  # Produce dynamic map, all of Victoria ----
  map <- mapdata %>%
    leaflet::leaflet(options = leaflet::leafletOptions(background = "white")) %>%
    leaflet::setView(
      lng = 145.4657, lat = -36.41472, # coordinates of map at first view
      zoom = zoom
    ) %>%
    # size of map at first view
    leaflet::addPolygons(
      color = "grey", # colour of boundary lines, 'transparent' for no lines
      weight = 1, # thickness of boundary lines
      fillColor = ~ pal(mapdata$value), # pre-defined above
      fillOpacity = 1.0, # strength of fill colour
      smoothFactor = 0.5, # smoothing between region
      stroke = T,
      highlightOptions = leaflet::highlightOptions( # to highlight regions as you hover over them
        color = "black", # boundary colour of region you hover over
        weight = 2, # thickness of region boundary
        bringToFront = FALSE
      ), # FALSE = metro outline remains
      label = sprintf(
        "<strong>%s</strong><br/>%s: %.1f",
        mapdata$sa4_name_2016, # region name displayed in label
        indic_long,
        mapdata$value
      ) %>% # eco data displayed in label
        lapply(shiny::HTML),
      labelOptions = leaflet::labelOptions( # label options
        style = list(
          "font-weight" = "normal", # "bold" makes it so
          padding = "3px 8px"
        ),
        textsize = "12px", # text size of label
        noHide = FALSE, # TRUE makes labels permanently visible (messy)
        direction = "auto"
      ) # text box flips from side to side as needed
    ) %>%
    leaflet::addLegend(
      position = "topright", # options: topright, bottomleft etc.
      pal = pal, # colour palette as defined
      values = mapdata$value, # fill data
      bins = 3,
      labFormat = leaflet::labelFormat(transform = identity),
      title = label_title,
      opacity = 1,
    ) %>%
    # label opacity
    leaflet::addPolygons(
      data = metro_outline, #
      fill = F,
      stroke = T,
      opacity = 1,
      color = "black",
      weight = 1
    )

  map
}

viz_gr_youth_unemp_emppop_partrate_bar <- function(data = filter_dash_data(c(
                                                     "15-24_unemployed_melbourne - inner",
                                                     "15-24_unemployed_melbourne - inner east",
                                                     "15-24_unemployed_melbourne - inner south",
                                                     "15-24_unemployed_melbourne - north east",
                                                     "15-24_unemployed_melbourne - north west",
                                                     "15-24_unemployed_melbourne - outer east",
                                                     "15-24_unemployed_melbourne - south east",
                                                     "15-24_unemployed_melbourne - west",
                                                     "15-24_unemployed_mornington peninsula",
                                                     "15-24_unemployed_ballarat",
                                                     "15-24_unemployed_bendigo",
                                                     "15-24_unemployed_geelong",
                                                     "15-24_unemployed_hume",
                                                     "15-24_unemployed_latrobe - gippsland",
                                                     "15-24_unemployed_victoria - north west",
                                                     "15-24_unemployed_shepparton",
                                                     "15-24_unemployed_warrnambool and south west",
                                                     "15-24_employed_melbourne - inner",
                                                     "15-24_employed_melbourne - inner east",
                                                     "15-24_employed_melbourne - inner south",
                                                     "15-24_employed_melbourne - north east",
                                                     "15-24_employed_melbourne - north west",
                                                     "15-24_employed_melbourne - outer east",
                                                     "15-24_employed_melbourne - south east",
                                                     "15-24_employed_melbourne - west",
                                                     "15-24_employed_mornington peninsula",
                                                     "15-24_employed_ballarat",
                                                     "15-24_employed_bendigo",
                                                     "15-24_employed_geelong",
                                                     "15-24_employed_hume",
                                                     "15-24_employed_latrobe - gippsland",
                                                     "15-24_employed_victoria - north west",
                                                     "15-24_employed_shepparton",
                                                     "15-24_employed_warrnambool and south west",
                                                     "15-24_nilf_melbourne - inner",
                                                     "15-24_nilf_melbourne - inner east",
                                                     "15-24_nilf_melbourne - inner south",
                                                     "15-24_nilf_melbourne - north east",
                                                     "15-24_nilf_melbourne - north west",
                                                     "15-24_nilf_melbourne - outer east",
                                                     "15-24_nilf_melbourne - south east",
                                                     "15-24_nilf_melbourne - west",
                                                     "15-24_nilf_mornington peninsula",
                                                     "15-24_nilf_ballarat",
                                                     "15-24_nilf_bendigo",
                                                     "15-24_nilf_geelong",
                                                     "15-24_nilf_hume",
                                                     "15-24_nilf_latrobe - gippsland",
                                                     "15-24_nilf_victoria - north west",
                                                     "15-24_nilf_shepparton",
                                                     "15-24_nilf_warrnambool and south west"
                                                   ),
                                                   df = dash_data
                                                   ),
                                                   selected_indicator = "unemp_rate") {
  # 12 month smoothing and only latest date
  df <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::select(.data$date, .data$sa4, .data$indicator, .data$value)

  # Go from long to wide
  df <- df %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value
    )

  # Calculate ratios
  df <- df %>%
    dplyr::mutate(
      emp_pop = 100 * .data$Employed /
        (.data$Employed + .data$NILF + .data$Unemployed),
      unemp_rate = 100 * .data$Unemployed /
        (.data$Employed + .data$Unemployed),
      part_rate = 100 * (.data$Employed + .data$Unemployed) /
        (.data$Employed + .data$Unemployed + .data$NILF)
    )

  # Go from wide to long again
  df <- df %>%
    dplyr::select(.data$date, .data$sa4, .data$emp_pop, .data$unemp_rate, .data$part_rate) %>%
    tidyr::pivot_longer(
      cols = !c(.data$date, .data$sa4),
      names_to = "indicator",
      values_to = "value"
    )

  # Reduce to selected_indicator
  df <- df %>%
    dplyr::filter(.data$indicator == selected_indicator)

  df <- df %>%
    dplyr::filter(.data$sa4 != "") %>%
    dplyr::mutate(sa4 = dplyr::if_else(grepl("Warrnambool", .data$sa4, fixed = TRUE),
      "Warrnambool & S. West",
      .data$sa4
    ))

  df %>%
    ggplot(aes(
      x = stats::reorder(.data$sa4, .data$value),
      y = .data$value
    )) +
    geom_col(
      col = "grey85",
      aes(fill = -.data$value)
    ) +
    geom_text(
      nudge_y = 0.1,
      aes(label = paste0(round2(.data$value, 1), "%")),
      colour = "black",
      hjust = 0,
      size = 12 / .pt
    ) +
    coord_flip(clip = "off") +
    scale_fill_distiller(palette = "Blues") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    djprtheme::theme_djpr(flipped = TRUE) +
    theme(
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank()
    ) +
    labs(title = "")
}

viz_gr_youth_vicaus_line <- function(data = filter_dash_data(c(
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
                                     ),
                                     df = dash_data
                                     ) %>%
                                       dplyr::mutate(
                                         state = dplyr::if_else(.data$state == "",
                                           "Aus",
                                           .data$state
                                         ),
                                         state = strayr::clean_state(.data$state)
                                       ),
                                     selected_indicator = "unemp_rate") {

  # 12 month smoothing, remove NAs and drop not needed columns
  df <- data %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::select(.data$date, .data$value, .data$series, .data$indicator, .data$state) %>%
    dplyr::ungroup()

  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "Unemployment rate",
    selected_indicator == "part_rate" ~ "Participation rate",
    selected_indicator == "emp_pop" ~ "Employment to population ratio",
    TRUE ~ NA_character_
  )

  # Reduce to selected_indicator
  df <- df %>%
    dplyr::filter(.data$indicator == .env$indic_long)

  # create state_group
  df <- df %>%
    dplyr::mutate(
      tooltip = paste0(
        .data$state, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

  latest <- df %>%
    dplyr::filter(
      .data$date == max(.data$date)
    ) %>%
    dplyr::select(.data$state, .data$value)

  vic_level_raw <- round2(latest$value[latest$state == "Vic"], 1)
  aus_level_raw <- round2(latest$value[latest$state == "Aus"], 1)
  vic_level <- paste0(vic_level_raw, "%")

  title <- dplyr::case_when(
    vic_level_raw > aus_level_raw ~ ", which was higher than the Australian average",
    vic_level_raw == aus_level_raw ~ ", which was the same as the Australian average",
    vic_level_raw < aus_level_raw ~ ", which was lower than the Australian average",
    TRUE ~ paste0("Victoria's ", indic_long, "compared to other states and territories and the Australian average")
  )

  title <- paste0("Victoria's youth ", tolower(indic_long), " in ", format(max(df$date), "%B %Y"), " was ", vic_level, title)

  subtitle <- paste0("Youth (age 15 to 24) ", tolower(indic_long), " by state")

  other_colour <- "grey70"

  df %>%
    djpr_ts_linechart(
      col_var = .data$state,
      y_labels = function(x) paste0(x, "%"),
      label_num = paste0(round2(.data$value, 1), "%")
    ) +
    scale_colour_manual(values = c(
      "Vic" = djprtheme::djpr_royal_blue,
      "Aus" = djprtheme::djpr_green,
      "NSW" = other_colour,
      "NT" = other_colour,
      "Tas" = other_colour,
      "SA" = other_colour,
      "Qld" = other_colour,
      "ACT" = other_colour,
      "WA" = other_colour
    )) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption_lfs()
    )
}
