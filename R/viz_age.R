# Functions to create the graphs for the 'Age' subpage on the dashboard.


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
    dplyr::mutate(
      value = 100 * ((.data$value /
        .data$value[.data$date == as.Date("2020-03-01")]) - 1),
      tooltip = paste0(
        .data$state, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

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
    dplyr::mutate(value = paste0(round2(.data$value, 1), " per cent"), ) %>%
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
    dplyr::mutate(
      geog = dplyr::if_else(.data$state == "", "Australia", .data$state),
      tooltip = paste0(
        .data$geog, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    ) %>%
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
                                             dplyr::mutate(value = slider::slide_mean(.data$value,
                                               before = 11,
                                               complete = TRUE
                                             )) %>%
                                             dplyr::filter(.data$date >= as.Date("2020-01-01"))) {
  data <- data %>%
    dplyr::group_by(.data$age, .data$date) %>%
    dplyr::summarise(value = sum(.data$value))

  # Indexing to Covid start
  data <- data %>%
    dplyr::group_by(.data$age) %>%
    dplyr::mutate(
      value = 100 * ((.data$value /
        .data$value[.data$date == as.Date("2020-03-01")]) - 1),
      tooltip = paste0(
        .data$age, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

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

viz_gr_yth_mostvuln_line <- function(data = filter_dash_data(
                                       c(
                                         "A84424601C",
                                         "A84424781X"
                                       ),
                                       df = dash_data
                                     )) {
  # select the necessary column
  df <- data %>%
    dplyr::select(.data$date, .data$series_id, .data$value)

  # 12 month moving average
  df <- df %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$value))

  df <- df %>%
    dplyr::mutate(indicator = dplyr::case_when(
      .data$series_id == "A84424601C" ~ "Youth not in education",
      .data$series_id == "A84424781X" ~ "Youth in education"
    )) %>%
    dplyr::select(-.data$series_id)

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
    dplyr::mutate(
      value = .data$perc,
      tooltip = paste0(
        .data$indicator, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

  title_df <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(-.data$perc, -.data$tooltip) %>%
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
  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "Unemployment rate",
    selected_indicator == "part_rate" ~ "Participation rate",
    selected_indicator == "emp_pop" ~ "Employment to population ratio",
    TRUE ~ NA_character_
  )

  # Reduce to selected_indicator
  df <- data %>%
    dplyr::filter(.data$indicator == .env$indic_long)

  # 12 month smoothing, remove NAs and drop not needed columns
  df <- df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::select(.data$date, .data$value, .data$series, .data$indicator, .data$state) %>%
    dplyr::ungroup()

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

  # combining value for each date, sa4 and indicator
  df <- data %>%
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

  df <- df %>%
    dplyr::group_by(.data$sa4, .data$indicator) %>%
    mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::filter(.data$date == max(.data$date))

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


  # combining value for each date, sa4 and indicator
  df <- data %>%
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

  # 12 month smoothing and only latest date
  df <- df %>%
    dplyr::group_by(.data$sa4, .data$indicator) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::filter(.data$date == max(.data$date))

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
  df <- data %>%
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

  # 12 month smoothing and only latest date
  df <- df %>%
    dplyr::group_by(.data$sa4, .data$indicator) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 11,
      complete = TRUE
    )) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()


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

viz_gr_age_jobact_sincecovidindex_line <- function(data = filter_dash_data(c(
                                                     "jobactive_youth (15-24)_ballarat",
                                                     "jobactive_youth (15-24)_bendigo",
                                                     "jobactive_youth (15-24)_barwon",
                                                     "jobactive_youth (15-24)_gippsland",
                                                     "jobactive_youth (15-24)_goulburn/murray",
                                                     "jobactive_youth (15-24)_inner metropolitan melbourne",
                                                     "jobactive_youth (15-24)_north eastern melbourne",
                                                     "jobactive_youth (15-24)_north western melbourne",
                                                     "jobactive_youth (15-24)_south coast of victoria",
                                                     "jobactive_youth (15-24)_south eastern melbourne and peninsula",
                                                     "jobactive_youth (15-24)_north western melbourne",
                                                     "jobactive_youth (15-24)_wimmera mallee",
                                                     "jobactive_mature age (50+)_ballarat",
                                                     "jobactive_mature age (50+)_bendigo",
                                                     "jobactive_mature age (50+)_barwon",
                                                     "jobactive_mature age (50+)_gippsland",
                                                     "jobactive_mature age (50+)_goulburn/murray",
                                                     "jobactive_mature age (50+)_inner metropolitan melbourne",
                                                     "jobactive_mature age (50+)_north eastern melbourne",
                                                     "jobactive_mature age (50+)_north western melbourne",
                                                     "jobactive_mature age (50+)_south coast of victoria",
                                                     "jobactive_mature age (50+)_south eastern melbourne and peninsula",
                                                     "jobactive_mature age (50+)_north western melbourne",
                                                     "jobactive_mature age (50+)_wimmera mallee",
                                                     "jobactive_total_ballarat",
                                                     "jobactive_total_bendigo",
                                                     "jobactive_total_barwon",
                                                     "jobactive_total_gippsland",
                                                     "jobactive_total_goulburn/murray",
                                                     "jobactive_total_inner metropolitan melbourne",
                                                     "jobactive_total_north eastern melbourne",
                                                     "jobactive_total_north western melbourne",
                                                     "jobactive_total_south coast of victoria",
                                                     "jobactive_total_south eastern melbourne and peninsula",
                                                     "jobactive_total_north western melbourne",
                                                     "jobactive_total_wimmera mallee"
                                                   ),
                                                   df = dash_data
                                                   ) %>%
                                                     dplyr::filter(date >= as.Date("2019-03-31"))) {
  df <- data %>%
    dplyr::select(
      .data$date, .data$series,
      .data$frequency, .data$value
    ) %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      indicator = .data$split_series[, 2],
      employment_region = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$series, -.data$jobactive)


  df <- df %>%
    dplyr::group_by(.data$indicator, .data$date) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value
    ) %>%
    dplyr::mutate("25-49 Age" = .data$Total - .data$`Mature Age (50+)` - .data$`Youth (15-24)`) %>%
    dplyr::select(.data$date, .data$`25-49 Age`, .data$`Mature Age (50+)`, .data$`Youth (15-24)`) %>%
    tidyr::pivot_longer(
      cols = !.data$date,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      value = 100 * (.data$value
        / .data$value[.data$date == as.Date("2020-03-31")]),
      tooltip = paste0(
        .data$indicator, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1)
      )
    )

  titl_df <- df %>%
    dplyr::group_by(.data$indicator) %>%
    tidyr::pivot_wider(names_from = .data$indicator, values_from = .data$value) %>%
    dplyr::mutate(value = ((.data$value[date == as.Date(max(.data$date))] - .data$value[date == as.Date("2020-03-01")])))

  latest_date <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::pull(.data$date)
  round2(1)

  latest_values <- df %>%
    dplyr::filter(date == max(.data$date)) %>%
    dplyr::select(-.data$tooltip) %>%
    dplyr::mutate(
      value = round2(.data$value, 1),
      date = format(.data$date, "%B %Y")
    ) %>%
    tidyr::pivot_wider(names_from = .data$indicator, values_from = .data$value)

  df %>%
    djpr_ts_linechart(
      col_var = .data$indicator,
      label_num = paste0(round2(.data$value, 1)),
    ) +
    labs(
      title = "title",
      subtitle = "Victorians Jobactive Caseload by age, Indexed March 2020",
      caption = caption_jobactive()
    )
}
