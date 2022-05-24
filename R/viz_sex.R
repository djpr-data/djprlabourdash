# Function to create the graphs for the 'Sex' subpage on the dashboard.


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
      col = djprtheme::djpr_cool_grey_11
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
    dplyr::mutate(
      sex = dplyr::if_else(.data$sex == "", "Persons", .data$sex),
      tooltip = paste0(
        .data$state, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

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
    dplyr::mutate(
      sex = dplyr::if_else(.data$sex == "", "Persons", .data$sex),
      tooltip = paste0(
        .data$sex, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

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

# Full-time and part-time  employment growth pattern by gender
viz_gr_full_part_line <- function(data, dates) {

  # We calculate part time employment using total + FT employment
  df <- data %>%
    dplyr::filter(date >= dates[1], date <= dates[2]) %>%
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
    # dplyr::arrange(.data$date) %>%
    dplyr::group_by(.data$indicator, .data$sex) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / lag(.data$value, 12) - 1)),
      tooltip = paste0(
        .data$sex, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    ) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  latest_month <- format(max(df$date), "%B %Y")

  # create latest data by gender
  latest_ft <- df %>%
    dplyr::filter(.data$indicator == "Employed full-time" &
      .data$date == max(.data$date)) %>%
    dplyr::mutate(value = round2(.data$value, 1))

  female_latest_f <- latest_ft %>%
    dplyr::filter(.data$sex == "Females") %>%
    dplyr::pull(.data$value)

  male_latest_f <- latest_ft %>%
    dplyr::filter(.data$sex == "Males") %>%
    dplyr::pull(.data$value)


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

viz_gr_female_jobact_sincecovid_line <- function(data = filter_dash_data(c(
                                                   "jobactive_female_ballarat",
                                                   "jobactive_female_bendigo",
                                                   "jobactive_female_barwon",
                                                   "jobactive_female_gippsland",
                                                   "jobactive_female_goulburn/murray",
                                                   "jobactive_female_inner metropolitan melbourne",
                                                   "jobactive_female_north eastern melbourne",
                                                   "jobactive_female_north western melbourne",
                                                   "jobactive_female_south coast of victoria",
                                                   "jobactive_female_south eastern melbourne and peninsula",
                                                   "jobactive_female_western melbourne",
                                                   "jobactive_female_wimmera mallee",
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
                                                   "jobactive_total_western melbourne",
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
    dplyr::mutate("Male" = .data$Total - .data$Female) %>%
    dplyr::select(.data$date, .data$Male, .data$Female) %>%
    tidyr::pivot_longer(
      cols = !.data$date,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      value = 100 * (.data$value
        / .data$value[.data$date == as.Date("2020-03-31")] - 1),
      tooltip = paste0(
        .data$indicator, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

  latest_female <- df %>%
    dplyr::filter(
      .data$indicator == "Female",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  title <- paste0(
    "The number of Victorian women receiving jobactive assistance is ",
    dplyr::case_when(
      latest_female > 0 ~ paste0(abs(latest_female), " per cent higher than "),
      latest_female == 0 ~ "the same as ",
      latest_female < 0 ~ paste0(abs(latest_female), " per cent lower than ")
    ),
    "it was in March 2020"
  )

  df %>%
    djpr_ts_linechart(
      col_var = .data$indicator,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = title,
      subtitle = "Victorian jobactive caseload by sex, cumulative change since March 2020",
      caption = caption_jobactive()
    )
}

viz_gr_female_jobactive_bar <- function(data = filter_dash_data(c(
                                          "jobactive_female_ballarat",
                                          "jobactive_female_bendigo",
                                          "jobactive_female_barwon",
                                          "jobactive_female_gippsland",
                                          "jobactive_female_goulburn/murray",
                                          "jobactive_female_inner metropolitan melbourne",
                                          "jobactive_female_north eastern melbourne",
                                          "jobactive_female_north western melbourne",
                                          "jobactive_female_south coast of victoria",
                                          "jobactive_female_south eastern melbourne and peninsula",
                                          "jobactive_female_western melbourne",
                                          "jobactive_female_wimmera mallee"
                                        ),
                                        df = dash_data
                                        )) {
  df <- data %>%
    dplyr::select(
      .data$date, .data$series,
      .data$unit, .data$value
    ) %>%
    dplyr::mutate(
      split_series = stringr::str_split_fixed(.data$series,
        pattern = " ; ",
        n = 3
      ),
      jobactive = .data$split_series[, 1],
      indicator = .data$split_series[, 2],
      region = .data$split_series[, 3]
    ) %>%
    dplyr::select(-.data$split_series, -.data$series, -.data$jobactive, -.data$indicator) %>%
    dplyr::mutate(
      value = .data$value * 1000
    )

  # reduce to only latest month
  df <- df %>%
    dplyr::group_by(.data$region, ) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  # value = scales::comma(.data$value * 1000),

  high_low <- df %>%
    summarise(
      min_region = .data$region[.data$value == min(.data$value)],
      min_caseload = .data$value[.data$value == min(.data$value)],
      max_region = .data$region[.data$value == max(.data$value)],
      max_caseload = .data$value[.data$value == max(.data$value)],
      date = max(.data$date)
    )

  title <- paste0(
    "The number of women receiving jobactive assistance ranged from ",
    scales::comma(round2(high_low$min_caseload, 1),
      accuracy = 1
    ),
    " in ",
    high_low$min_region,
    " to ",
    scales::comma(round2(high_low$max_caseload, 1),
      accuracy = 1
    ),
    " in ",
    high_low$ max_region,
    " as at ",
    format(high_low$date, "%B %Y")
  )

  # draw bar chart for all employment regions
  df %>%
    dplyr::mutate(region = gsub(
      "South Eastern Melbourne",
      "SE Melbourne",
      .data$region
    )) %>%
    ggplot(aes(
      x = stats::reorder(.data$region, .data$value),
      y = .data$value
    )) +
    geom_col(
      col = "grey85",
      aes(fill = -.data$value)
    ) +
    geom_text(
      nudge_y = 5,
      aes(label = paste0(
        scales::comma(round2(.data$value, 0),
          accuracy = 1
        )
      )),
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
    labs(
      title = title,
      subtitle = paste0(
        "Female Victorians jobactive caseload by employment region, ",
        format(max(data$date), "%B %Y")
      ),
      caption = caption_jobactive()
    )
}
