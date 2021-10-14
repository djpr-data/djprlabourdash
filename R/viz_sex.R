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

viz_gr_underemp_bysex_line <- function(data = filter_dash_data(c(
                                         "A85223418L",
                                         "A85223482F"
                                       ),
                                       df = dash_data
                                       )) {
  df <- data %>%
    dplyr::select(.data$date, .data$series, .data$value)

  df <- df %>%
    dplyr::mutate(series = dplyr::if_else(
      .data$series == "Underemployment rate (proportion of labour force) ;  > Males ;  > Victoria ;",
      "Males",
      "Females"
    ))


  latest_values <- df %>%
    dplyr::filter(date == max(.data$date)) %>%
    dplyr::mutate(
      value = round2(.data$value, 1),
      date = format(.data$date, "%B %Y")
    ) %>%
    # dplyr::select(.data$series, .data$value, .data$date) %>%
    tidyr::pivot_wider(
      names_from = .data$series,
      values_from = .data$value
    )

  title <- dplyr::case_when(
    latest_values$Females > latest_values$Males ~
    paste0("The underemployment rate in ", latest_values$date, " was higher for women than men"),
    latest_values$Females < latest_values$Males ~
    paste0("The underemployment rate in ", latest_values$date, " was lower for women than men"),
    latest_values$Females == latest_values$Males ~
    paste0("The underemployment rate in ", latest_values$date, " was the same for women and men"),
    TRUE ~ "Underemployment rate for men and women in Victoria"
  )

  # add tooltip
  df <- df %>%
    dplyr::mutate(
      tooltip = paste0(
        .data$series, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

  df %>%
    djpr_ts_linechart(
      col_var = .data$series,
      label_num = paste0(round2(.data$value, 1), "%")
    ) +
    labs(
      subtitle = "Underemployment rate by sex, Victoria, per cent of labour force",
      caption = caption_lfs(),
      title = title
    ) +
    scale_y_continuous(
      limits = function(x) c(0, x[2]),
      labels = function(x) paste0(x, "%"),
      breaks = scales::breaks_pretty(5),
      expand = expansion(mult = c(0, 0.05))
    )
}

viz_gr_women_emp_sincecovid_line <- function(data = filter_dash_data(c(
                                               "15-24_females_employed",
                                               "25-54_females_employed",
                                               "55+_females_employed"
                                             ),
                                             df = dash_data
                                             )) {
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
    dplyr::filter(date >= as.Date("2020-01-01"))

  df <- df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = 100 * ((.data$value /
      .data$value[.data$date == as.Date("2020-03-01")]) - 1))

  # add tooltip
  df <- df %>%
    dplyr::mutate(
      tooltip = paste0(
        .data$series, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )


  df <- df %>%
    mutate(series = gsub("Employed ; Females ; ", "", .data$series, fixed = TRUE))

  latest_month <- format(max(df$date), "%B %Y")

  # create latest data by age
  latest_youth <- df %>%
    dplyr::filter(.data$date == max(.data$date),
                  .data$series == "15-24") %>%
    dplyr::mutate(value = round2(.data$value, 1)) %>%
    dplyr::pull(.data$value)

  title <- dplyr::case_when(
    latest_youth < 0 ~ paste0("The number of young Victorian women in employment is ",
                              abs(latest_youth),
                              " per cent below its pre-COVID level"),
    latest_youth == 0 ~ "The number of young Victorian women in employment is the same as its pre-COVID level",
    latest_youth > 0 ~ paste0("The number of young Victorian women in employment is ",
                              abs(latest_youth),
                              " per cent above its pre-COVID level")
  )

  df %>%
    djpr_ts_linechart(
      col_var = .data$series,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%"),
      hline = 0
    ) +
    labs(
      title = title,
      subtitle = "Cumulative change in employment by age since March 2020 for Victorian women",
      caption = paste0(caption_lfs_det_m(), " Data smoothed using a 12 month rolling average.")
    )
}
