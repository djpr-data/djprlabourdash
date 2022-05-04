# Functions to create Indicators page graphs

viz_ind_emp_sincecovid_line <- function(data = filter_dash_data(c(
                                          "A84423043C",
                                          "A84423349V"
                                        ),
                                        df = dash_data
                                        ) %>%
                                          dplyr::filter(date >=
                                            as.Date("2020-01-01"))) {
  df <- data %>%
    dplyr::mutate(state = dplyr::if_else(.data$state == "",
      "Australia",
      .data$state
    ))

  # tooltip added
  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::mutate(
      value = 100 * ((.data$value
        / .data$value[.data$date == as.Date("2020-03-01")]) - 1),
      tooltip = paste0(
        .data$state, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )


  latest_vic <- df %>%
    dplyr::filter(
      .data$state == "Victoria",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  title <- paste0(
    "The number of Victorians employed is ",
    dplyr::case_when(
      latest_vic > 0 ~ paste0(abs(latest_vic), " per cent higher than "),
      latest_vic == 0 ~ "the same as ",
      latest_vic < 0 ~ paste0(abs(latest_vic), " per cent lower than ")
    ),
    "it was in March 2020"
  )

  df %>%
    djpr_ts_linechart(
      col_var = .data$state,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = title,
      subtitle = "Cumulative change in employment since March 2020",
      caption = caption_lfs()
    )
}

viz_ind_empgro_line <- function(data = filter_dash_data(c(
                                  "A84423349V",
                                  "A84423043C"
                                ))) {
  df <- data %>%
    dplyr::mutate(state = dplyr::if_else(.data$state == "", "Australia", .data$state)) %>%
    dplyr::group_by(.data$indicator, .data$state) %>%
    dplyr::mutate(value = 100 * ((.data$value / lag(.data$value, 12)) - 1)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  # add tooltip
  df <- df %>%
    dplyr::mutate(
      tooltip = paste0(
        .data$state, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

  df_latest <- df %>%
    dplyr::filter(.data$date == max(.data$date))

  vic_latest <- df_latest %>%
    dplyr::filter(.data$state == "Victoria") %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  aus_latest <- df_latest %>%
    dplyr::filter(.data$state == "Australia") %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  latest_month <- format(unique(df_latest$date), "%B %Y")

  title <- dplyr::case_when(
    vic_latest > aus_latest ~
    "Employment growth in Victoria outpaced Australia as a whole in the 12 months to ",
    vic_latest < aus_latest ~
    "Employment growth in Victoria lagged behind Australia as a whole in the 12 months to ",
    vic_latest == aus_latest ~
    "Employment in Victoria grew at the same pace as the Australian total in the 12 months to"
  )

  title <- paste0(title, latest_month)

  df %>%
    djpr_ts_linechart(
      col_var = .data$state,
      y_labels = function(x) paste0(x, "%"),
      label_num = paste0(round2(.data$value, 1), "%")
    ) +
    labs(
      subtitle = "Annual employment growth in Victoria and Australia",
      caption = caption_lfs(),
      title = title
    )
}

viz_ind_unemp_states_dot <- function(data = filter_dash_data(
                                       c(
                                         "A84423354L",
                                         "A84423270C",
                                         "A84423368A",
                                         "A84423340X",
                                         "A84423326C",
                                         "A84423284T",
                                         "A84423312R",
                                         "A84423298F",
                                         "A84423050A"
                                       )
                                     )) {
  df <- data %>%
    dplyr::mutate(state = dplyr::if_else(.data$state == "",
      "Australia",
      .data$state
    )) %>%
    dplyr::mutate(state = dplyr::if_else(.data$state == "Australian Capital Territory",
      "ACT",
      .data$state
    )) %>%
    dplyr::group_by(.data$state) %>%
    dplyr::filter(.data$date %in% c(
      max(.data$date),
      subtract_years(max(.data$date), 1)
    ))

  df_wide <- df %>%
    dplyr::mutate(date_type = dplyr::if_else(.data$date == min(.data$date),
      "min_date",
      "max_date"
    )) %>%
    dplyr::select(.data$state, .data$value, .data$date_type) %>%
    tidyr::spread(key = .data$date_type, value = .data$value) %>%
    dplyr::mutate(arrow_max = if_else(.data$max_date > .data$min_date,
      max(c(.data$min_date, .data$max_date - 0.05)),
      .data$max_date + 0.05
    ))


  vic_rank <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$state != "Australia",
      .data$date == max(.data$date)
    ) %>%
    dplyr::mutate(rank = dplyr::min_rank(-.data$value)) %>%
    dplyr::filter(.data$state == "Victoria") %>%
    dplyr::pull(.data$rank)

  title <- dplyr::case_when(
    vic_rank == 8 ~ "is the lowest of all Australian states and territories",
    vic_rank == 7 ~ "is the second lowest of all Australian states and territories",
    vic_rank == 6 ~ "is the third lowest of all Australian states and territories",
    vic_rank == 5 ~ "is the fourth lowest of all Australian states and territories",
    vic_rank < 5 &
      df_wide$max_date[df_wide$state == "Victoria"] < df_wide$min_date[df_wide$state == "Victoria"] ~
    "has fallen over the past year",
    TRUE ~ "compared to other states and territories"
  )

  title <- paste0("Victoria's unemployment rate ", title)

  df %>%
    ggplot(aes(
      x = stats::reorder(.data$state, .data$value),
      y = .data$value, col = format(.data$date, "%b %Y")
    )) +
    geom_segment(
      data = df_wide,
      aes(
        x = stats::reorder(.data$state, .data$max_date),
        xend = stats::reorder(.data$state, .data$max_date),
        y = .data$min_date,
        yend = .data$arrow_max
      ),
      colour = djprtheme::djpr_cool_grey_11,
      arrow = arrow(
        length = unit(0.5, "lines"),
        type = "closed",
        angle = 25
      ),
      inherit.aes = F
    ) +
    ggiraph::geom_point_interactive(
      size = 4,
      aes(tooltip = paste0(
        format(.data$date, "%b %Y"),
        "\n",
        round2(.data$value, 1), "%"
      ))
    ) +
    ggrepel::geom_label_repel(
      data = ~ dplyr::filter(., .data$state == "Victoria"),
      aes(label = format(.data$date, "%b %Y")),
      direction = "x",
      label.padding = unit(0.1, "lines"),
      size = 14 / .pt,
      min.segment.length = unit(10000, "lines"),
      label.size = NA,
      nudge_x = 0.33
    ) +
    theme_djpr(flipped = T) +
    coord_flip() +
    djpr_colour_manual(2) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      y = "Unemployment rate",
      subtitle = paste0(
        "Unemployment rate in Australian states and territories in ",
        unique(df$date) %>% format("%B %Y") %>% paste0(collapse = " and ")
      ),
      title = title,
      caption = caption_lfs()
    )
}

viz_ind_emppop_state_line <- function(data = filter_dash_data(c(
                                        "A84423272J",
                                        "A84423356T",
                                        "A84423286W",
                                        "A84423370L",
                                        "A84423328J",
                                        "A84423300F",
                                        "A84423314V",
                                        "A84423342C"
                                      ),
                                      df = dash_data
                                      ) %>%
                                        dplyr::mutate(
                                          state = dplyr::case_when(
                                            .data$series == "Employment to population ratio ;  Persons ;  > Victoria ;" ~
                                            "Vic",
                                            .data$series == "Employment to population ratio ;  Persons ;  > New South Wales ;" ~
                                            "NSW",
                                            .data$series == "Employment to population ratio ;  Persons ;  > Queensland ;" ~
                                            "QLD",
                                            .data$series == "Employment to population ratio ;  Persons ;  > Northern Territory ;" ~
                                            "NT",
                                            .data$series == "Employment to population ratio ;  Persons ;  > Western Australia ;" ~
                                            "WA",
                                            .data$series == "Employment to population ratio ;  Persons ;  > South Australia ;" ~
                                            "SA",
                                            .data$series == "Employment to population ratio ;  Persons ;  > Tasmania ;" ~
                                            "Tas",
                                            .data$series == "Employment to population ratio ;  Persons ;  > Australian Capital Territory ;" ~
                                            "ACT",
                                            TRUE ~ .data$state
                                          )
                                        ),
                                      non_filtered_latest = filter_dash_data(
                                        df = dash_data,
                                        series_ids = c(
                                          "A84423272J",
                                          "A84423356T",
                                          "A84423286W",
                                          "A84423370L",
                                          "A84423328J",
                                          "A84423300F",
                                          "A84423314V",
                                          "A84423342C"
                                        )
                                      ) %>%
                                        dplyr::filter(
                                          .data$date == max(.data$date),
                                          !(.data$state %in% c(
                                            "Northern Territory",
                                            "Australian Capital Territory"
                                          )
                                          )
                                        ) %>%
                                        dplyr::arrange(-.data$value)) {
  df <- data %>%
    dplyr::mutate(
      state_group = dplyr::if_else(.data$state %in% c(
        "Vic", "NSW"
      ),
      .data$state,
      "Other"
      )
    )

  vic_rank <- which(non_filtered_latest$state == "Victoria")
  vic_level <- non_filtered_latest %>%
    dplyr::filter(.data$state == "Victoria") %>%
    dplyr::pull(.data$value) %>%
    round(1) %>%
    paste0("%")

  title <- dplyr::case_when(
    vic_rank == 1 ~ paste0(vic_level, " of Victorian adults are employed, the highest ratio of any Australian state"),
    vic_rank == 2 ~ paste0(vic_level, " of Victorian adults are employed, the second highest ratio of any Australian state"),
    vic_rank == 3 ~ paste0(vic_level, " of Victorian adults are employed, the third highest ratio of any Australian state"),
    TRUE ~ "Victoria's employment to population ratio compared to other states and territories"
  )

  other_colour <- djprtheme::djpr_cool_grey_11

  df %>%
    djpr_ts_linechart(
      col_var = .data$state,
      label_num = paste0(round2(.data$value, 1), "%")
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(5),
      labels = function(x) paste0(x, "%")
    ) +
    scale_colour_manual(values = c(
      "Vic" = djprtheme::djpr_blue,
      "NSW" = djprtheme::djpr_green,
      "NT" = other_colour,
      "Tas" = other_colour,
      "SA" = other_colour,
      "QLD" = other_colour,
      "WA" = other_colour,
      "ACT" = other_colour
    )) +
    labs(
      title = title,
      subtitle = "Employment to population ratio in Australian states and territories",
      caption = caption_lfs()
    )
}

viz_ind_partrate_bar <- function(data = filter_dash_data(c(
                                   "A84423355R",
                                   "A84423271F",
                                   "A84423369C",
                                   "A84423341A",
                                   "A84423327F",
                                   "A84423285V",
                                   "A84423313T",
                                   "A84423299J",
                                   "A84423051C"
                                 ),
                                 df = dash_data
                                 )) {

  # name Australia
  data <- data %>%
    dplyr::mutate(
      state = dplyr::if_else(.data$state == "",
        "Australia",
        .data$state
      ),
      state = strayr::clean_state(.data$state)
    )

  # select the latest date

  data <- data %>%
    dplyr::group_by(.data$state) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()


  # Create title long title
  vic_rank <- data %>%
    dplyr::filter(
      .data$state != "Australia",
      .data$date == max(.data$date)
    ) %>%
    dplyr::mutate(rank = dplyr::min_rank(-.data$value)) %>%
    dplyr::filter(.data$state == "Vic") %>%
    dplyr::pull(.data$rank)

  # title <- dplyr::case_when(
  #   vic_rank == 8 ~ "was the lowest in Australia",
  #   vic_rank == 7 ~ "was the second lowest in Australia",
  #   vic_rank == 6 ~ "was the third lowest in Australia",
  #   vic_rank == 5 ~ "was the fourth lowest in Australia",
  #   vic_rank == 4 ~ "was the fourth highest in Australia",
  #   vic_rank == 3 ~ "was the third highest in Australia",
  #   vic_rank == 2 ~ "was the second highest in Australia",
  #   vic_rank == 1 ~ "was the highest in Australia",
  #   TRUE ~ "compared to to other states and territories"
  # )
  #
  # title <- paste0(
  #   "Victoria's participation rate ", title,
  #   " in ", format(max(data$date), "%B %Y")
  # )

  title <- "Victoria's participation rate compared to other states and territories"

  data <- data %>%
    mutate(fill_col = dplyr::if_else(
      .data$state %in% c("Vic", "Aus"), .data$state, "Other"
    ))

  # Create plot
  data %>%
    ggplot(aes(
      x = stats::reorder(.data$state, .data$value),
      y = .data$value
    )) +
    geom_col(
      aes(fill = .data$fill_col),
      alpha = 0.9
    ) +
    geom_text(
      nudge_y = 0.1,
      aes(label = paste0(round2(.data$value, 1), "%")),
      colour = "black",
      hjust = 0,
      size = 12 / .pt
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(
      values = c(
        "Vic" = djprtheme::djpr_blue,
        "Aus" = djprtheme::djpr_green,
        "Other" = djprtheme::djpr_cool_grey_11
      )
    ) +
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
      subtitle = "Participation rate in Australian states and territories",
      caption = caption_lfs()
    )
}

viz_ind_unemprate_line <- function(data = filter_dash_data(c(
                                     "A84423354L",
                                     "A84423050A"
                                   ),
                                   df = dash_data
                                   )) {
  data <- data %>%
    mutate(geog = if_else(.data$state == "", "Australia", .data$state))

  latest_values <- data %>%
    dplyr::filter(date == max(.data$date)) %>%
    dplyr::mutate(
      value = round2(.data$value, 1),
      date = format(.data$date, "%B %Y")
    ) %>%
    dplyr::select(.data$geog, .data$value, .data$date) %>%
    tidyr::spread(key = .data$geog, value = .data$value)

  title <- dplyr::case_when(
    latest_values$Victoria > latest_values$Australia ~
    paste0("Victoria's unemployment rate in ", latest_values$date, " was higher than Australia's"),
    latest_values$Victoria < latest_values$Australia ~
    paste0("Victoria's unemployment rate in ", latest_values$date, " was lower than Australia's"),
    latest_values$Victoria == latest_values$Australia ~
    paste0("Victoria's unemployment rate in ", latest_values$date, " was the same as Australia's"),
    TRUE ~ "Unemployment rate in Victoria and Australia"
  )

  # add tooltip
  data <- data %>%
    dplyr::mutate(
      tooltip = paste0(
        .data$geog, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )


  data %>%
    djpr_ts_linechart(
      col_var = .data$geog,
      label_num = paste0(round2(.data$value, 1), "%")
    ) +
    labs(
      subtitle = "Unemployment rate in Victoria and Australia",
      caption = caption_lfs(),
      title = title
    ) +
    scale_y_continuous(
      limits = function(x) c(0, x[2]),
      labels = function(x) paste0(x, "%"),
      breaks = scales::breaks_pretty(5),
      expand = expansion(mult = c(0, 0.1))
    )
}


viz_ind_underut_area <- function(data = filter_dash_data(c(
                                   "A85223450L",
                                   "A85223451R",
                                   "A84423354L"
                                 ),
                                 df = dash_data
                                 )) {
  data <- data %>%
    dplyr::mutate(under = if_else(.data$indicator == "Underemployment rate (proportion of labour force)",
      "Underemployment rate",
      .data$indicator
    ))

  label_df <- data %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::mutate(series_order = dplyr::case_when(
      .data$under == "Unemployment rate" ~ 1,
      .data$under == "Underemployment rate" ~ 2,
      .data$under == "Underutilisation rate" ~ 3,
      TRUE ~ NA_real_
    )) %>%
    dplyr::arrange(.data$series_order) %>%
    dplyr::select(.data$date, .data$value, .data$under) %>%
    dplyr::mutate(
      label = paste0(
        if_else(.data$under == "Underemployment rate",
          "Underemp. rate",
          .data$under
        ),
        " ", round2(.data$value, 1), "%"
      ),
      label_y = if_else(.data$under == "Underutilisation rate",
        .data$value,
        (cumsum(.data$value) - .data$value) + (.data$value / 2)
      )
    )

  title <- paste0(
    "In ", format(unique(label_df$date), "%B %Y"), ", ",
    round2(label_df$value[label_df$under == "Underutilisation rate"], 1),
    " per cent of the Victorian labour force was either unemployed",
    " or underemployed"
  )

  data %>%
    dplyr::filter(!grepl("Underutilisation", .data$series, fixed = TRUE)) %>%
    ggplot(aes(x = .data$date, y = .data$value, fill = .data$under)) +
    geom_area(colour = NA) +
    geom_label(
      data = label_df,
      inherit.aes = FALSE,
      aes(
        y = .data$label_y,
        x = .data$date,
        label = stringr::str_wrap(.data$label, 10),
        colour = .data$under
      ),
      label.size = 0,
      label.padding = unit(0.1, "lines"),
      size = 12 / .pt,
      hjust = 0
    ) +
    geom_line(
      data = data %>%
        dplyr::filter(grepl("Underutilisation", .data$series, fixed = TRUE)),
      size = 0.5,
      colour = "black"
    ) +
    scale_fill_manual(values = c(
      "Unemployment rate" = djprtheme::djpr_blue,
      "Underemployment rate" = djprtheme::djpr_green,
      "Underutilisation rate" = "black"
    )) +
    scale_colour_manual(values = c(
      "Unemployment rate" = djprtheme::djpr_blue,
      "Underemployment rate" = djprtheme::djpr_green,
      "Underutilisation rate" = "black"
    )) +
    theme_djpr() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_text(size = 12)
    ) +
    scale_x_date(
      expand = expansion(mult = c(.02, .25)),
      date_labels = "%b\n %Y",
      breaks = djprtheme::breaks_right(
        limits = c(
          min(data$date),
          max(data$date)
        )
      )
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(add = c(0, 0.5))
    ) +
    labs(
      subtitle = "Labour force underutilisation in Victoria",
      caption = caption_lfs(),
      title = title
    )
}


viz_ind_hoursworked_line <- function(data = filter_dash_data(c(
                                       "A84426256L",
                                       "A84426277X",
                                       "A84423689R",
                                       "A84423091W"
                                     ),
                                     df = dash_data
                                     )) {
  data <- data %>%
    mutate(geog = if_else(.data$state == "",
      "Australia",
      .data$state
    )) %>%
    dplyr::select(
      .data$indicator, .data$date,
      .data$value, .data$geog
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value
    ) %>%
    dplyr::rename(
      civ_pop = starts_with("Civilian population"),
      hours = starts_with("Monthly hours")
    ) %>%
    dplyr::mutate(value = .data$hours / .data$civ_pop) %>%
    dplyr::filter(!is.na(.data$value))

  latest_values <- data %>%
    dplyr::filter(date == max(.data$date)) %>%
    dplyr::mutate(
      value = round2(.data$value, 1),
      date = format(.data$date, "%B %Y")
    ) %>%
    dplyr::select(
      .data$geog,
      .data$value,
      .data$date
    ) %>%
    tidyr::spread(
      key = .data$geog,
      value = .data$value
    )

  title <- dplyr::case_when(
    latest_values$Victoria > latest_values$Australia ~
    paste0("Victorian adults worked more hours on average in ", latest_values$date, " than Australian adults"),
    latest_values$Victoria < latest_values$Australia ~
    paste0("Victorian adults worked fewer hours on average in ", latest_values$date, " than Australian adults"),
    latest_values$Victoria == latest_values$Australia ~
    paste0("In ", latest_values$date, ", Victorian and Australian adults worked the same number of hours on average"),
    TRUE ~ "Monthly hours worked per civilian population in Victoria and Australia"
  )

  data %>%
    djpr_ts_linechart(
      col_var = .data$geog
    ) +
    labs(
      subtitle = "Average monthly hours worked per civilian adult in Victoria and Australia",
      caption = paste0(caption_lfs(), " Civilian adults are all residents aged 15 and above who are not in active military service."),
      title = title
    )
}

# comparing participation and unemployment visualisation

viz_ind_partrate_un_line <- function(data = filter_dash_data(c(
                                       "A84423355R",
                                       "A84423354L"
                                     ),
                                     df = dash_data
                                     )) {
  df <- data %>%
    dplyr::select(.data$date, .data$value, .data$indicator) %>%
    dplyr::mutate(series = .data$indicator)

  min_year <- format(min(df$date), "%Y")
  max_year <- format(max(df$date), "%Y")

  # Add average for each indicator
  df <- df %>%
    dplyr::mutate(indicator = paste0("Average ", min_year, "-", max_year)) %>%
    dplyr::group_by(.data$series, .data$indicator) %>%
    dplyr::mutate(value = mean(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(df) %>%
    dplyr::mutate(
      tooltip =
        dplyr::if_else(
          .data$indicator == "Average",
          paste0("Average\n", round2(.data$value, 1), "%"),
          paste0(
            .data$indicator, "\n",
            format(.data$date, "%b %Y"), "\n",
            round2(.data$value, 1), "%"
          )
        )
    )

  # Create title
  latest_change <- df %>%
    dplyr::filter(!grepl("Average", .data$indicator, fixed = TRUE)) %>%
    dplyr::select(.data$date, .data$value, .data$indicator) %>%
    dplyr::group_by(.data$indicator) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(value = 100 * ((.data$value / dplyr::lag(.data$value, 1)) - 1)) %>%
    dplyr::filter(date == max(.data$date)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = .data$indicator,
      values_from = .data$value
    ) %>%
    dplyr::mutate(
      date = format(.data$date, "%B %Y")
    )


  title <- dplyr::case_when(
    latest_change$`Participation rate` > 0 & latest_change$`Unemployment rate` > 0 ~
    paste0("Both the participation rate and the unemployment rate increased in ", latest_change$date),
    latest_change$`Participation rate` > 0 & latest_change$`Unemployment rate` < 0 ~
    paste0("While the participation rate increased, the unemployment rate declined in ", latest_change$date),
    latest_change$`Participation rate` < 0 & latest_change$`Unemployment rate` > 0 ~
    paste0("While the participation rate declined, the unemployment rate increased in ", latest_change$date),
    TRUE ~ "Unemployment and participation rates, Victoria"
  )



  df %>%
    djpr_ts_linechart(
      col_var = .data$indicator,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%"),
      x_expand_mult = c(0, 0.22)
    ) +
    scale_colour_manual(values = rev(c(
      djpr_pal(2),
      djprtheme::djpr_cool_grey_11
    ))) +
    labs(
      subtitle = "Participation rate and unemployment rate for Victoria ",
      caption = caption_lfs(),
      title = title
    ) +
    facet_wrap(~series, ncol = 1, scales = "free_y")
}

viz_ind_partrate_un_scatter <- function(data = filter_dash_data(c(
                                          "A84423355R",
                                          "A84423354L"
                                        ),
                                        df = dash_data
                                        ),
                                        selected_period = c("month", "year")) {
  selected_period <- match.arg(selected_period)

  df <- data %>%
    dplyr::select(.data$date, .data$value, .data$indicator)

  df <- df %>%
    dplyr::group_by(.data$indicator) %>%
    dplyr::mutate(change = .data$value - lag(
      .data$value,
      dplyr::if_else(
        selected_period == "month",
        1,
        12
      )
    )) %>%
    dplyr::select(.data$date, .data$indicator, .data$change) %>%
    tidyr::spread(key = .data$indicator, value = .data$change) %>%
    dplyr::mutate(focus_date = if_else(.data$date == max(.data$date), TRUE, FALSE)) %>%
    dplyr::filter(!is.na(.data$`Unemployment rate`))


  quadrants <- dplyr::tibble(
    x = c(-0.5, 0.75, -0.5, 0.75),
    y = c(1.75, 1.75, -1.75, -1.75),
    label = c(
      "Unemployment \U2193\nParticipation \U2191",
      "Unemployment \U2191\nParticipation \U2191",
      "Unemployment \U2193\nParticipation \U2193",
      "Unemployment \U2191\nParticipation \U2193"
    )
  )

  if (selected_period == "year") {
    quadrants <- quadrants %>%
      dplyr::mutate(
        x = .data$x * 4,
        y = .data$y * 2.3
      )
  }

  latest_month <- df %>%
    filter(.data$date == max(.data$date))

  title <- case_when(
    latest_month$`Participation rate` > 0 &
      latest_month$`Unemployment rate` > 0 ~
    "Unemployment and participation both rose in ",
    latest_month$`Participation rate` < 0 &
      latest_month$`Unemployment rate` < 0 ~
    "Unemployment and participation both fell in ",
    latest_month$`Participation rate` > 0 &
      latest_month$`Unemployment rate` < 0 ~
    "Unemployment fell even as participation rose in ",
    latest_month$`Participation rate` < 0 &
      latest_month$`Unemployment rate` > 0 ~
    "Unemployment rose despite a fall in participation in "
  )

  title <- paste0(
    title,
    dplyr::if_else(selected_period == "month",
      "",
      "the year to "
    ),
    format(latest_month$date, "%B %Y")
  )

  df <- df %>%
    dplyr::mutate(
      tooltip =
        paste0(
          format(.data$date, "%b %Y"), "\n",
          "Unemployment: ",
          dplyr::if_else(.data$`Unemployment rate` >= 0,
            "\U2191",
            "\U2193"
          ),
          abs(round2(.data$`Unemployment rate`, 1)), " ppts.\n",
          "Participation: ",
          dplyr::if_else(.data$`Participation rate` >= 0,
            "\U2191",
            "\U2193"
          ),
          abs(round2(.data$`Participation rate`, 1)), " ppts."
        )
    )

  change_desc <- dplyr::if_else(selected_period == "month",
    "Monthly change",
    "Annual change"
  )

  df %>%
    ggplot(aes(
      x = .data$`Unemployment rate`,
      y = .data$`Participation rate`,
      col = .data$focus_date,
      alpha = .data$focus_date
    )) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_text(
      data = quadrants,
      inherit.aes = FALSE,
      size = 14 / .pt,
      lineheight = 0.9,
      colour = djprtheme::djpr_cool_grey_11,
      aes(x = .data$x, y = .data$y, label = .data$label)
    ) +
    ggiraph::geom_point_interactive(
      size = 2.5,
      aes(tooltip = .data$tooltip)
    ) +
    geom_text(
      data = ~ filter(., .data$date == max(.data$date)),
      aes(label = format(.data$date, "%b %Y")),
      size = 14 / .pt,
      nudge_y = dplyr::if_else(selected_period == "month", 0.15, 0.3)
    ) +
    djpr_colour_manual(2) +
    scale_x_continuous(labels = function(x) paste0(x, " ppts")) +
    scale_y_continuous(labels = function(x) paste0(x, " ppts")) +
    suppressWarnings(scale_alpha_discrete(range = c(0.25, 1))) +
    theme_djpr() +
    labs(
      y = paste0(change_desc, " in participation rate\n"),
      x = paste0(change_desc, " in unemployment rate"),
      caption = caption_lfs(),
      title = title,
      subtitle = paste0(
        change_desc,
        " in participation rate by ",
        tolower(change_desc),
        " in unemployment rate, March 1978 to ",
        max(df$date) %>% format("%B %Y")
      )
    ) +
    theme(axis.title.y = element_text(angle = 90))
}

viz_ind_partrate_line <- function(data = filter_dash_data(c(
                                    "A84423355R",
                                    "A84423051C"
                                  ),
                                  df = dash_data
                                  )) {
  data <- data %>%
    dplyr::mutate(
      geog = dplyr::if_else(.data$state == "", "Australia", .data$state),
      tooltip = paste0(
        .data$geog, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

  latest_values <- data %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::mutate(
      value = round2(.data$value, 1),
      date = format(.data$date, "%B %Y")
    ) %>%
    dplyr::select(.data$geog, .data$value, .data$date) %>%
    tidyr::spread(key = .data$geog, value = .data$value)

  title <- dplyr::case_when(
    latest_values$Victoria > latest_values$Australia ~
    paste0("Victoria's participation rate in ", latest_values$date, " was higher than Australia's"),
    latest_values$Victoria < latest_values$Australia ~
    paste0("Victoria's participation rate in ", latest_values$date, " was lower than Australia's"),
    latest_values$Victoria == latest_values$Australia ~
    paste0("Victoria's participation rate in ", latest_values$date, " was the same as Australia's"),
    TRUE ~ "Labour force participation in Victoria and Australia"
  )

  data %>%
    djpr_ts_linechart(
      col_var = .data$geog,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(
      subtitle = "Participation rate in Victoria and Australia",
      caption = caption_lfs(),
      title = title
    )
}

viz_ind_gen_full_part_line <- function(data = filter_dash_data(c(
                                         "pt_emp_vic",
                                         "A84423357V"
                                       ),
                                       df = dash_data
                                       ) %>%
                                         dplyr::filter(date >= as.Date("2020-01-01"))) {


  # preparing data to calculate indexed value

  df <- data %>%
    dplyr::group_by(.data$indicator) %>%
    dplyr::mutate(
      value = 100 * ((.data$value /
        .data$value[.data$date == as.Date("2020-03-01")]) - 1),
      tooltip = paste0(
        .data$indicator, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

  latest_full_time <- df %>%
    dplyr::filter(
      .data$indicator == "Employed full-time",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  latest_part_time <- df %>%
    dplyr::filter(
      .data$indicator == "Employed part-time",
      .data$date == max(.data$date)
    ) %>%
    dplyr::pull(.data$value) %>%
    round2(1)

  title <- paste0(
    "The number of Victorians employed full-time is ",
    dplyr::case_when(
      latest_full_time > 0 ~ paste0(latest_full_time, " per cent higher than "),
      latest_full_time == 0 ~ "the same as ",
      latest_full_time < 0 ~ paste0(latest_full_time, " per cent lower than ")
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
      subtitle = "Cumulative change in full-time and part-time employment since March 2020 for Victorian workers",
      caption = caption_lfs()
    )
}

viz_ind_effective_unemprate_line <- function(data = filter_dash_data(c(
                                               "A84423350C",
                                               "A84423351F",
                                               "A84423354L",
                                               "employed full-time_did not work (0 hours)_no work, not enough work available, or stood down_victoria",
                                               "employed part-time_did not work (0 hours)_no work, not enough work available, or stood down_victoria",
                                               "employed full-time_did not work (0 hours)_worked fewer hours than usual for other reasons_victoria",
                                               "employed part-time_did not work (0 hours)_worked fewer hours than usual for other reasons_victoria"
                                             ),
                                             df = dash_data
                                             ) %>%
                                               dplyr::filter(date >= as.Date("2019-06-01"))) {

  # split off em2b data and drop spare columns, filter out those who worked 0 hours and apply 3 months average
  # We are only interested in two reasons for working 0 hours: 'no work' and 'other reasons'
  zero_hours <- data %>%
    dplyr::filter(.data$series_id %in% c(
      "employed full-time_did not work (0 hours)_no work, not enough work available, or stood down_victoria",
      "employed full-time_did not work (0 hours)_worked fewer hours than usual for other reasons_victoria",
      "employed part-time_did not work (0 hours)_no work, not enough work available, or stood down_victoria",
      "employed part-time_did not work (0 hours)_worked fewer hours than usual for other reasons_victoria"
    )) %>%
    dplyr::select(.data$date, .data$series, .data$value) %>%
    tidyr::pivot_wider(
      names_from = .data$series,
      values_from = .data$value
    ) %>%
    dplyr::mutate(emp_zero_hours = .data$"Employed full-time ; Did not work (0 hours) ; No work, not enough work available, or stood down ; Victoria" +
      .data$"Employed full-time ; Did not work (0 hours) ; Worked fewer hours than usual for other reasons ; Victoria" +
      .data$"Employed part-time ; Did not work (0 hours) ; No work, not enough work available, or stood down ; Victoria" +
      .data$"Employed part-time ; Did not work (0 hours) ; Worked fewer hours than usual for other reasons ; Victoria") %>%
    dplyr::select(.data$date, .data$emp_zero_hours) %>%
    dplyr::mutate(emp_zero_hours = slider::slide_mean(.data$emp_zero_hours,
      before = 2L,
      complete = TRUE
    )) %>%
    dplyr::filter(!is.na(.data$emp_zero_hours))

  # clean up original data source
  unemp <- data %>%
    dplyr::select(.data$date, .data$series, .data$value) %>%
    tidyr::pivot_wider(
      names_from = .data$series,
      values_from = .data$value
    ) %>%
    dplyr::rename(
      unemp = starts_with("Unemployed"),
      lf = starts_with("Labour force")
    ) %>%
    dplyr::select(.data$date, .data$unemp, .data$lf)

  # Combine data sources and calculate effective unemp rate -----
  df <- unemp %>%
    dplyr::right_join(zero_hours, by = "date")

  df <- df %>%
    dplyr::mutate(
      `Unemployment rate` = 100 * (.data$unemp / .data$lf),
      `Effective unemployment rate` = 100 * ((.data$unemp + .data$emp_zero_hours) / .data$lf)
    ) %>%
    dplyr::select(.data$date, .data$`Unemployment rate`, .data$`Effective unemployment rate`) %>%
    tidyr::pivot_longer(
      names_to = "series",
      values_to = "value",
      cols = !.data$date
    )

  # Visualise -----
  max_date <- df %>%
    dplyr::filter(date == max(.data$date))

  # lockdown dates for shading
  #  start = end = NULL

  lockdown_dates <- dplyr::tibble(
    start = c(
      "2020-03-31",
      "2020-07-09",
      "2021-02-13",
      "2021-05-28",
      "2021-07-16",
      "2021-08-05"
    ),
    end = c(
      "2020-05-12",
      "2020-10-27",
      "2021-02-17",
      "2021-06-10",
      "2021-07-27",
      "2021-10-21"
    )
  ) %>%
    dplyr::mutate(across(everything(), as.Date))

  # line graph
  df %>%
    ggplot(aes(x = .data$date, y = .data$value, col = .data$series)) +
    geom_rect(
      data = lockdown_dates,
      aes(
        xmin = .data$start, xmax = .data$end,
        ymin = -Inf, ymax = Inf
      ),
      fill = "grey80",
      colour = "grey80",
      inherit.aes = F
    ) +
    geom_text(
      data = filter(
        lockdown_dates,
        .data$start == min(.data$start)
      ) %>%
        dplyr::mutate(
          label = "Shutdowns "
        ),
      aes(x = .data$start, y = 11, label = .data$label),
      hjust = 1,
      size = 14 / .pt,
      col = "grey80",
      inherit.aes = F
    ) +
    geom_line() +
    geom_point(
      data = max_date,
      fill = "white",
      stroke = 1.5, size = 2.5, shape = 21
    ) +
    geom_text(
      data = max_date,
      aes(label = paste0(
        stringr::str_wrap(.data$series, 8),
        " ",
        round(.data$value, 1),
        "%"
      )),
      lineheight = 0.9,
      nudge_x = 35,
      size = 14 / .pt,
      hjust = 0
    ) +
    theme_djpr() +
    scale_colour_manual(palette = djpr_pal) +
    scale_y_continuous(
      limits = function(x) c(0, x[2]),
      expand = expansion(add = c(0, 1)),
      breaks = seq(0, 16, 2),
      labels = function(x) paste0(x, "%")
    ) +
    scale_x_date(
      date_labels = "%b\n%Y",
      breaks = djprtheme::breaks_right(
        limits = c(
          min(df$date),
          max(df$date)
        ),
        n_breaks = 5
      ),
      expand = expansion(add = c(10, 160))
    ) +
    theme(axis.title.x = element_blank()) +
    labs(
      title = "Including zero-hours workers in the unemployment rate gives a clearer picture of the economic effects of COVID and lockdowns",
      subtitle = "Unemployment rate, with and without people working zero hours (per cent of labour force)",
      caption = paste0(caption_lfs_det_m(), "Zero-hours data smoothed using a 3 month rolling average.")
    )
}
