
#' Line chart of cumulative employment change since March 2020
#' in Victoria and Australia
#' @noRd
#' @examples
#' \dontrun{
#' dash_data <- load_dash_data()
#' viz_ind_emp_sincecovid_line()
#' }
#'
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

  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::mutate(value = 100 * ((.data$value
      / .data$value[.data$date == as.Date("2020-03-01")]) - 1))

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
      latest_vic > 0 ~ paste0(latest_vic, " per cent higher than "),
      latest_vic == 0 ~ "the same as ",
      latest_vic < 0 ~ paste0(latest_vic, " per cent lower than ")
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
      subtitle = "Cumulative change in employment since March 2020, per cent",
      caption = caption_lfs()
    )
}

viz_ind_empgro_line <- function(data = filter_dash_data(c(
                                  "A84423349V",
                                  "A84423043C"
                                ))) {
  df <- data %>%
    dplyr::mutate(state = dplyr::if_else(.data$state == "", "Australia", .data$state)) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::group_by(.data$indicator, .data$state) %>%
    dplyr::mutate(value = 100 * ((.data$value / lag(.data$value, 12)) - 1)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  vic_latest <- df %>%
    dplyr::filter(.data$state == "Victoria" &
      .data$date == max(.data$date)) %>%
    dplyr::pull(.data$value)

  aus_latest <- df %>%
    dplyr::filter(.data$state == "Australia" &
      .data$date == max(.data$date)) %>%
    dplyr::pull(.data$value)

  latest_month <- format(max(df$date), "%B %Y")

  title <- dplyr::if_else(
    vic_latest > aus_latest,
    paste0("Employment growth in Victoria outpaced Australia as a whole in the 12 months to ", latest_month),
    paste0("Employment growth in Victoria lagged behind Australia as a whole in the 12 months to ", latest_month)
  )

  df %>%
    djpr_ts_linechart(
      col_var = .data$state,
      y_labels = function(x) paste0(x, "%"),
      label_num = paste0(round(.data$value, 1), "%")
    ) +
    labs(
      subtitle = "Annual employment growth in Victoria and Australia, per cent",
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
    vic_rank == 8 ~ "is the lowest in Australia",
    vic_rank == 7 ~ "is the second lowest in Australia",
    vic_rank == 6 ~ "is the third lowest in Australia",
    vic_rank == 5 ~ "is the fourth lowest in Australia",
    vic_rank < 5 &
      df_wide$max_date[df_wide$state == "Victoria"] < df_wide$min_date[df_wide$state == "Victoria"] ~
    "has fallen over the past year",
    TRUE ~ "compared to other states and territories"
  )

  title <- paste0("Victoria's unemployment rate ", title)

  df %>%
    ggplot(aes(x = stats::reorder(.data$state, .data$value),
               y = .data$value, col = format(.data$date, "%b %Y"))) +
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
        round2(.data$value, 1)
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


viz_ind_emppop_state_slope <- function(data = filter_dash_data(c(
                                         "A84423272J",
                                         "A84423356T",
                                         "A84423286W",
                                         "A84423370L",
                                         "A84423328J",
                                         "A84423300F",
                                         "A84423314V",
                                         "A84423342C"
                                       ))) {
  df <- data %>%
    dplyr::filter(date %in% c(
      max(.data$date),
      subtract_years(max(.data$date), 1)
    )) %>%
    dplyr::mutate(
      state_abbr = strayr::clean_state(.data$state),
      state_group = dplyr::if_else(.data$state_abbr %in% c(
        "Vic", "NSW"
      ),
      .data$state_abbr,
      "Other"
      )
    )

  latest <- df %>%
    dplyr::filter(
      .data$date == max(.data$date),
      !.data$state_abbr %in% c("ACT", "NT")
    ) %>%
    dplyr::select(.data$state_abbr, .data$value) %>%
    dplyr::mutate(rank = dplyr::min_rank(-.data$value))

  vic_rank <- latest$rank[latest$state_abbr == "Vic"]
  nsw_rank <- latest$rank[latest$state_abbr == "NSW"]
  vic_level <- paste0(round2(latest$value[latest$state_abbr == "Vic"], 1), "%")
  vic_change <- df %>%
    dplyr::filter(.data$state_abbr == "Vic") %>%
    dplyr::summarise(change = .data$value[.data$date == max(.data$date)] -
      .data$value[.data$date == subtract_years(max(.data$date), 1)]) %>%
    dplyr::pull(.data$change)

  title <- dplyr::case_when(
    vic_rank == 1 ~ paste0(vic_level, " of Victorian adults are employed, the highest ratio of any Australian state"),
    vic_rank == 2 ~ paste0(vic_level, " of Victorian adults are employed, the second highest ratio of any Australian state"),
    vic_rank == 3 ~ paste0(vic_level, " of Victorian adults are employed, the third highest ratio of any Australian state"),
    vic_rank <= 4 & vic_change > 0 ~ paste0("Victoria's employment to population ratio rose by ", round2(vic_change, 1), " percentage points in the year to ", format(max(df$date), "%B %Y")),
    vic_rank < nsw_rank ~ "Victoria's employment to population ratio is higher than the ratio in New South Wales",
    TRUE ~ "Victoria's employment to population ratio compared to other states and territories"
  )

  df %>%
    ggplot(aes(
      x = .data$date, y = .data$value,
      col = .data$state_group, group = .data$state
    )) +
    geom_line() +
    ggiraph::geom_point_interactive(aes(tooltip = paste0(
      .data$state_abbr, "\n",
      round2(.data$value, 1)
    )),
    size = 3,
    shape = "circle filled",
    stroke = 1.5,
    fill = "white"
    ) +
    ggrepel::geom_text_repel(
      direction = "y",
      data = ~ dplyr::filter(., date == max(.data$date)),
      aes(label = .data$state_abbr),
      size = 14 / .pt,
      min.segment.length = 25,
      nudge_x = 15
    ) +
    theme_djpr() +
    scale_x_date(
      breaks = unique(df$date),
      expand = expansion(add = c(10, 50)),
      date_labels = "%B\n%Y"
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(5),
      labels = function(x) paste0(x, "%")
    ) +
    scale_colour_manual(values = c(
      "Vic" = djprtheme::djpr_royal_blue,
      "NSW" = djprtheme::djpr_green,
      "Other" = "grey70"
    )) +
    theme(axis.title = element_blank()) +
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
      aes(label = paste0(round(.data$value, 1), "%")),
      colour = "black",
      hjust = 0,
      size = 12 / .pt
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(
      values = c(
        "Vic" = djprtheme::djpr_royal_blue,
        "Aus" = djprtheme::djpr_green,
        "Other" = "grey70"
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
    mutate(geog = if_else(state == "", "Australia", state))

  latest_values <- data %>%
    filter(date == max(date)) %>%
    mutate(
      value = round(value, 1),
      date = format(date, "%B %Y")
    ) %>%
    select(geog, value, date) %>%
    tidyr::spread(key = geog, value = value)

  title <- dplyr::case_when(
    latest_values$Victoria > latest_values$Australia ~
    paste0("Victoria's unemployment rate in ", latest_values$date, " was higher than Australia's"),
    latest_values$Victoria < latest_values$Australia ~
    paste0("Victoria's unemployment rate in ", latest_values$date, " was lower than Australia's"),
    latest_values$Victoria == latest_values$Australia ~
    paste0("Victoria's unemployment rate in ", latest_values$date, " was the same as Australia's"),
    TRUE ~ "Unemployment rate in Victoria and Australia"
  )

  data %>%
    djpr_ts_linechart(
      col_var = geog,
      label_num = paste0(round(.data$value, 1), "%")
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
      expand = expansion(mult = c(0, 0.05))
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
    round(label_df$value[label_df$under == "Underutilisation rate"], 1),
    " per cent of the Victorian labour force was either unemployed",
    " or underemployed"
  )

  data %>%
    dplyr::filter(!grepl("Underutilisation", .data$series)) %>%
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
        dplyr::filter(grepl("Underutilisation", .data$series)),
      size = 0.5,
      colour = "black"
    ) +
    scale_fill_manual(values = c(
      "Unemployment rate" = djprtheme::djpr_royal_blue,
      "Underemployment rate" = djprtheme::djpr_green,
      "Underutilisation rate" = "black"
    )) +
    scale_colour_manual(values = c(
      "Unemployment rate" = djprtheme::djpr_royal_blue,
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

viz_ind_partrate_line <- function(data = filter_dash_data(c(
                                    "A84423355R",
                                    "A84423051C"
                                  ),
                                  df = dash_data
                                  )) {
  data <- data %>%
    mutate(geog = if_else(state == "", "Australia", state))

  latest_values <- data %>%
    filter(date == max(date)) %>%
    mutate(
      value = round(value, 1),
      date = format(date, "%B %Y")
    ) %>%
    select(geog, value, date) %>%
    tidyr::spread(key = geog, value = value)

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
      col_var = geog,
      label_num = paste0(round(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(
      subtitle = "Participation rate in Victoria and Australia",
      caption = caption_lfs(),
      title = title
    )
}
