
#' Line chart of cumulative employment change since March 2020
#' in Victoria and Australia
#' @noRd
#' @examples
#' \dontrun{
#' dash_data <- load_dash_data()
#' viz_ind_emp_sincecovid_line()
#' }
#'
viz_ind_emp_sincecovid_line <- function(data = filter_dash_data(c("A84423043C", "A84423349V"),
                                          df = dash_data
                                        )) {
  df <- data %>%
    dplyr::mutate(state = dplyr::if_else(state == "", "Australia", state))

  df <- df %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(value = 100 * ((value / value[date == as.Date("2020-03-01")]) - 1))

  max_date <- df %>%
    dplyr::filter(date == max(.data$date))

  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      state,
      "\n",
      format(
        .data$date,
        "%b %Y"
      ),
      "\n",
      round(.data$value, 1)
    ))

  days_in_data <- as.numeric(max(df$date) - min(df$date))

  lab_df <- max_date %>%
    dplyr::mutate(label = paste0(
      stringr::str_wrap(state, 10),
      "\n",
      paste0(
        stringr::str_wrap(round(.data$value, 1), 10),
        "%"
      )
    ))

  df %>%
    ggplot(aes(
      x = .data$date,
      y = .data$value,
      col = .data$state
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    scale_colour_discrete(palette = djprtheme::djpr_pal) +
    geom_point(
      data = max_date,
      fill = "white",
      stroke = 1.5,
      size = 2.5,
      shape = 21
    ) +
    ggrepel::geom_label_repel(
      data = lab_df,
      aes(label = label),
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
    scale_x_date(
      expand = expansion( # mult = c(0, 0.08)
        add = c(0, days_in_data * 0.18)
      ),
      date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.1),
      labels = function(x) paste0(x, "%")
    ) +
    djprtheme::theme_djpr() +
    theme(axis.title.x = element_blank()) +
    coord_cartesian(clip = "off") +
    labs(
      title = "Victorian employment compared to the national total",
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
    dplyr::mutate(value = 100 * ((value / lag(value, 12)) - 1)) %>%
    dplyr::filter(!is.na(value)) %>%
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
      col_var = state,
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
    dplyr::mutate(date_type = dplyr::if_else(date == min(date),
      "min_date",
      "max_date"
    )) %>%
    dplyr::select(state, value, date_type) %>%
    tidyr::spread(key = date_type, value = value) %>%
    dplyr::mutate(arrow_max = if_else(max_date > min_date,
      max(c(min_date, max_date - 0.05)),
      max_date + 0.05
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
  ) %>%
    paste0("Victoria's unemployment rate ", .)

  df %>%
    ggplot(aes(x = stats::reorder(state, value), y = value, col = format(date, "%b %Y"))) +
    geom_segment(
      data = df_wide,
      aes(
        x = stats::reorder(state, max_date),
        xend = stats::reorder(state, max_date),
        y = min_date,
        yend = arrow_max
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
      direction = "y",
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
      state_abbr = strayr::strayr(.data$state),
      state_group = dplyr::if_else(state_abbr %in% c(
        "Vic", "NSW"
      ),
      state_abbr,
      "Other"
      )
    )

  latest <- df %>%
    dplyr::filter(
      date == max(date),
      !state_abbr %in% c("ACT", "NT")
    ) %>%
    dplyr::select(state_abbr, value) %>%
    dplyr::mutate(rank = dplyr::min_rank(-value))

  vic_rank <- latest$rank[latest$state_abbr == "Vic"]
  nsw_rank <- latest$rank[latest$state_abbr == "NSW"]
  vic_level <- paste0(round2(latest$value[latest$state_abbr == "Vic"], 1), "%")
  vic_change <- df %>%
    dplyr::filter(state_abbr == "Vic") %>%
    dplyr::summarise(change = value[date == max(date)] - value[date == subtract_years(max(date), 1)]) %>%
    dplyr::pull(change)

  title <- dplyr::case_when(
    vic_rank == 1 ~ paste0(vic_level, " of Victorian adults are employed, the highest ratio of any Australian state"),
    vic_rank == 2 ~ paste0(vic_level, " of Victorian adults are employed, the second highest ratio of any Australian state"),
    vic_rank == 3 ~ paste0(vic_level, " of Victorian adults are employed, the third highest ratio of any Australian state"),
    vic_rank <= 4 & vic_change > 0 ~ paste0("Victoria's employment to population ratio rose by ", round2(vic_change, 1), " percentage points in the year to ", format(max(df$date), "%B %Y")),
    vic_rank < nsw_rank ~ "Victoria's employment to population ratio is higher than the ratio in New South Wales",
    TRUE ~ "Victoria's employment to population ratio compared to other states and territories"
  )

  df %>%
    ggplot(aes(x = date, y = value, col = state_group, group = state)) +
    geom_line() +
    ggiraph::geom_point_interactive(aes(tooltip = paste0(
      state_abbr, "\n",
      round2(value, 1)
    )),
    size = 3,
    shape = "circle filled",
    stroke = 1.5,
    fill = "white"
    ) +
    ggrepel::geom_text_repel(
      direction = "y",
      data = ~ dplyr::filter(., date == max(date)),
      aes(label = state_abbr),
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

viz_ind_underut_area <- function(data = filter_dash_data(c(
                                   "A85223450L",
                                   "A85223451R",
                                   "A84423354L"
                                 ))) {
  area_df <- data %>%
    dplyr::filter(!grepl("Underutilisation", series))

  area_df %>%
    ggplot(aes(x = date, y = value, fill = indicator)) +
    geom_col(
      position = "stack",
      col = NA
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
                                            ))
{

#name Australia
    data <- data %>%
      dplyr::mutate(state = dplyr::if_else(.data$state == "",
                                         "Australia",
                                         .data$state),
                  state = strayr::strayr(.data$state))

#select the latest date

  data<- data %>%
    dplyr::group_by(.data$state) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()


  # Create title long title
  vic_rank <- data%>%
    dplyr::filter(
      .data$state != "Australia",
      .data$date == max(.data$date)
    ) %>%

    dplyr::mutate(rank = dplyr::min_rank(-.data$value)) %>%
    dplyr::filter(.data$state == "Vic") %>%
    dplyr::pull(.data$rank)

  title <- dplyr::case_when(
    vic_rank == 8 ~ "was the lowest in Australia",
    vic_rank == 7 ~ "was the second lowest in Australia",
    vic_rank == 6 ~ "was the third lowest in Australia",
    vic_rank == 5 ~ "was the fourth lowest in Australia",
    vic_rank == 4 ~ "was the fourth highest in Australia",
    vic_rank == 3 ~ "was the third highest in Australia",
    vic_rank == 2 ~ "was the second highest in Australia",
    vic_rank == 1 ~ "was the highest in Australia",
    TRUE ~ "compared to to other states and territories")

  title <- paste0("Victoria's participation rate ", title,
                  " in ", format(max(data$date), "%B %Y") )

  # Create plot
  data %>%
    ggplot(aes(
      x = stats::reorder(.data$state, .data$value),
      y =.data$value
    )) +

    geom_col(
      aes(fill = -.data$value)
    ) +

    geom_text(
      nudge_y = 0.1,
      aes(label =paste0(round(.data$value, 1),"%")),
      colour = "black",
      hjust = 0,
      size = 12 / .pt
    ) +

  coord_flip(clip = "off") +
    #scale_fill_distiller(palette = "Blues") +
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
      caption = caption_lfs())

}

viz_ind_unemprate_line <- function(data = filter_dash_data(c(
                                                           "A84423354L",
                                                          "A84423050A"

),
df = dash_data
)) {

}
