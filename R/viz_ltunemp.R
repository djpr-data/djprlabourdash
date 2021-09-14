# Functions to create the graphs for the 'Long-term unemployed' subpage on the dashboard.

viz_gr_ltunemp_line <- function(data = filter_dash_data(c(
                            "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
                            "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
                            "A84423687K",
                            "A84423089K",
                            "A84597681W"),
                            df = dash_data
                            )
                          )
{
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


  df <- df %>%
    dplyr::mutate(tooltip = paste0(
                    .data$state, "\n",
                    format(.data$date, "%b %Y"), "\n",
                    round2(.data$value, 1), "%"
                  )
                )

  latest_values <- df %>%
    dplyr::filter(date == max(.data$date)) %>%
    dplyr::select(-.data$tooltip) %>%
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
                          "unemployed total ('000)_victoria_under 4 weeks (under 1 month)"),
                          df = dash_data))
{
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

