
#' Line chart of cumulative employment change since March 2020
#' in Victoria and Australia
#' @noRd
#' @examples
#' \dontrun{
#' dash_data <- load_dash_data()
#' viz_ind_emp_sincecovid_line()
#' }
#'
title_ind_emp_sincecovid_line <- function(data = filter_dash_data(c("A84423043C", "A84423349V"),
                                            df = dash_data
                                          )) {
  df <- data %>%
    dplyr::filter(.data$date >= as.Date("2020-03-01")) %>%
    dplyr::mutate(state = dplyr::if_else(.data$state == "", "Australia", .data$state)) %>%
    dplyr::group_by(.data$state, .data$series) %>%
    dplyr::mutate(value = 100 * ((value / value[date == as.Date("2020-03-01")]) - 1)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  latest <- df %>%
    dplyr::select(value, state) %>%
    tidyr::spread(key = state, value = value)

  if (latest$Victoria < 0) {
    title <- "Victorian employment is below pre-COVID levels"
  } else {
    if (round(latest$Victoria, 1) < round(latest$Australia)) {
      title <- "Victorian employment is above pre-COVID levels, but growth lags behind the national total"
    } else if (round(latest$Victoria, 1) == round(latest$Australia, 1)) {
      title <- "Victorian employment is above pre-COVID levels, and has caught up with the national employment growth"
    } else {
      title <- "Victorian employment is above pre-COVID levels, and has grown faster than the national total"
    }
  }
  return(title)
}

viz_ind_emp_sincecovid_line <- function(data = filter_dash_data(c("A84423043C", "A84423349V"),
                                          df = dash_data
                                        ),
                                        title = title_ind_emp_sincecovid_line(data)) {
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
      paste0(stringr::str_wrap(round(.data$value, 1), 10),
             "%")
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
      fontface = "bold",
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
    dplyr::group_by(.data$indicator) %>%
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
    paste0("Employment growth in Victoria lagged behind Australia as a whole in 12 months to ", latest_month)
  )

  df %>%
    djpr_ts_linechart(col_var = state,
                      y_labels = function(x) paste0(x, "%")) +
    labs(subtitle = "Annual employment growth in Victoria and Australia, per cent",
         caption = caption_lfs(),
         title = title)

}

viz_ind_emppopratio_line <- function(data = filter_dash_data(c("A84423356T",
                                                                   "A84423244X",
                                                                   "A84423468K")
)) {

  df <- data %>%
    dplyr::mutate(sex = dplyr::if_else(.data$sex == "",
                                       "Persons",
                                       .data$sex))

  latest_year <- df %>%
    dplyr::group_by(sex) %>%
    dplyr::mutate(d_year = value - dplyr::lag(value, 12)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(.data$date, .data$sex, .data$d_year) %>%
    tidyr::spread(key = sex, value = d_year)

  nice_date <- format(latest_year$date, "%B %Y")

  title <- dplyr::case_when(
    latest_year$Females > 0 &
      latest_year$Males > 0 ~
      paste0("A larger proportion of Victorian men and women are in work in ",
             nice_date, " than a year earlier"),
    latest_year$Females > 0 &
      latest_year$Males < 0 ~
      paste0("The proportion of Victorian women in work rose over the year to ",
             nice_date, " but the male employment-to-population ratio fell"),
    latest_year$Females < 0 &
      latest_year$Males > 0 ~
      paste0("The proportion of Victorian men in work rose over the year to ",
             nice_date, " but the female employment-to-population ratio fell"),
    TRUE ~ "Employment-to-population ratio for Victorian men and women"
  )

  df %>%
    djpr_ts_linechart(col_var = sex,
                      y_labels = function(x) paste0(x, "%")) +
    labs(title = title,
         subtitle = "Employment to population ratio by sex, Victoria",
         caption = caption_lfs())

}

viz_ind_unemp_states_dot <- function(data = filter_dash_data(
  c("A84423354L",
    "A84423270C",
    "A84423368A",
    "A84423340X",
    "A84423326C",
    "A84423284T",
    "A84423312R",
    "A84423298F",
    "A84423050A")

)) {
  df <- data %>%
    dplyr::mutate(state = dplyr::if_else(.data$state == "",
                                  "Australia",
                                  .data$state)) %>%
    dplyr::mutate(state = dplyr::if_else(.data$state == "Australian Capital Territory",
                                         "ACT",
                                         .data$state)) %>%
    dplyr::group_by(.data$state) %>%
    dplyr::filter(.data$date %in% c(max(.data$date),
                                    subtract_years(max(.data$date), 1)))

  df_wide <- df %>%
    dplyr::mutate(date_type = dplyr::if_else(date == min(date),
                                             "min_date",
                                             "max_date")) %>%
    dplyr::select(state, value, date_type) %>%
    tidyr::spread(key = date_type, value = value) %>%
    dplyr::mutate(arrow_max = if_else(max_date > min_date,
                                      max_date - 0.05,
                                      max_date + 0.05))


  vic_rank <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$state != "Australia",
                  .data$date == max(.data$date)) %>%
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
    TRUE ~ "Victoria's unemployment rate compared to other states and territories"
  ) %>%
    paste0("Victoria's unemployment rate ", .)

  df %>%
    ggplot(aes(x = reorder(state, value), y = value, col = format(date, "%b %Y"))) +
    geom_segment(data = df_wide,
                 aes(x = reorder(state, max_date),
                     xend = reorder(state, max_date),
                     y = min_date,
                     yend = arrow_max
                     ),
                 colour = djprtheme::djpr_cool_grey_11,
                 arrow = arrow(length = unit(0.5, "lines"),
                               type = "closed",
                               angle = 25),
                 inherit.aes = F) +
    ggiraph::geom_point_interactive(size = 3,
                                    aes(tooltip = paste0(format(.data$date, "%b %Y"),
                                                         "\n",
                                                         round2(.data$value, 1))
                                        )) +
    ggrepel::geom_label_repel(data = ~dplyr::filter(., .data$state == "Victoria"),
               aes(label = format(.data$date, "%b %Y") ),
               direction = "y",
               label.padding = unit(0.1, "lines"),
               size = 14 / .pt,
               min.segment.length = unit(10000, "lines"),
               label.size = NA,
               nudge_x = 0.33) +
    theme_djpr(flipped = T) +
    coord_flip() +
    djpr_colour_manual(2) +
    labs(y = "Unemployment rate",
         subtitle = paste0("Unemployment rate in Australian states and territories in ",
                           unique(df$date) %>% format("%B %Y") %>% paste0(collapse = " and ")),
         title = title,
         caption = caption_lfs())

}
