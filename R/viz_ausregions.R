#' @importFrom rlang `:=`

viz_reg_emp_regionstates_sincecovid_line <- function(data = filter_dash_data(c(
                                                       "A84600075R",
                                                       "A84599625R",
                                                       "A84599781T",
                                                       "A84599607K",
                                                       "A84600243R",
                                                       "A84599715V",
                                                       "A84599631K"
                                                     ),
                                                     df = dash_data
                                                     ) %>%
                                                       dplyr::mutate(
                                                         state = dplyr::case_when(
                                                           .data$series == ">> Rest of Vic. ;  Employed total ;  Persons ;" ~
                                                             "Reg. Vic",
                                                           .data$series == ">> Rest of NSW ;  Employed total ;  Persons ;" ~
                                                             "Reg. NSW",
                                                           .data$series == ">> Rest of Qld ;  Employed total ;  Persons ;" ~
                                                             "Reg. QLD",
                                                           .data$series == ">>> Northern Territory - Outback ;  Employed total ;  Persons ;" ~
                                                             "Reg. NT",
                                                           .data$series == ">> Rest of WA ;  Employed total ;  Persons ;" ~
                                                             "Reg. WA",
                                                           .data$series == ">> Rest of SA ;  Employed total ;  Persons ;" ~
                                                             "Reg. SA",
                                                           .data$series == ">> Rest of Tas. ;  Employed total ;  Persons ;" ~
                                                             "Reg. Tas",
                                                           TRUE ~ .data$state
                                                         )
                                                       )) {
  df <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(
      value = slider::slide_mean(.data$value, before = 2, complete = TRUE)
    ) %>%
    dplyr::filter(date >= as.Date("2020-01-01"))

  df <- df %>%
    dplyr::mutate(
      state_group = dplyr::if_else(
        .data$state %in% c(
          "Reg. Vic", "Reg. NSW"
        ),
        .data$state,
        "Other"
      )
    )

  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::mutate(
      value = 100 * (
        (.data$value / .data$value[.data$date == as.Date("2020-03-01")]) - 1),
      tooltip = paste0(
        .data$state, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    )

  latest <- df %>%
    dplyr::select(.data$date, .data$state, .data$value) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    ungroup() %>%
    dplyr::mutate(rank = dplyr::min_rank(-.data$value))

  vic_level_raw <- round2(latest$value[latest$state == "Reg. Vic"], 1)
  vic_rank <- latest$rank[latest$state == "Reg. Vic"]
  vic_level <- paste0(vic_level_raw, "%")
  latest_date_pretty <- format(max(df$date), "%B %Y")
  covid_date_pretty <- format(as.Date("2020-03-01"), "%B %Y")

  title_part_1 <- dplyr::case_when(
    sign(vic_level_raw) == 1 ~ paste0(
      "rose by ", vic_level,
      " between ", covid_date_pretty,
      " and ", latest_date_pretty
    ),
    sign(vic_level_raw) == -1 ~ paste0(
      "fell by ", vic_level,
      " between ", covid_date_pretty,
      " and ", latest_date_pretty
    ),
    sign(vic_level_raw) == 0 ~ paste0(
      "was the same in",
      latest_date_pretty,
      "as it was in ",
      covid_date_pretty
    )
  )

  title_part_2 <- dplyr::case_when(
    vic_rank == 1 ~ "",
    vic_rank == 2 ~ "second",
    vic_rank == 3 ~ "third",
    vic_rank == 4 ~ "fourth",
    vic_rank == 5 ~ "fifth",
    vic_rank == 6 ~ "sixth",
    vic_rank == 7 ~ "seventh",
    vic_rank == 8 ~ "eighth"
  )

  title <- paste0(
    "Employment in regional Victoria ",
    title_part_1,
    " and has risen the ",
    title_part_2,
    " fastest of any Australian regional area"
  )

  other_colour <- djprtheme::djpr_cool_grey_11

  df %>%
    djpr_ts_linechart(
      col_var = .data$state,
      label_num = paste0(round2(.data$value, 1), "%"),
      hline = 0
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(5),
      labels = function(x) paste0(x, "%")
    ) +
    scale_colour_manual(values = c(
      "Reg. Vic" = djprtheme::djpr_blue,
      "Reg. NSW" = djprtheme::djpr_green,
      "Reg. NT" = other_colour,
      "Reg. Tas" = other_colour,
      "Reg. SA" = other_colour,
      "Reg. QLD" = other_colour,
      "Reg. WA" = other_colour
    )) +
    labs(
      title = title,
      subtitle = "Cumulative change in employment for regional states and territories of Australia since March 2020",
      caption = paste0(caption_lfs_det_m(), "Data smoothed using a 3 month rolling average.")
    )
}

viz_reg_regionstates_dot <- function(data = filter_dash_data(c(
                                       "A84599628W",
                                       "A84599629X",
                                       "A84599630J",
                                       "A84600078W",
                                       "A84600079X",
                                       "A84600080J",
                                       "A84599784X",
                                       "A84599785A",
                                       "A84599786C",
                                       "A84599718A",
                                       "A84599719C",
                                       "A84599720L",
                                       "A84600246W",
                                       "A84600247X",
                                       "A84600248A",
                                       "A84599634T",
                                       "A84599635V",
                                       "A84599636W",
                                       "A84599610X",
                                       "A84599611A",
                                       "A84599612C"
                                     ),
                                     df = dash_data
                                     ),
                                     selected_indicator = "unemp_rate") {
  df <- data %>%
    dplyr::select(.data$date, .data$value, .data$series, .data$indicator) %>%
    dplyr::mutate(indicator_short = dplyr::case_when(
      .data$indicator == "Unemployment rate" ~ "unemp_rate",
      .data$indicator == "Participation rate" ~ "part_rate",
      .data$indicator == "Employment to population ratio" ~ "emp_pop"
    ))

  df <- df %>%
    dplyr::filter(.data$indicator_short == selected_indicator)

  df <- df %>%
    dplyr::mutate(
      series = gsub(";.*", "", .data$series),
      series = gsub(">> Rest of ", "Regional ", .data$series),
      series = dplyr::if_else(grepl("Northern Territory", .data$series, fixed = TRUE),
        "Regional NT",
        .data$series
      ),
      series = stringr::str_trim(.data$series)
    )

  # 3 month average
  df <- df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(
      value = slider::slide_mean(.data$value, before = 2, complete = TRUE)
    ) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::ungroup()

  # select most current and one year prior
  df <- df %>%
    dplyr::filter(.data$date %in% c(
      max(.data$date),
      subtract_years(max(.data$date), 1)
    ))

  # create ranking
  df <- df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::mutate(rank = dplyr::dense_rank(-.data$value)) %>%
    dplyr::select(.data$rank, .data$series) %>%
    dplyr::right_join(df, by = "series")

  # create min_date and max_date
  df_wide <- df %>%
    dplyr::mutate(data_type = dplyr::if_else(.data$date == min(.data$date),
      "min_date",
      "max_date"
    )) %>%
    dplyr::select(.data$data_type, .data$value, .data$series, .data$rank) %>%
    tidyr::spread(key = .data$data_type, value = .data$value) %>%
    dplyr::mutate(arrow_end = dplyr::if_else(.data$max_date > .data$min_date,
      .data$max_date - 0.08,
      .data$max_date + 0.08
    ))

  latest_values <- df %>%
    dplyr::filter(
      .data$date == max(.data$date),
      .data$series == "Regional Vic."
    ) %>%
    dplyr::select(.data$series, .data$value, .data$date) %>%
    tidyr::pivot_wider(names_from = .data$series, values_from = .data$value)

  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "Unemployment rate",
    selected_indicator == "part_rate" ~ "Participation rate",
    selected_indicator == "emp_pop" ~ "Employment to population ratio",
    TRUE ~ NA_character_
  )

  title <- paste0(
    "The ", tolower(indic_long),
    " in regional Victoria was ",
    round2(latest_values$`Regional Vic.`, 1),
    " per cent in ",
    format(latest_values$date, "%B %Y")
  )

  df %>%
    ggplot(aes(
      x = stats::reorder(.data$series, .data$rank),
      y = .data$value,
      col = factor(.data$date)
    )) +
    geom_segment(
      data = df_wide,
      aes(
        x = stats::reorder(.data$series, .data$rank),
        xend = stats::reorder(.data$series, .data$rank),
        y = .data$min_date,
        yend = .data$arrow_end
      ),
      arrow = arrow(
        angle = 25,
        length = unit(0.5, "line"),
        type = "closed"
      ),
      inherit.aes = FALSE
    ) +
    ggiraph::geom_point_interactive(
      size = 4,
      aes(tooltip = paste0(
        .data$series,
        "\n",
        format(.data$date, "%B %Y"),
        "\n",
        round2(.data$value, 1), "%"
      ))
    ) +
    ggrepel::geom_text_repel(
      data = df %>%
        dplyr::filter(.data$series == "Regional Vic."),
      aes(label = format(.data$date, "%b %Y")),
      size = 14 / .pt,
      direction = "x",
      force = 10,
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
      subtitle = paste0(indic_long, " in regional areas of Australia"),
      caption = paste0(caption_lfs_det_m(), "Data smoothed using a 3 month rolling average."),
      y = paste0("", indic_long)
    )
}

viz_reg_regionstates_bar <- function(data = filter_dash_data(c(
                                       "15-24_employed_rest of nsw",
                                       "15-24_employed_rest of nt",
                                       "15-24_employed_rest of qld",
                                       "15-24_employed_rest of sa",
                                       "15-24_employed_rest of tas.",
                                       "15-24_employed_rest of vic.",
                                       "15-24_employed_rest of wa",
                                       "15-24_nilf_rest of nsw",
                                       "15-24_nilf_rest of nt",
                                       "15-24_nilf_rest of qld",
                                       "15-24_nilf_rest of sa",
                                       "15-24_nilf_rest of tas.",
                                       "15-24_nilf_rest of vic.",
                                       "15-24_nilf_rest of wa",
                                       "15-24_unemployed_rest of nsw",
                                       "15-24_unemployed_rest of nt",
                                       "15-24_unemployed_rest of qld",
                                       "15-24_unemployed_rest of sa",
                                       "15-24_unemployed_rest of tas.",
                                       "15-24_unemployed_rest of vic.",
                                       "15-24_unemployed_rest of wa",
                                       "25-54_employed_rest of nsw",
                                       "25-54_employed_rest of nt",
                                       "25-54_employed_rest of qld",
                                       "25-54_employed_rest of sa",
                                       "25-54_employed_rest of tas.",
                                       "25-54_employed_rest of vic.",
                                       "25-54_employed_rest of wa",
                                       "25-54_nilf_rest of nsw",
                                       "25-54_nilf_rest of nt",
                                       "25-54_nilf_rest of qld",
                                       "25-54_nilf_rest of sa",
                                       "25-54_nilf_rest of tas.",
                                       "25-54_nilf_rest of vic.",
                                       "25-54_nilf_rest of wa",
                                       "25-54_unemployed_rest of nsw",
                                       "25-54_unemployed_rest of nt",
                                       "25-54_unemployed_rest of qld",
                                       "25-54_unemployed_rest of sa",
                                       "25-54_unemployed_rest of tas.",
                                       "25-54_unemployed_rest of vic.",
                                       "25-54_unemployed_rest of wa",
                                       "55+_employed_rest of nsw",
                                       "55+_employed_rest of nt",
                                       "55+_employed_rest of qld",
                                       "55+_employed_rest of sa",
                                       "55+_employed_rest of tas.",
                                       "55+_employed_rest of vic.",
                                       "55+_employed_rest of wa",
                                       "55+_nilf_rest of nsw",
                                       "55+_nilf_rest of nt",
                                       "55+_nilf_rest of qld",
                                       "55+_nilf_rest of sa",
                                       "55+_nilf_rest of tas.",
                                       "55+_nilf_rest of vic.",
                                       "55+_nilf_rest of wa",
                                       "55+_unemployed_rest of nsw",
                                       "55+_unemployed_rest of nt",
                                       "55+_unemployed_rest of qld",
                                       "55+_unemployed_rest of sa",
                                       "55+_unemployed_rest of tas.",
                                       "55+_unemployed_rest of vic.",
                                       "55+_unemployed_rest of wa"
                                     ),
                                     df = dash_data
                                     ),
                                     selected_indicator = "unemp_rate") {
  df <- data %>%
    dplyr::select(.data$date, .data$series, .data$value)

  df <- df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value, before = 11, complete = TRUE)) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::filter(.data$date == max(.data$date))

  # Note that this is substantially faster than tidyr::separate()
  split_series <- stringr::str_split_fixed(df$series, stringr::fixed(" ; "), 3)

  df <- df %>%
    dplyr::mutate(
      age = split_series[, 1],
      indic = split_series[, 2],
      geog = split_series[, 3]
    ) %>%
    dplyr::select(-.data$series)

  # calculate participation, unemployment rate and employment to pop ratio for each regional area
  df <- df %>%
    tidyr::pivot_wider(
      names_from = .data$indic,
      values_from = .data$value
    )

  # Calculate national totals for regional areas
  df <- df %>%
    dplyr::group_by(.data$date, .data$age) %>%
    dplyr::summarise(
      Employed = sum(.data$Employed),
      NILF = sum(.data$NILF),
      Unemployed = sum(.data$Unemployed)
    ) %>%
    dplyr::mutate(geog = "Rest of Aus.") %>%
    dplyr::bind_rows(df)

  df <- df %>%
    dplyr::mutate(
      part_rate = 100 * ((.data$Employed + .data$Unemployed) / (.data$Employed + .data$Unemployed + .data$NILF)),
      unemp_rate = 100 * (.data$Unemployed / (.data$Employed + .data$Unemployed)),
      emp_pop = 100 * (.data$Employed / (.data$Employed + .data$Unemployed + .data$NILF))
    )

  # depending on selected_indicator, choose measure to be calculated
  df <- df %>%
    dplyr::select(.data$date, .data$age, .data$geog, "value" = .env$selected_indicator)

  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "Unemployment rate",
    selected_indicator == "part_rate" ~ "Participation rate",
    selected_indicator == "emp_pop" ~ "Employment to population ratio",
    TRUE ~ NA_character_
  )

  max_date <- max(data$date) %>%
    format("%B %Y")

  subtitle <- paste0(
    indic_long,
    " in regional areas by age, ", max_date
  )


  df <- df %>%
    dplyr::mutate(state_group = dplyr::if_else(
      .data$geog %in% c("Rest of Vic.", "Rest of Aus."), .data$geog, "Other"
    ))

  df <- df %>%
    dplyr::mutate(
      geog = gsub("Rest of ", "Regional ", .data$geog, fixed = TRUE)
    )

  title_df <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$geog %in% c("Regional Aus.", "Regional Vic.")) %>%
    dplyr::group_by(.data$age) %>%
    dplyr::mutate(rank = rank(.data$value))

  title <- dplyr::case_when(
    all(title_df$rank[title_df$geog == "Regional Vic."] == 1) ~
      paste0(
        "Regional Victoria had a lower ",
        tolower(indic_long),
        " than regional Australia across all age groups in ",
        max_date
      ),
    title_df$rank[title_df$geog == "Regional Vic." & title_df$age == "15-24"] == 1 ~
      paste0(
        "Regional Victoria had a lower ",
        tolower(indic_long),
        " for young people than regional Australia in ",
        max_date
      ),
    title_df$rank[title_df$geog == "Regional Vic." & title_df$age == "15-24"] == 2 ~
      paste0(
        "Regional Victoria had a higher ",
        tolower(indic_long),
        " for young people than regional Australia in ",
        max_date
      ),
    TRUE ~
      paste0(
        "Regional ", indic_long, " by age by State and Territory, ",
        max_date
      )
  )

  max_value <- max(df$value)

  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$geog, "\n",
      round2(.data$value, 1), "%"
    ))

  make_age_patch <- function(age, upper_limit = max_value) {
    df %>%
      dplyr::filter(.data$age == .env$age) %>%
      ggplot(aes(
        x = stats::reorder(.data$geog, .data$value),
        y = .data$value,
        fill = .data$state_group
      )) +
      ggiraph::geom_col_interactive(aes(tooltip = .data$tooltip)) +
      coord_flip() +
      theme_djpr(flipped = TRUE) +
      djpr_y_continuous(
        limits = c(0, upper_limit),
        breaks = scales::breaks_pretty(5),
        labels = function(x) paste0(x, "%")
      ) +
      scale_fill_manual(
        values = c(
          "Rest of Vic." = djprtheme::djpr_blue,
          "Rest of Aus." = djprtheme::djpr_green,
          "Other" = "grey75"
        )
      ) +
      labs(subtitle = paste0("Age ", age)) +
      theme(
        plot.subtitle = element_text(
          hjust = 0.5,
          colour = "black",
          size = 14
        ),
        axis.title.x = element_blank(),
        axis.text.x = if (age == "55+") {
          element_text()
        } else {
          element_blank()
        }
      )
  }

  patches <- lapply(
    c(
      "15-24",
      "25-54",
      "55+"
    ),
    make_age_patch
  )


  patchwork::wrap_plots(
    patches,
    ncol = 1
  ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = paste0(caption_lfs_det_m(), " Data is smoothed using a 12 month rolling average."),
      theme = theme_djpr()
    )
}
