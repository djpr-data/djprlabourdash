#' Function to create the graphs for the 'Industries' subpage on the dashboard.
#' @param data the dataframe containing data to visualise
#' @examples
#' \dontrun{
#'
#' dash_data <- load_dash_data()
#'
#' # for 'viz_industries_empchange_sincecovid_bar':
#' data <- filter_dash_data(c(
#'   "A84601680F",
#'   "A84601683L",
#'   "A84601686V",
#'   "A84601665J",
#'   "A84601704L",
#'   "A84601707V",
#'   "A84601710J",
#'   "A84601638A",
#'   "A84601653X",
#'   "A84601689A",
#'   "A84601656F",
#'   "A84601713R",
#'   "A84601668R",
#'   "A84601695W",
#'   "A84601698C",
#'   "A84601650T",
#'   "A84601671C",
#'   "A84601641R",
#'   "A84601716W",
#'   "A84601662A"
#' )) %>%
#'   dplyr::filter(date >= as.Date("2020-01-01"))
#' }
#' @import djprtheme

viz_industries_empchange_sincecovid_bar <- function(data = filter_dash_data(c(
                                                      "A84601680F",
                                                      "A84601683L",
                                                      "A84601686V",
                                                      "A84601665J",
                                                      "A84601704L",
                                                      "A84601707V",
                                                      "A84601710J",
                                                      "A84601638A",
                                                      "A84601653X",
                                                      "A84601689A",
                                                      "A84601656F",
                                                      "A84601713R",
                                                      "A84601668R",
                                                      "A84601695W",
                                                      "A84601698C",
                                                      "A84601650T",
                                                      "A84601671C",
                                                      "A84601641R",
                                                      "A84601716W",
                                                      "A84601662A"
                                                    ),
                                                    df = dash_data
                                                    )) {
  data <- data %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = 100 * ((.data$value / .data$value[date == as.Date("2020-02-01")]) - 1))

  # reduce to only latest month (indexing already done above in data input into function)                                {
  data <- data %>%
    dplyr::group_by(.data$series) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  # add entry for data$industry for Victoria; employed total
  data <- data %>%
    dplyr::mutate(
      industry = dplyr::if_else(.data$industry == "",
        "Victoria, all industries",
        .data$industry
      )
    )

  lab_df <- data %>%
    dplyr::select(.data$industry, .data$value) %>%
    dplyr::mutate(
      lab_y = dplyr::if_else(.data$value >= 0, .data$value + 0.1, .data$value - 0.75),
      lab_hjust = dplyr::if_else(.data$value >= 0, 0, 1)
    )

  title <- paste0(
    "Employment in ",
    data$industry[data$value == max(data$value)],
    dplyr::if_else(max(data$value) > 0, " grew", " shrank"),
    " by ",
    round2(max(data$value), 1),
    " per cent between February 2020 and ",
    format(max(data$date), "%B %Y"),
    ", while employment in ",
    data$industry[data$value == min(data$value)],
    dplyr::if_else(min(data$value) > 0, " grew", " shrank"),
    " by ",
    round2(abs(min(data$value)), 1),
    " per cent"
  )

  # draw bar chart for all 19 industries plus Vic total
  data %>%
    ggplot(aes(
      x = stats::reorder(.data$industry, .data$value),
      y = .data$value
    )) +
    geom_col(
      aes(fill = -.data$value)
    ) +
    geom_text(
      data = lab_df,
      aes(
        y = .data$lab_y,
        hjust = .data$lab_hjust,
        label = paste0(round2(.data$value, 1), "%")
      ),
      colour = "black",
      size = 11 / .pt
    ) +
    geom_hline(
      yintercept = 0
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.15))) +
    scale_fill_distiller(palette = "Blues") +
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
        "Growth in employment by industry between February 2020 and ",
        format(max(data$date), "%B %Y")
      ),
      caption = paste0(caption_lfs_det_q(), " Data not seasonally adjusted. ")
    )
}

table_industries_employment <- function(data = filter_dash_data(c(
                                          "A84601680F",
                                          "A84601683L",
                                          "A84601686V",
                                          "A84601665J",
                                          "A84601704L",
                                          "A84601707V",
                                          "A84601710J",
                                          "A84601638A",
                                          "A84601653X",
                                          "A84601689A",
                                          "A84601656F",
                                          "A84601713R",
                                          "A84601668R",
                                          "A84601695W",
                                          "A84601698C",
                                          "A84601650T",
                                          "A84601671C",
                                          "A84601641R",
                                          "A84601716W",
                                          "A84601662A",
                                          "A84601681J",
                                          "A84601684R",
                                          "A84601687W",
                                          "A84601666K",
                                          "A84601705R",
                                          "A84601708W",
                                          "A84601711K",
                                          "A84601639C",
                                          "A84601654A",
                                          "A84601690K",
                                          "A84601657J",
                                          "A84601714T",
                                          "A84601669T",
                                          "A84601696X",
                                          "A84601699F",
                                          "A84601651V",
                                          "A84601672F",
                                          "A84601642T",
                                          "A84601717X",
                                          "A84601663C",
                                          "A84601682K",
                                          "A84601685T",
                                          "A84601688X",
                                          "A84601667L",
                                          "A84601706T",
                                          "A84601709X",
                                          "A84601712L",
                                          "A84601640L",
                                          "A84601655C",
                                          "A84601691L",
                                          "A84601658K",
                                          "A84601715V",
                                          "A84601670A",
                                          "A84601697A",
                                          "A84601700C",
                                          "A84601652W",
                                          "A84601673J",
                                          "A84601643V",
                                          "A84601718A",
                                          "A84601664F"
                                        ),
                                        df = dash_data
                                        ),
                                        chosen_industry = "Agriculture, Forestry and Fishing") {
  latest_date <- format(max(data$date), "%b %Y")

  # add entry for data$industry for "Victoria, all industries" where ""
  df <- data %>%
    dplyr::mutate(
      industry = dplyr::if_else(.data$industry == "",
        "Victoria, all industries",
        .data$industry
      )
    )

  # filter out the chosen industry and vic_total
  df <- df %>%
    group_by(.data$indicator) %>%
    dplyr::filter(.data$industry %in%
      c("Victoria, all industries", .env$chosen_industry))

  table_df <- df %>%
    dplyr::group_by(.data$industry, .data$indicator) %>%
    dplyr::mutate(
      d_quarter = 100 * ((.data$value / dplyr::lag(.data$value, 1)) - 1),
      d_year = 100 * ((.data$value / dplyr::lag(.data$value, 4)) - 1)
    ) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(
      .data$indicator, .data$value, .data$industry,
      .data$d_quarter, .data$d_year
    ) %>%
    dplyr::ungroup()

  table_df <- table_df %>%
    dplyr::mutate(across(
      c(.data$value, .data$d_quarter, .data$d_year),
      ~ round2(.x, 1)
    ))

  table_df <- table_df %>%
    dplyr::mutate(across(
      c(.data$value, .data$d_quarter, .data$d_year),
      ~ round2(.x, 1)
    )) %>%
    dplyr::mutate(
      value = scales::comma(.data$value * 1000),
      d_quarter = paste0(.data$d_quarter, "%"),
      d_year = paste0(.data$d_year, "%")
    )

  table_df <- table_df %>%
    dplyr::rename({{ latest_date }} := .data$value,
      `Change over quarter` = .data$d_quarter,
      `Change over year` = .data$d_year
    )

  table_df <- table_df %>%
    tidyr::gather(
      key = "series", value = "value",
      -.data$indicator, -.data$industry
    ) %>%
    tidyr::spread(key = .data$industry, value = .data$value)

  table_df <- table_df %>%
    dplyr::mutate(indic_order = dplyr::case_when(
      indicator == "Employed total" ~ 1,
      indicator == "Employed full-time" ~ 2,
      indicator == "Employed part-time" ~ 3
    )) %>%
    mutate(series_order = dplyr::case_when(
      series == "Change over quarter" ~ 2,
      series == "Change over year" ~ 3,
      TRUE ~ 1
    )) %>%
    dplyr::arrange(.data$indic_order, .data$series_order) %>%
    dplyr::select(-ends_with("order"))

  # Fix to ensure that Victoria, all industries is always the last column
  table_df <- table_df %>%
    dplyr::select(.data$indicator, .data$series, .env$chosen_industry, .data$`Victoria, all industries`)

  out <- table_df %>%
    dplyr::group_by(.data$indicator) %>%
    dplyr::mutate(indicator = dplyr::if_else(
      dplyr::row_number() != 1,
      "",
      .data$indicator
    )) %>%
    dplyr::rename(
      ` ` = .data$indicator,
      `  ` = .data$series
    ) %>%
    flextable::flextable() %>%
    flextable::bold(part = "header") %>%
    flextable::border_remove() %>%
    flextable::border(
      part = "body",
      j = 2:4,
      i = 2:nrow(table_df),
      border.top = flextable::fp_border_default(color = "grey90", width = 0.25)
    ) %>%
    flextable::border(
      part = "body",
      i = c(1, 4, 7),
      border.top = flextable::fp_border_default()
    ) %>%
    flextable::border(
      part = "body",
      i = nrow(table_df),
      border.bottom = flextable::fp_border_default()
    ) %>%
    flextable::set_table_properties("autofit", width = 1) %>%
    flextable::font(part = "body", fontname = "Roboto") %>%
    flextable::font(part = "header", fontname = "Roboto") %>%
    flextable::fontsize(size = 9) %>%
    flextable::fontsize(size = 9, part = "header")

  table_caption <- caption_auto(
    data = data,
    notes = "Data not seasonally adjusted."
  )
  # Add caption
  out <- out %>%
    flextable::add_footer(` ` = table_caption) %>%
    flextable::merge_at(
      j = 1:flextable::ncol_keys(out),
      part = "footer"
    ) %>%
    flextable::italic(part = "footer") %>%
    flextable::font(fontname = "Roboto") %>%
    flextable::fontsize(
      size = 9 * 0.85,
      part = "footer"
    ) %>%
    flextable::color(
      part = "footer",
      color = "#343a40"
    ) %>%
    flextable::line_spacing(
      part = "footer",
      space = 0.8
    )

  out
}

viz_industries_emp_line <- function(data = filter_dash_data(c(
                                      "A84601680F",
                                      "A84601683L",
                                      "A84601686V",
                                      "A84601665J",
                                      "A84601704L",
                                      "A84601707V",
                                      "A84601710J",
                                      "A84601638A",
                                      "A84601653X",
                                      "A84601689A",
                                      "A84601656F",
                                      "A84601713R",
                                      "A84601668R",
                                      "A84601695W",
                                      "A84601698C",
                                      "A84601650T",
                                      "A84601671C",
                                      "A84601641R",
                                      "A84601716W",
                                      "A84601662A"
                                    ),
                                    df = dash_data
                                    ),
                                    chosen_industry = "Agriculture, Forestry and Fishing") {
  df <- data %>%
    dplyr::mutate(
      industry = dplyr::if_else(.data$industry == "",
        "Victoria, all industries",
        .data$industry
      )
    )

  df <- df %>%
    dplyr::filter(.data$industry %in% c("Victoria, all industries", .env$chosen_industry))

  df <- df %>%
    dplyr::group_by(.data$industry) %>%
    dplyr::mutate(
      value = 100 * ((.data$value / dplyr::lag(.data$value, 4)) - 1),
      tooltip = paste0(
        .data$industry, "\n",
        format(.data$date, "%b %Y"), "\n",
        round2(.data$value, 1), "%"
      )
    ) %>%
    dplyr::select(.data$date, .data$industry, .data$value, .data$tooltip) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$value))

  colours <- c(
    djprtheme::djpr_royal_blue,
    djprtheme::djpr_green
  )

  names(colours) <- c(
    "Victoria, all industries",
    chosen_industry
  )

  df %>%
    djpr_ts_linechart(
      col_var = .data$industry,
      label_num = paste0(round2(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    scale_colour_manual(values = colours) +
    labs(
      subtitle = "Annual change in total employment",
      caption = caption_lfs_det_q(),
      title = paste0(
        "Annual employment growth in ",
        chosen_industry,
        " compared to Victorian average"
      )
    )
}

viz_industries_emp_bysex_bar <- function(data = filter_dash_data(c(
                                           "females_greater melbourne_accommodation and food services_employed full-time",
                                           "females_greater melbourne_administrative and support services_employed full-time",
                                           "females_greater melbourne_agriculture, forestry and fishing_employed full-time",
                                           "females_greater melbourne_arts and recreation services_employed full-time",
                                           "females_greater melbourne_construction_employed full-time",
                                           "females_greater melbourne_education and training_employed full-time",
                                           "females_greater melbourne_electricity, gas, water and waste services_employed full-time",
                                           "females_greater melbourne_financial and insurance services_employed full-time",
                                           "females_greater melbourne_health care and social assistance_employed full-time",
                                           "females_greater melbourne_information media and telecommunications_employed full-time",
                                           "females_greater melbourne_manufacturing_employed full-time",
                                           "females_greater melbourne_mining_employed full-time",
                                           "females_greater melbourne_other services_employed full-time",
                                           "females_greater melbourne_professional, scientific and technical services_employed full-time",
                                           "females_greater melbourne_public administration and safety_employed full-time",
                                           "females_greater melbourne_rental, hiring and real estate services_employed full-time",
                                           "females_greater melbourne_retail trade_employed full-time",
                                           "females_greater melbourne_transport, postal and warehousing_employed full-time",
                                           "females_greater melbourne_wholesale trade_employed full-time",
                                           "males_greater melbourne_accommodation and food services_employed full-time",
                                           "males_greater melbourne_administrative and support services_employed full-time",
                                           "males_greater melbourne_agriculture, forestry and fishing_employed full-time",
                                           "males_greater melbourne_arts and recreation services_employed full-time",
                                           "males_greater melbourne_construction_employed full-time",
                                           "males_greater melbourne_education and training_employed full-time",
                                           "males_greater melbourne_electricity, gas, water and waste services_employed full-time",
                                           "males_greater melbourne_financial and insurance services_employed full-time",
                                           "males_greater melbourne_health care and social assistance_employed full-time",
                                           "males_greater melbourne_information media and telecommunications_employed full-time",
                                           "males_greater melbourne_manufacturing_employed full-time",
                                           "males_greater melbourne_mining_employed full-time",
                                           "males_greater melbourne_other services_employed full-time",
                                           "males_greater melbourne_professional, scientific and technical services_employed full-time",
                                           "males_greater melbourne_public administration and safety_employed full-time",
                                           "males_greater melbourne_rental, hiring and real estate services_employed full-time",
                                           "males_greater melbourne_retail trade_employed full-time",
                                           "males_greater melbourne_transport, postal and warehousing_employed full-time",
                                           "males_greater melbourne_wholesale trade_employed full-time",
                                           "females_rest of vic._accommodation and food services_employed full-time",
                                           "females_rest of vic._administrative and support services_employed full-time",
                                           "females_rest of vic._agriculture, forestry and fishing_employed full-time",
                                           "females_rest of vic._arts and recreation services_employed full-time",
                                           "females_rest of vic._construction_employed full-time",
                                           "females_rest of vic._education and training_employed full-time",
                                           "females_rest of vic._electricity, gas, water and waste services_employed full-time",
                                           "females_rest of vic._financial and insurance services_employed full-time",
                                           "females_rest of vic._health care and social assistance_employed full-time",
                                           "females_rest of vic._information media and telecommunications_employed full-time",
                                           "females_rest of vic._manufacturing_employed full-time",
                                           "females_rest of vic._mining_employed full-time",
                                           "females_rest of vic._other services_employed full-time",
                                           "females_rest of vic._professional, scientific and technical services_employed full-time",
                                           "females_rest of vic._public administration and safety_employed full-time",
                                           "females_rest of vic._rental, hiring and real estate services_employed full-time",
                                           "females_rest of vic._retail trade_employed full-time",
                                           "females_rest of vic._transport, postal and warehousing_employed full-time",
                                           "females_rest of vic._wholesale trade_employed full-time",
                                           "males_rest of vic._accommodation and food services_employed full-time",
                                           "males_rest of vic._administrative and support services_employed full-time",
                                           "males_rest of vic._agriculture, forestry and fishing_employed full-time",
                                           "males_rest of vic._arts and recreation services_employed full-time",
                                           "males_rest of vic._construction_employed full-time",
                                           "males_rest of vic._education and training_employed full-time",
                                           "males_rest of vic._electricity, gas, water and waste services_employed full-time",
                                           "males_rest of vic._financial and insurance services_employed full-time",
                                           "males_rest of vic._health care and social assistance_employed full-time",
                                           "males_rest of vic._information media and telecommunications_employed full-time",
                                           "males_rest of vic._manufacturing_employed full-time",
                                           "males_rest of vic._mining_employed full-time",
                                           "males_rest of vic._other services_employed full-time",
                                           "males_rest of vic._professional, scientific and technical services_employed full-time",
                                           "males_rest of vic._public administration and safety_employed full-time",
                                           "males_rest of vic._rental, hiring and real estate services_employed full-time",
                                           "males_rest of vic._retail trade_employed full-time",
                                           "males_rest of vic._transport, postal and warehousing_employed full-time",
                                           "males_rest of vic._wholesale trade_employed full-time",
                                           "females_greater melbourne_accommodation and food services_employed part-time",
                                           "females_greater melbourne_administrative and support services_employed part-time",
                                           "females_greater melbourne_agriculture, forestry and fishing_employed part-time",
                                           "females_greater melbourne_arts and recreation services_employed part-time",
                                           "females_greater melbourne_construction_employed part-time",
                                           "females_greater melbourne_education and training_employed part-time",
                                           "females_greater melbourne_electricity, gas, water and waste services_employed part-time",
                                           "females_greater melbourne_financial and insurance services_employed part-time",
                                           "females_greater melbourne_health care and social assistance_employed part-time",
                                           "females_greater melbourne_information media and telecommunications_employed part-time",
                                           "females_greater melbourne_manufacturing_employed part-time",
                                           "females_greater melbourne_mining_employed part-time",
                                           "females_greater melbourne_other services_employed part-time",
                                           "females_greater melbourne_professional, scientific and technical services_employed part-time",
                                           "females_greater melbourne_public administration and safety_employed part-time",
                                           "females_greater melbourne_rental, hiring and real estate services_employed part-time",
                                           "females_greater melbourne_retail trade_employed part-time",
                                           "females_greater melbourne_transport, postal and warehousing_employed part-time",
                                           "females_greater melbourne_wholesale trade_employed part-time",
                                           "males_greater melbourne_accommodation and food services_employed part-time",
                                           "males_greater melbourne_administrative and support services_employed part-time",
                                           "males_greater melbourne_agriculture, forestry and fishing_employed part-time",
                                           "males_greater melbourne_arts and recreation services_employed part-time",
                                           "males_greater melbourne_construction_employed part-time",
                                           "males_greater melbourne_education and training_employed part-time",
                                           "males_greater melbourne_electricity, gas, water and waste services_employed part-time",
                                           "males_greater melbourne_financial and insurance services_employed part-time",
                                           "males_greater melbourne_health care and social assistance_employed part-time",
                                           "males_greater melbourne_information media and telecommunications_employed part-time",
                                           "males_greater melbourne_manufacturing_employed part-time",
                                           "males_greater melbourne_mining_employed part-time",
                                           "males_greater melbourne_other services_employed part-time",
                                           "males_greater melbourne_professional, scientific and technical services_employed part-time",
                                           "males_greater melbourne_public administration and safety_employed part-time",
                                           "males_greater melbourne_rental, hiring and real estate services_employed part-time",
                                           "males_greater melbourne_retail trade_employed part-time",
                                           "males_greater melbourne_transport, postal and warehousing_employed part-time",
                                           "males_greater melbourne_wholesale trade_employed part-time",
                                           "females_rest of vic._accommodation and food services_employed part-time",
                                           "females_rest of vic._administrative and support services_employed part-time",
                                           "females_rest of vic._agriculture, forestry and fishing_employed part-time",
                                           "females_rest of vic._arts and recreation services_employed part-time",
                                           "females_rest of vic._construction_employed part-time",
                                           "females_rest of vic._education and training_employed part-time",
                                           "females_rest of vic._electricity, gas, water and waste services_employed part-time",
                                           "females_rest of vic._financial and insurance services_employed part-time",
                                           "females_rest of vic._health care and social assistance_employed part-time",
                                           "females_rest of vic._information media and telecommunications_employed part-time",
                                           "females_rest of vic._manufacturing_employed part-time",
                                           "females_rest of vic._mining_employed part-time",
                                           "females_rest of vic._other services_employed part-time",
                                           "females_rest of vic._professional, scientific and technical services_employed part-time",
                                           "females_rest of vic._public administration and safety_employed part-time",
                                           "females_rest of vic._rental, hiring and real estate services_employed part-time",
                                           "females_rest of vic._retail trade_employed part-time",
                                           "females_rest of vic._transport, postal and warehousing_employed part-time",
                                           "females_rest of vic._wholesale trade_employed part-time",
                                           "males_rest of vic._accommodation and food services_employed part-time",
                                           "males_rest of vic._administrative and support services_employed part-time",
                                           "males_rest of vic._agriculture, forestry and fishing_employed part-time",
                                           "males_rest of vic._arts and recreation services_employed part-time",
                                           "males_rest of vic._construction_employed part-time",
                                           "males_rest of vic._education and training_employed part-time",
                                           "males_rest of vic._electricity, gas, water and waste services_employed part-time",
                                           "males_rest of vic._financial and insurance services_employed part-time",
                                           "males_rest of vic._health care and social assistance_employed part-time",
                                           "males_rest of vic._information media and telecommunications_employed part-time",
                                           "males_rest of vic._manufacturing_employed part-time",
                                           "males_rest of vic._mining_employed part-time",
                                           "males_rest of vic._other services_employed part-time",
                                           "males_rest of vic._professional, scientific and technical services_employed part-time",
                                           "males_rest of vic._public administration and safety_employed part-time",
                                           "males_rest of vic._rental, hiring and real estate services_employed part-time",
                                           "males_rest of vic._retail trade_employed part-time",
                                           "males_rest of vic._transport, postal and warehousing_employed part-time",
                                           "males_rest of vic._wholesale trade_employed part-time",
                                           "A84423461V",
                                           "A84423237A"
                                         ), df = dash_data) %>%
                                           dplyr::group_by(.data$series) %>%
                                           dplyr::filter(.data$date == max(.data$date)) %>%
                                           dplyr::ungroup(),
                                         chosen_industry = "Agriculture, Forestry and Fishing") {
  df <- data %>%
    dplyr::mutate(
      industry = dplyr::if_else(.data$industry == "",
        "Victoria, all industries",
        .data$industry
      )
    )


  df <- df %>%
    dplyr::filter(.data$industry %in% c("Victoria, all industries", .env$chosen_industry)) %>%
    dplyr::select(
      .data$date, .data$value, .data$series,
      .data$indicator, .data$sex, .data$industry, .data$gcc_restofstate
    )

  df <- df %>%
    dplyr::group_by(.data$sex, .data$industry) %>%
    dplyr::summarise(
      value = sum(.data$value),
      date = max(.data$date)
    ) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::group_by(.data$industry) %>%
    dplyr::mutate(perc = .data$value / sum(.data$value)) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::mutate(order = if_else(.data$industry == "Victoria, all industries", 2, 1))

  label_df <- df %>%
    dplyr::group_by(.data$industry) %>%
    dplyr::arrange(desc(.data$sex)) %>%
    dplyr::mutate(label_y = cumsum(.data$perc) - .data$perc + (.data$perc / 2))

  legend_df <- label_df %>%
    dplyr::filter(.data$industry != "Victoria, all industries")

  chosen_industry_female_share <- round2(df$perc[df$sex == "Females" & df$industry != "Victoria, all industries"] * 100, 1)
  vic_female_share <- round2(df$perc[df$sex == "Females" & df$industry == "Victoria, all industries"] * 100, 1)

  title <- paste0(
    "Women account for ",
    chosen_industry_female_share,
    " per cent of workers in the ",
    chosen_industry,
    " industry, which is ",
    dplyr::case_when(
      chosen_industry_female_share > vic_female_share ~
      "higher than",
      chosen_industry_female_share < vic_female_share ~
      "lower than",
      chosen_industry_female_share == vic_female_share ~
      "the same as"
    ),
    " the Victorian average"
  )

  df %>%
    ggplot(aes(
      x = stats::reorder(stringr::str_wrap(.data$industry, 15), -.data$order),
      y = .data$value, fill = .data$sex
    )) +
    geom_col(
      position = "fill",
      alpha = 1,
      col = "grey70"
    ) +
    geom_text(
      data = label_df,
      aes(y = .data$label_y, label = paste0(round2(.data$perc * 100, 1), "%")),
      size = 16 / .pt,
      colour = "white"
    ) +
    geom_text(
      data = legend_df,
      aes(y = .data$label_y, label = .data$sex, col = .data$sex),
      nudge_x = 0.55,
      size = 14 / .pt
    ) +
    coord_flip() +
    theme_djpr() +
    djpr_fill_manual(2) +
    djpr_colour_manual(2) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank()
    ) +
    labs(
      subtitle = paste0(
        "Percentage share of men and women employed in industries in ",
        format(max(df$date), "%B %Y"), "."
      ),
      caption = paste0(caption_lfs_det_q(), " Data not seasonally adjusted."),
      title = title
    )
}
