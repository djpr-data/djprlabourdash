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

industries_employment_treemap <- function(data = filter_dash_data(c(
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
df = dash_data)){
  df <- data %>%
    dplyr::mutate(
      industry = dplyr::if_else(.data$industry == "",
                                "Victoria, all industries",
                                .data$industry
      )) %>%
    dplyr::filter(.data$industry != "Victoria, all industries") %>%
    dplyr::filter(.data$indicator == "Employed total") %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::filter(!is.na(.data$industry)) %>%
    dplyr::group_by(.data$industry) %>%
    dplyr::summarise(
      value = sum(value),
      date = max(date)
    ) %>%
    dplyr::mutate(perc = value / sum(value)) %>%
    dplyr::ungroup()


  max_ind <- df %>%
    filter(perc == max(perc)) %>%
    select(industry)


  max_ind_share <- df %>%
    filter(perc == max(perc)) %>%
    select(perc)


  title <- paste0(
    "As at ",
    format(max(df$date), "%B %Y"),
    ", ",
    max_ind,
    " employes the most people, accounts for ",
    round2(max_ind_share*100,1),
    " percent of the total employment in Victoria"
  )

  ind_cols <- grDevices::colorRampPalette(suppressWarnings(djpr_pal(4)))(19)

  # ind_cols <- grDevices::colorRampPalette(c(djprtheme::djpr_green,
  #                                          "white",
  #                                          djprtheme::djpr_royal_blue))(19)



  df %>% ggplot(aes(fill = industry, area = value, label = industry)) +
    treemapify:: geom_treemap() +
    treemapify:: geom_treemap_text(data = df, colour = "white", place = "centre", aes(label = paste0(industry))) +
    treemapify:: geom_treemap_text(data = df, colour = "white", place = "bottom", aes(label = paste0(round2(perc*100, 1), "%"))) +
    scale_fill_manual(values = ind_cols) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank()
    ) +
    theme_djpr() +
    labs(
      title = title,
      subtitle = paste0(
        "Victoria's employment composition ",
        format(max(df$date), "%B %Y")
      ),
      caption = caption_lfs_det_q()
    )

}

viz_industries_hourchange_sincecovid_bar <- function(data = filter_dash_data(c("females_greater melbourne_accommodation and food services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_administrative and support services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_agriculture, forestry and fishing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_arts and recreation services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_construction_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_education and training_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_electricity, gas, water and waste services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_financial and insurance services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_health care and social assistance_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_information media and telecommunications_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_manufacturing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_mining_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_other services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_professional, scientific and technical services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_public administration and safety_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_rental, hiring and real estate services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_retail trade_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_transport, postal and warehousing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_wholesale trade_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_accommodation and food services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_administrative and support services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_agriculture, forestry and fishing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_arts and recreation services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_construction_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_education and training_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_electricity, gas, water and waste services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_financial and insurance services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_health care and social assistance_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_information media and telecommunications_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_manufacturing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_mining_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_other services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_professional, scientific and technical services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_public administration and safety_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_rental, hiring and real estate services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_retail trade_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_transport, postal and warehousing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_greater melbourne_wholesale trade_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._accommodation and food services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._administrative and support services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._agriculture, forestry and fishing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._arts and recreation services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._construction_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._education and training_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._electricity, gas, water and waste services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._financial and insurance services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._health care and social assistance_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._information media and telecommunications_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._manufacturing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._mining_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._other services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._professional, scientific and technical services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._public administration and safety_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._rental, hiring and real estate services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._retail trade_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._transport, postal and warehousing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_rest of vic._wholesale trade_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._accommodation and food services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._administrative and support services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._agriculture, forestry and fishing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._arts and recreation services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._construction_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._education and training_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._electricity, gas, water and waste services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._financial and insurance services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._health care and social assistance_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._information media and telecommunications_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._manufacturing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._mining_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._other services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._professional, scientific and technical services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._public administration and safety_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._rental, hiring and real estate services_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._retail trade_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._transport, postal and warehousing_number of hours actually worked in all jobs (employed full-time)",
                                                                               "males_rest of vic._wholesale trade_number of hours actually worked in all jobs (employed full-time)",
                                                                               "females_greater melbourne_accommodation and food services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_administrative and support services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_agriculture, forestry and fishing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_arts and recreation services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_construction_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_education and training_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_electricity, gas, water and waste services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_financial and insurance services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_health care and social assistance_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_information media and telecommunications_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_manufacturing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_mining_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_other services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_professional, scientific and technical services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_public administration and safety_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_rental, hiring and real estate services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_retail trade_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_transport, postal and warehousing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_greater melbourne_wholesale trade_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_accommodation and food services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_administrative and support services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_agriculture, forestry and fishing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_arts and recreation services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_construction_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_education and training_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_electricity, gas, water and waste services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_financial and insurance services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_health care and social assistance_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_information media and telecommunications_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_manufacturing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_mining_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_other services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_professional, scientific and technical services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_public administration and safety_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_rental, hiring and real estate services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_retail trade_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_transport, postal and warehousing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_greater melbourne_wholesale trade_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._accommodation and food services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._administrative and support services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._agriculture, forestry and fishing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._arts and recreation services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._construction_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._education and training_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._electricity, gas, water and waste services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._financial and insurance services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._health care and social assistance_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._information media and telecommunications_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._manufacturing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._mining_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._other services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._professional, scientific and technical services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._public administration and safety_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._rental, hiring and real estate services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._retail trade_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._transport, postal and warehousing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "females_rest of vic._wholesale trade_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._accommodation and food services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._administrative and support services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._agriculture, forestry and fishing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._arts and recreation services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._construction_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._education and training_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._electricity, gas, water and waste services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._financial and insurance services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._health care and social assistance_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._information media and telecommunications_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._manufacturing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._mining_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._other services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._professional, scientific and technical services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._public administration and safety_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._rental, hiring and real estate services_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._retail trade_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._transport, postal and warehousing_number of hours actually worked in all jobs (employed part-time)",
                                                                               "males_rest of vic._wholesale trade_number of hours actually worked in all jobs (employed part-time)"
                                                                               ),
df = dash_data
)) {
  df <- data %>%
  dplyr::group_by(.data$industry,.data$date) %>%
  dplyr::summarise(hour = sum(value)) %>%
  dplyr::mutate(change = hour - lag(hour),
                growth = 100 * ((hour / lag(hour) - 1))) %>%
  dplyr::filter(date == max(.data$date))

  df_total <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr:: summarise(hour = sum(value)) %>%
    dplyr::mutate(total_lag = lag(hour))

  df_all <- df %>%
    merge(df_total, by = "date") %>%
  dplyr::mutate(ind_contri = change/total_lag*100) %>%
    select(industry, date, ind_contri)%>%
    arrange(ind_contri)

  title <- paste0(
    "Total hours worked across industries",
    dplyr::if_else(sum(df_all$ind_contri) > 0, " grew", " shrank"),
    " by ",
    round2(sum(df_all$ind_contri), 1),
    " per cent between February 2020 and ",
    format(df_all$date, "%B %Y"),
    ", with ",
    df_all$industry[df_all$ind_contri == max(df_all$ind_contri)],
    " contributing the most with ",
    round2(max(df_all$ind_contri), 1),
    " per cent",
    " while ",
    df_all$industry[df_all$ind_contri == min(df_all$ind_contri)],
    " contributed",
    round2(abs(min(df_all$ind_contri)), 1),
    " per cent"
  )

  lab_df <- df_all %>%
    dplyr::mutate(
      lab_y = dplyr::if_else(ind_contri >= 0, ind_contri + 0.05, ind_contri - 0.05),
      lab_hjust = dplyr::if_else(ind_contri >= 0, 0, 1)
    )

  df_all %>%
    ggplot(aes(
      x = stats::reorder(industry, ind_contri),
      y = ind_contri
    )) +
    geom_col(aes(fill=-ind_contri)) +
    geom_text(
      data = lab_df,
      aes(y = lab_y,
        label = paste0(round(ind_contri, 1), "%")
      ),
      hjust = lab_df$lab_hjust,
      colour = "black",
      size = 11 / .pt
    ) +
    coord_flip(clip = "off") +
 #   scale_y_continuous(expand = expansion(mult = c(0.2, 0.15))) +
    scale_fill_distiller(palette = "blue") +
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
        "Growth in hours worked by industry between February 2020 and ",
        format(max(data$date), "%B %Y")
      ),
      caption = caption_lfs_det_q()
    )
}

viz_industries_emp_sincecovid_bar_contri <- function(data = filter_dash_data(c(  "A84601680F",
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
  df <- data %>%
    dplyr::group_by(.data$industry,.data$date) %>%
    dplyr::summarise(emp = sum(value)) %>%
    dplyr::mutate(change = emp - lag(emp),
                  growth = 100 * ((emp / lag(emp) - 1))) %>%
    dplyr::filter(date == max(.data$date))

  df_total <- data %>%
    dplyr::group_by(.data$date) %>%
    dplyr:: summarise(emp = sum(value)) %>%
    dplyr::mutate(total_lag = lag(emp))


  df_all <- df %>%
    merge(df_total, by = "date") %>%
    dplyr::mutate(ind_contri = change/total_lag*100) %>%
    select(industry, date, ind_contri)%>%
    filter(df$industry != "")%>%
    arrange(ind_contri)

  title <- paste0(
    "Total employment across industries",
    dplyr::if_else(sum(df_all$ind_contri) > 0, " grew", " shrank"),
    " by ",
    round2(sum(df_all$ind_contri), 1),
    " per cent between February 2020 and ",
    format(df_all$date, "%B %Y"),
    ", with ",
    df_all$industry[df_all$ind_contri == max(df_all$ind_contri)],
    " contributing the most with ",
    round2(max(df_all$ind_contri), 1),
    " per cent",
    " while ",
    df_all$industry[df_all$ind_contri == min(df_all$ind_contri)],
    " contributed",
    round2(abs(min(df_all$ind_contri)), 1),
    " per cent"
  )

  lab_df <- df_all %>%
    dplyr::mutate(
      lab_y = dplyr::if_else(ind_contri >= 0, ind_contri + 0.02, ind_contri - 0.02),
      lab_hjust = dplyr::if_else(ind_contri >= 0, 0, 1)
    )

  df_all %>%
    ggplot(aes(
      x = stats::reorder(industry, ind_contri),
      y = ind_contri
    )) +
    geom_col(aes(fill=-ind_contri)) +
    geom_text(
      data = lab_df,
      aes(y = lab_y,
          label = paste0(round(ind_contri, 1), "%")
      ),
      hjust = lab_df$lab_hjust,
      colour = "black",
      size = 11 / .pt
    ) +
    coord_flip(clip = "off") +
    #   scale_y_continuous(expand = expansion(mult = c(0.2, 0.15))) +
    scale_fill_distiller(palette = djprtheme::djpr_green) +
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
        "Growth in hours worked by industry between February 2020 and ",
        format(max(data$date), "%B %Y")
      ),
      caption = caption_lfs_det_q()
    )
}

# viz_industries_empchange_sincecovid_bar <- function(data = filter_dash_data(c(
#                                                       "A84601680F",
#                                                       "A84601683L",
#                                                       "A84601686V",
#                                                       "A84601665J",
#                                                       "A84601704L",
#                                                       "A84601707V",
#                                                       "A84601710J",
#                                                       "A84601638A",
#                                                       "A84601653X",
#                                                       "A84601689A",
#                                                       "A84601656F",
#                                                       "A84601713R",
#                                                       "A84601668R",
#                                                       "A84601695W",
#                                                       "A84601698C",
#                                                       "A84601650T",
#                                                       "A84601671C",
#                                                       "A84601641R",
#                                                       "A84601716W",
#                                                       "A84601662A"
#                                                     ),
#                                                     df = dash_data
#                                                     )) {
#   data <- data %>%
#     dplyr::group_by(.data$series) %>%
#     dplyr::mutate(value = 100 * ((.data$value / .data$value[date == as.Date("2020-02-01")]) - 1))
#
#   # reduce to only latest month (indexing already done above in data input into function)                                {
#   data <- data %>%
#     dplyr::group_by(.data$series) %>%
#     dplyr::filter(.data$date == max(.data$date)) %>%
#     dplyr::ungroup()
#
#   # add entry for data$industry for Victoria; employed total
#   data <- data %>%
#     dplyr::mutate(
#       industry = dplyr::if_else(.data$industry == "",
#         "Victoria, all industries",
#         .data$industry
#       )
#     )
#
#   lab_df <- data %>%
#     dplyr::select(industry, value) %>%
#     dplyr::mutate(
#       lab_y = dplyr::if_else(value >= 0, value + 0.1, value - 0.75),
#       lab_hjust = dplyr::if_else(value >= 0, 0, 1)
#     )
#
#   title <- paste0(
#     "Employment in ",
#     data$industry[data$value == max(data$value)],
#     dplyr::if_else(max(data$value) > 0, " grew", " shrank"),
#     " by ",
#     round2(max(data$value), 1),
#     " per cent between February 2020 and ",
#     format(max(data$date), "%B %Y"),
#     ", while employment in ",
#     data$industry[data$value == min(data$value)],
#     dplyr::if_else(min(data$value) > 0, " grew", " shrank"),
#     " by ",
#     round2(abs(min(data$value)), 1),
#     " per cent"
#   )
#
#   # draw bar chart for all 19 industries plus Vic total
#   data %>%
#     ggplot(aes(
#       x = stats::reorder(industry, value),
#       y = value
#     )) +
#     geom_col(
#       aes(fill = -value)
#     ) +
#     geom_text(
#       data = lab_df,
#       aes(
#         y = lab_y,
#         hjust = lab_hjust,
#         label = paste0(round(value, 1), "%")
#       ),
#       colour = "black",
#       size = 11 / .pt
#     ) +
#     geom_hline(
#       yintercept = 0
#     ) +
#     coord_flip(clip = "off") +
#     scale_y_continuous(expand = expansion(mult = c(0.2, 0.15))) +
#     scale_fill_distiller(palette = "Blues") +
#     djprtheme::theme_djpr(flipped = TRUE) +
#     theme(
#       axis.title.x = element_blank(),
#       panel.grid = element_blank(),
#       axis.text.y = element_text(size = 12),
#       axis.text.x = element_blank()
#     ) +
#     labs(
#       title = title,
#       subtitle = paste0(
#         "Growth in employment by industry between February 2020 and ",
#         format(max(data$date), "%B %Y")
#       ),
#       caption = caption_lfs_det_q()
#     )
# }

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
  data <- data %>%
    dplyr::mutate(
      industry = dplyr::if_else(.data$industry == "",
        "Victoria, all industries",
        .data$industry
      )
    )

  # filter out the chosen industry and vic_total
  data <- data %>%
    group_by(.data$indicator) %>%
    dplyr::filter(.data$industry %in%
      c("Victoria, all industries", .env$chosen_industry))

  table_df <- data %>%
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

  col_names <- names(table_df)

  col_header_style <- list(
    `font-weight` = "600"
  )

  my_table <- table_df %>%
    rename(
      industry = 3,
      aggregate = 4
    ) %>%
    reactable::reactable(
      columns = list(
        indicator = reactable::colDef(
          name = "",
          minWidth = 65,
          style = reactable::JS("function(rowInfo, colInfo, state) {
        var firstSorted = state.sorted[0]
        // Merge cells if unsorted
        if (!firstSorted || firstSorted.id === 'indicator') {
          var prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.row['indicator'] === prevRow['indicator']) {
            return { visibility: 'hidden' }
          }
        }
      }")
        ),
        series = reactable::colDef(
          name = "",
          minWidth = 45
        ),
        industry = reactable::colDef(
          name = col_names[3],
          headerStyle = col_header_style
        ),
        aggregate = reactable::colDef(
          name = col_names[4],
          headerStyle = col_header_style
        )
      ),
      defaultColDef = reactable::colDef(
        minWidth = 50
      ),
      highlight = TRUE,
      resizable = TRUE,
      sortable = FALSE,
      theme = reactable::reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "7px 1px 1px 1px",
        tableStyle = list(`border-bottom` = "1px solid #000"),
        headerStyle = list(
          fontWeight = "normal",
          `border-bottom` = "1px solid #000"
        ),
        groupHeaderStyle = list(fontWeight = "normal"),
        style = list(fontFamily = "Roboto, sans-serif, -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial")
      )
    )

  my_table
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
  data <- data %>%
    dplyr::mutate(
      industry = dplyr::if_else(.data$industry == "",
        "Victoria, all industries",
        .data$industry
      )
    )

  data <- data %>%
    dplyr::filter(.data$industry %in% c("Victoria, all industries", .env$chosen_industry))

  data <- data %>%
    dplyr::group_by(industry) %>%
    dplyr::mutate(
      value = 100 * ((value / dplyr::lag(value, 4)) - 1)
    ) %>%
    select(date, industry, value) %>%
    dplyr::ungroup()

  data %>%
    dplyr::filter(!is.na(value)) %>%
    djpr_ts_linechart(
      col_var = .data$industry,
      label_num = paste0(round(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%")
    ) +
    labs(
      subtitle = "Change in total employment",
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
    dplyr::select(date, value, series, indicator, sex, industry, gcc_restofstate)

  df <- df %>%
    dplyr::group_by(.data$sex, .data$industry) %>%
    dplyr::summarise(
      value = sum(value),
      date = max(date)
    ) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::group_by(industry) %>%
    dplyr::mutate(perc = value / sum(value)) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::mutate(order = if_else(industry == "Victoria, all industries", 2, 1))

  label_df <- df %>%
    dplyr::group_by(industry) %>%
    dplyr::arrange(desc(sex)) %>%
    dplyr::mutate(label_y = cumsum(perc) - perc + (perc / 2))

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
      x = stats::reorder(stringr::str_wrap(industry, 15), -order),
      y = value, fill = sex
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
    # scale_x_discrete(expand = expansion(add = c(0.25, 0.85))) +
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
      caption = caption_lfs_det_q(),
      title = title
    )
}
