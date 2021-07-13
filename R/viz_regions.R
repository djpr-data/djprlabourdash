#' Function to create the graphs for the 'Regions' subpage on the dashboard.
#' @name title_unemprate_vic
#' @param data the dataframe containing data to visualise
#' @examples
#' \dontrun{
#'
#' dash_data <- load_dash_data()
#'
#' # for 'viz_emp_regions_sincecovid':
#' data <- filter_dash_data(c(
#'   "A84600141A",
#'   "A84600075R"
#' )) %>%
#'   dplyr::filter(date >= as.Date("2020-01-01"))
#'
#' # for 'viz_reg_unemprate_multiline':
#' data <- filter_dash_data(c(
#'   "A84600253V",
#'   "A84599659L",
#'   "A84600019W",
#'   "A84600187J",
#'   "A84599557X",
#'   "A84600115W",
#'   "A84599851L",
#'   "A84599923L",
#'   "A84600025T",
#'   "A84600193C",
#'   "A84599665J",
#'   "A84600031L",
#'   "A84599671C",
#'   "A84599677T",
#'   "A84599683L",
#'   "A84599929A",
#'   "A84600121T",
#'   "A84600037A"
#' ))
#' }
#' @importFrom rlang `:=`
title_unemprate_vic <- function(data = filter_dash_data(c(
                                  "A84599659L",
                                  "A84600019W",
                                  "A84600187J",
                                  "A84599557X",
                                  "A84600115W",
                                  "A84599851L",
                                  "A84599923L",
                                  "A84600025T",
                                  "A84600193C",
                                  "A84599665J",
                                  "A84600031L",
                                  "A84599671C",
                                  "A84599677T",
                                  "A84599683L",
                                  "A84599929A",
                                  "A84600121T",
                                  "A84600037A"
                                )) %>%
                                  group_by(.data$series_id) %>%
                                  mutate(value = slider::slide_mean(.data$value,
                                    before = 2,
                                    complete = TRUE
                                  )) %>%
                                  dplyr::filter(.data$date == max(.data$date))) {
  high_low <- data %>%
    dplyr::ungroup() %>%
    summarise(
      min_sa4 = .data$sa4[.data$value == min(.data$value)],
      min_ur = .data$value[.data$value == min(.data$value)],
      max_sa4 = .data$sa4[.data$value == max(.data$value)],
      max_ur = .data$value[.data$value == max(.data$value)],
      date = unique(.data$date)
    )

  paste0(
    "The unemployment rate across Victoria ranges from ",
    round2(high_low$min_ur, 1),
    " per cent in ",
    high_low$min_sa4,
    " to ",
    round2(high_low$max_ur, 1),
    " per cent in ",
    high_low$max_sa4,
    " as at ",
    format(high_low$date, "%B %Y")
  )
}

map_unemprate_vic <- function(data = filter_dash_data(c(
                                "A84600253V",
                                "A84600145K",
                                "A84599659L",
                                "A84600019W",
                                "A84600187J",
                                "A84599557X",
                                "A84600115W",
                                "A84599851L",
                                "A84599923L",
                                "A84600025T",
                                "A84600193C",
                                "A84600079X",
                                "A84599665J",
                                "A84600031L",
                                "A84599671C",
                                "A84599677T",
                                "A84599683L",
                                "A84599929A",
                                "A84600121T",
                                "A84600037A"
                              )) %>%
                                group_by(.data$series_id) %>%
                                mutate(value = slider::slide_mean(.data$value,
                                  before = 2,
                                  complete = TRUE
                                )) %>%
                                dplyr::filter(.data$date == max(.data$date)),
                              zoom = 6) {

  # Call SA4 shape file, but only load Victoria and exclude 'weird' areas (migratory and other one)
  sa4_shp <- sa42016 %>%
    dplyr::filter(.data$state_name_2016 == "Victoria") %>%
    dplyr::filter(.data$sa4_code_2016 < 297)

  # Fix issue with different naming for North West region in Victoria
  data <- data %>%
    dplyr::mutate(
      sa4 = dplyr::if_else(.data$sa4 == "Victoria - North West",
        "North West",
        .data$sa4
      )
    )

  # Join shape file with data to create mapdata ----
  mapdata <- sa4_shp %>%
    dplyr::left_join(data, by = c("sa4_name_2016" = "sa4"))

  # Create colour palette
  # Switched here from binned to continuous colours
  # pal <- leaflet::colorBin("Blues", mapdata$value, 3) # last object is number of bins
  pal <- leaflet::colorNumeric("Blues", c(min(mapdata$value), max(mapdata$value)), alpha = T)

  # Create metro boundary (Greater Melbourne) ----
  metro_boundary_sa4 <- c(
    "Melbourne - Inner", "Melbourne - Inner East", "Melbourne - Inner South", "Melbourne - North East",
    "Melbourne - North West", "Melbourne - Outer East", "Melbourne - South East", "Melbourne - West",
    "Mornington Peninsula"
  )

  mapdata <- mapdata %>%
    sf::st_transform("+proj=longlat +datum=WGS84")

  metro_outline <- mapdata %>%
    dplyr::filter(.data$sa4_name_2016 %in% metro_boundary_sa4) %>%
    dplyr::summarise(areasqkm_2016 = sum(.data$areasqkm_2016))

  # Produce dynamic map, all of Victoria ----
  map <- mapdata %>%
    leaflet::leaflet(options = leaflet::leafletOptions(background = "white")) %>%
    leaflet::setView(
      lng = 145.4657, lat = -36.41472, # coordinates of map at first view
      zoom = zoom
    ) %>%
    # size of map at first view
    leaflet::addPolygons(
      color = "grey", # colour of boundary lines, 'transparent' for no lines
      weight = 1, # thickness of boundary lines
      fillColor = ~ pal(mapdata$value), # pre-defined above
      fillOpacity = 1.0, # strength of fill colour
      smoothFactor = 0.5, # smoothing between region
      stroke = T,
      highlightOptions = leaflet::highlightOptions( # to highlight regions as you hover over them
        color = "black", # boundary colour of region you hover over
        weight = 2, # thickness of region boundary
        bringToFront = FALSE
      ), # FALSE = metro outline remains
      label = sprintf( # region label definition
        "<strong>%s</strong><br/>Unemployment rate: %.1f", # label title, strong = bold, %.1f = 1 dec points
        mapdata$sa4_name_2016, # region name displayed in label
        mapdata$value
      ) %>% # eco data displayed in label
        lapply(htmltools::HTML),
      labelOptions = leaflet::labelOptions( # label options
        style = list(
          "font-weight" = "normal", # "bold" makes it so
          padding = "3px 8px"
        ),
        textsize = "12px", # text size of label
        noHide = FALSE, # TRUE makes labels permanently visible (messy)
        direction = "auto"
      ) # text box flips from side to side as needed
    ) %>%
    leaflet::addLegend(
      position = "topright", # options: topright, bottomleft etc.
      pal = pal, # colour palette as defined
      values = mapdata$value, # fill data
      bins = 3,
      labFormat = leaflet::labelFormat(transform = identity),
      title = "Unemployment<br/>rate (per cent)", # label title
      opacity = 1,
    ) %>%
    # label opacity
    leaflet::addPolygons(
      data = metro_outline, #
      fill = F,
      stroke = T,
      opacity = 1,
      color = "black",
      weight = 1
    )
  # Display dynamic map: can zoom in, zoom out and hover over regions displaying distinct data----
  map
}

# Comparison of change in employment since Mar-20 in Greater Melbourne region and Rest of Victoria
title_reg_emp_regions_sincecovid_line <- function(data) {
  current <- data %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = 100 * ((.data$value / .data$value[date == as.Date("2020-03-01")]) - 1)) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$gcc_restofstate, .data$value) %>%
    tidyr::spread(key = .data$gcc_restofstate, value = .data$value)

  diff <- current$`Greater Melbourne` - current$`Rest of Vic.`
  case_when(
    abs(diff) < 0.1 ~
    "Employment in Greater Melbourne has caught up with the rest of Victoria",
    sign(diff) == -1 ~
    "Employment in Greater Melbourne has not kept pace with the rest of Victoria",
    TRUE ~ "Employment in Greater Melbourne has grown faster than the rest of Victoria"
  )
}

viz_reg_emp_regions_sincecovid_line <- function(data = filter_dash_data(c(
                                                  "A84600141A",
                                                  "A84600075R"
                                                )) %>%
                                                  dplyr::group_by(series_id) %>%
                                                  dplyr::mutate(value = slider::slide_mean(value,
                                                    before = 2,
                                                    complete = T
                                                  )) %>%
                                                  dplyr::filter(date >= as.Date("2020-01-01")),
                                                title = title_reg_emp_regions_sincecovid_line(data = data)) {
  df <- data %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(value = 100 * ((value / value[date == as.Date("2020-03-01")]) - 1))

  df %>%
    djpr_ts_linechart(
      col_var = gcc_restofstate,
      label_num = paste0(round(.data$value, 1), "%"),
      y_labels = function(x) paste0(x, "%"),
      hline = 0
    ) +
    labs(
      title = title,
      subtitle = "Cumulative change in employment (%) in Greater Melbourne and the rest of Victoria since March 2020",
      caption = paste0(caption_lfs_det_m(), " Data not seasonally adjusted. Smoothed using a 3 month rolling average.")
    )
}

viz_reg_unemprate_multiline <- function(data = filter_dash_data(c(
                                          "A84600253V",
                                          "A84599659L",
                                          "A84600019W",
                                          "A84600187J",
                                          "A84599557X",
                                          "A84600115W",
                                          "A84599851L",
                                          "A84599923L",
                                          "A84600025T",
                                          "A84600193C",
                                          "A84599665J",
                                          "A84600031L",
                                          "A84599671C",
                                          "A84599677T",
                                          "A84599683L",
                                          "A84599929A",
                                          "A84600121T",
                                          "A84600037A"
                                        )) %>%
                                          dplyr::group_by(.data$series_id) %>%
                                          dplyr::mutate(value = slider::slide_mean(.data$value,
                                            before = 2,
                                            complete = TRUE
                                          )) %>%
                                          dplyr::filter(!is.na(.data$value))) {
  data <- data %>%
    dplyr::mutate(
      tooltip = paste0(
        .data$sa4, "\n", format(.data$date, "%b %Y"),
        "\n", round2(.data$value, 1), "%"
      ),
      sa4 = gsub(" and South ", " & S. ", .data$sa4, fixed = TRUE)
    )

  max_y <- max(data$value)
  mid_x <- stats::median(data$date)

  data <- data %>%
    dplyr::mutate(
      sa4 = dplyr::if_else(.data$sa4 == "", "Victoria", .data$sa4),
      is_vic = dplyr::if_else(.data$sa4 == "Victoria", TRUE, FALSE)
    )

  data <- data %>%
    dplyr::mutate(
      line_col =
        dplyr::case_when(
          .data$is_vic ~
          "Victoria",
          grepl("Melbourne|Mornington", .data$sa4) ~
          "Greater Melbourne",
          TRUE ~ "Rest of Victoria"
        )
    )


  vic <- data %>%
    filter(.data$sa4 == "Victoria") %>%
    select(-.data$sa4)

  facet_labels <- data %>%
    dplyr::group_by(.data$sa4, .data$is_vic, .data$line_col) %>%
    dplyr::summarise() %>%
    dplyr::mutate(
      x = .env$mid_x,
      y = .env$max_y
    )

  reg_sa4s <- sort(unique(data$sa4[data$line_col == "Rest of Victoria"]))
  melb_sa4s <- sort(unique(data$sa4[data$line_col == "Greater Melbourne"]))

  data$sa4 <- factor(data$sa4,
    levels = c("Victoria", reg_sa4s, melb_sa4s)
  )

  highest_current_ur <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::filter(.data$value == max(.data$value)) %>%
    dplyr::pull(.data$sa4)

  title <- paste0(
    highest_current_ur, " had the highest unemployment rate in Victoria in ",
    format(max(data$date), "%B %Y")
  )

  data %>%
    djpr_ts_linechart(
      col_var = line_col,
      label = F,
      dot = F
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      limits = function(limits) c(0, limits[2]),
      breaks = function(limits) {
        c(
          0,
          min(c(
            limits[2],
            10
          ))
        )
      },
      expand = expansion(mult = c(0, 0.1))
    ) +
    geom_label(
      data = facet_labels,
      aes(
        label = stringr::str_wrap(.data$sa4, 11),
        y = .data$y,
        x = .data$x
      ),
      nudge_y = 0.1,
      lineheight = 0.85,
      label.padding = unit(0.05, "lines"),
      label.size = 0,
      size = 12 / .pt
    ) +
    geom_line(data = vic) +
    facet_wrap(~ factor(sa4), ncol = 6, scales = "free_x") +
    scale_x_date(
      # date_labels = "%Y",
      breaks = scales::breaks_pretty(n = 3)
    ) +
    theme(
      strip.text = element_blank(),
      panel.spacing = unit(1.5, "lines"),
      axis.text = element_text(size = 12)
    ) +
    labs(
      title = title,
      subtitle = "Unemployment rate by region (SA4), per cent",
      caption = paste0(caption_lfs_det_m(), " Data not seasonally adjusted. Smoothed using a 3 month rolling average.")
    )
}

viz_reg_unemprate_bar <- function(data = filter_dash_data(c(
                                    "A84600253V",
                                    "A84599659L",
                                    "A84600019W",
                                    "A84600187J",
                                    "A84599557X",
                                    "A84600115W",
                                    "A84599851L",
                                    "A84599923L",
                                    "A84600025T",
                                    "A84600193C",
                                    "A84599665J",
                                    "A84600031L",
                                    "A84599671C",
                                    "A84599677T",
                                    "A84599683L",
                                    "A84599929A",
                                    "A84600121T",
                                    "A84600037A"
                                  ),
                                  df = dash_data
                                  ) %>%
                                    dplyr::group_by(series_id) %>%
                                    dplyr::mutate(value = slider::slide_mean(.data$value,
                                      before = 2,
                                      complete = TRUE
                                    )) %>%
                                    dplyr::filter(.data$date == max(.data$date))) {
  data <- data %>%
    dplyr::filter(.data$sa4 != "") %>%
    dplyr::mutate(sa4 = dplyr::if_else(grepl("Warrnambool", .data$sa4),
      "Warrnambool & S. West",
      .data$sa4
    ))

  data %>%
    ggplot(aes(
      x = stats::reorder(.data$sa4, .data$value),
      y = .data$value
    )) +
    geom_col(
      col = "grey85",
      aes(fill = -.data$value)
    ) +
    geom_text(
      nudge_y = 0.1,
      aes(label = round(.data$value, 1)),
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
    labs(title = "")
}

text_reg_regions_sincecovid <- function(data = filter_dash_data(c(
                                          "A84600141A",
                                          "A84600075R"
                                        ))) {
  emp_gcc_rest <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(.data$value,
      before = 2,
      complete = TRUE
    )) %>%
    dplyr::filter(.data$date >= as.Date("2020-01-01")) %>%
    dplyr::ungroup()

  emp_gcc_rest <- emp_gcc_rest %>%
    dplyr::filter(.data$date %in% c(
      as.Date("2020-03-01"),
      as.Date("2020-10-01"),
      max(.data$date)
    )) %>%
    dplyr::select(.data$date, .data$value, .data$gcc_restofstate) %>%
    dplyr::group_by(.data$gcc_restofstate) %>%
    dplyr::mutate(
      d_sincecovid_abs = .data$value - .data$value[date == as.Date("2020-03-01")],
      d_sincecovid_perc = 100 * ((.data$value /
        .data$value[date == as.Date("2020-03-01")]) - 1),
      gcc_restofstate = dplyr::if_else(.data$gcc_restofstate == "Greater Melbourne",
        "melb", "rest"
      )
    )

  emp_gcc_rest <- emp_gcc_rest %>%
    split(emp_gcc_rest$gcc_restofstate)

  melb_emp_precovid <- emp_gcc_rest$melb %>%
    dplyr::filter(.data$date == min(.data$date)) %>%
    dplyr::pull(.data$value) / 1000


  melb_emp_oct20 <- emp_gcc_rest$melb %>%
    dplyr::filter(.data$date == as.Date("2020-10-01")) %>%
    dplyr::pull(.data$value) / 1000

  melb_emp_covid_to_oct_abs <- (1000 * (melb_emp_oct20 - melb_emp_precovid)) %>%
    round2(0)

  melb_emp_covid_to_oct_perc <- (100 * ((melb_emp_oct20 / melb_emp_precovid) - 1)) %>%
    round2(1)

  rest_emp_covid_to_oct_perc <- emp_gcc_rest$rest %>%
    dplyr::filter(.data$date == as.Date("2020-10-01")) %>%
    dplyr::pull(.data$d_sincecovid_perc) %>%
    round2(1)

  melb_emp_current <- (emp_gcc_rest$melb %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::pull(.data$d_sincecovid_perc)) %>%
    round2(1)

  latest_month <- max(emp_gcc_rest$melb$date) %>% format("%B %Y")

  rest_emp_current <- (emp_gcc_rest$rest %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::pull(.data$d_sincecovid_perc)) %>%
    round2(1)


  text_active(
    string = paste0(
      "In March 2020, before COVID-19 struck Victoria, there were XX million ",
      "people employed in Greater Melbourne. Employment fell to XX million ",
      "people by October 2020 - a decline of XX or XX per cent. Employment ",
      "in the rest of Victoria fell by only XX per cent over the same period. ",
      "Greater Melbourne employment is XX per cent XX pre-COVID levels as at XX, ",
      "while employment in the rest of Victoria is XX per cent XX where it ",
      "was in March 2020."
    ),
    numbers = c(
      round2(melb_emp_precovid, 2),
      round2(melb_emp_oct20, 2),
      paste0(abs(melb_emp_covid_to_oct_abs), ",000"),
      abs(melb_emp_covid_to_oct_perc),
      abs(rest_emp_covid_to_oct_perc),
      abs(melb_emp_current),
      dplyr::if_else(melb_emp_current > 0, "above", "below"),
      latest_month,
      abs(rest_emp_current),
      dplyr::if_else(rest_emp_current > 0, "above", "below")
    )
  )
}

viz_reg_unemprate_dispersion <- function(data = filter_dash_data(c(
                                           "A84600253V",
                                           "A84599659L",
                                           "A84600019W",
                                           "A84600187J",
                                           "A84599557X",
                                           "A84600115W",
                                           "A84599851L",
                                           "A84599923L",
                                           "A84600025T",
                                           "A84600193C",
                                           "A84599665J",
                                           "A84600031L",
                                           "A84599671C",
                                           "A84599677T",
                                           "A84599683L",
                                           "A84599929A",
                                           "A84600121T",
                                           "A84600037A"
                                         ),
                                         df = dash_data
                                         ) %>%
                                           dplyr::group_by(series_id) %>%
                                           dplyr::mutate(value = slider::slide_mean(.data$value, before = 2, complete = TRUE))) {
  df_summ <- data %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(sa4 = dplyr::if_else(.data$sa4 == "", "Victoria", .data$sa4)) %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(
      vic = value[sa4 == "Victoria"],
      max_ur = max(value),
      min_ur = min(value)
    ) %>%
    dplyr::mutate(range = max_ur - min_ur)

  df_tidy <- df_summ %>%
    dplyr::select(-.data$range) %>%
    tidyr::gather(
      key = series, value = value,
      -.data$date
    )

  df_tidy <- df_tidy %>%
    mutate(
      tooltip = case_when(
        series == "vic" ~ "Victoria ",
        series == "max_ur" ~ "Highest ",
        series == "min_ur" ~ "Lowest ",
        TRUE ~ NA_character_
      ),
      tooltip = paste0(tooltip, round2(.data$value, 1), "%")
    )

  days_in_data <- as.numeric(max(data$date) - min(data$date))

  # First plot: Show highest / lowest / state-wide unemp rates----
  plot_high_low <- df_tidy %>%
    ggplot(aes(x = .data$date)) +
    geom_ribbon(
      data = df_summ,
      aes(
        ymin = .data$min_ur,
        ymax = .data$max_ur
      ),
      colour = NA,
      alpha = 0.25
    ) +
    geom_line(
      aes(
        y = .data$value,
        color = .data$series
      )
    ) +
    ggiraph::geom_point_interactive(aes(
      tooltip = .data$tooltip,
      y = .data$value
    ),
    size = 3,
    colour = "white",
    alpha = 0.01
    ) +
    ggrepel::geom_label_repel(
      data = ~ filter(., date == max(date)),
      aes(
        label = stringr::str_wrap(.data$tooltip, 7),
        col = .data$series,
        y = .data$value
      ),
      label.size = NA,
      lineheight = 0.85,
      size = 12 / .pt,
      hjust = 0,
      min.segment.length = unit(10000, "lines"),
      nudge_x = days_in_data * 0.1,
      label.padding = 0.01,
      direction = "y"
    ) +
    djprtheme::theme_djpr() +
    djprtheme::djpr_y_continuous(
      limits = c(0, max(df_tidy$value)),
      labels = function(x) paste0(x, "%")
    ) +
    scale_x_date(
      expand = expansion(
        add = c(0, days_in_data * 0.2)
      ),
      breaks = scales::breaks_pretty(n = 4),
      date_labels = "%b\n%Y"
    ) +
    scale_colour_manual(values = c(
      "min_ur" = djprtheme::djpr_green,
      "max_ur" = djprtheme::djpr_royal_blue,
      "vic" = "black"
    )) +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 12),
      plot.subtitle = element_text(size = 14)
    ) +
    labs(subtitle = "Highest and lowest unemployment rates\nin Victorian regions (SA4s)")


  # Second plot: Range between high and low -----
  plot_range <- df_summ %>%
    ggplot(aes(x = date, y = range)) +
    ggiraph::geom_col_interactive(aes(tooltip = paste0(
      format(.data$date, "%B %Y"),
      "\n", round2(.data$range, 1), " ppts"
    )),
    fill = djpr_pal(1),
    colour = NA,
    size = 0,
    alpha = 0.25
    ) +
    theme_djpr() +
    scale_x_date(
      date_labels = "%b\n%Y",
      breaks = scales::breaks_pretty(n = 4)
    ) +
    djpr_y_continuous(
      limits = function(x) c(0, max(x)) # ,
      # labels = function(x) paste0(x, " ppts")
    ) +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 12),
      plot.subtitle = element_text(size = 14)
    ) +
    labs(subtitle = "Range between highest and lowest\n(percentage points)")

  # Create title -----

  current_range <- df_summ %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::pull(.data$range) %>%
    round2(1)

  plot_title <- paste0(
    "There was a ", current_range,
    " percentage point gap between the highest and lowest ",
    "unemployment rates in Victorian regions in ",
    format(max(df_tidy$date), "%B %Y")
  )

  # Combine plots -----
  plots_combined <- patchwork::wrap_plots(plot_high_low,
    plot_range,
    patchwork::plot_spacer(),
    ncol = 2
  ) +
    coord_cartesian(clip = "off") +
    patchwork::plot_layout(heights = c(0.99, 0.01)) +
    patchwork::plot_annotation(
      title = plot_title,
      subtitle = "Gap between unemployment rates across Victorian regions (SA4s), including metropolitan SA4s",
      caption = paste0(caption_lfs_det_m(), " Data not seasonally adjusted. Smoothed using a 3 month rolling average."),
      theme = theme_djpr()
    )

  plots_combined
}

map_reg_sa4 <- function(sa4 = c(
                          "Melbourne - North East",
                          "Melbourne - Inner",
                          "Ballarat",
                          "Geelong",
                          "Hume",
                          "Latrobe - Gippsland",
                          "Melbourne - Outer East",
                          "Melbourne - South East",
                          "North West",
                          "Melbourne - Inner East",
                          "Melbourne - West",
                          "Bendigo",
                          "Warrnambool and South West",
                          "Melbourne - North West",
                          "Shepparton",
                          "Melbourne - Inner South",
                          "Mornington Peninsula"
                        )) {
  sa4 <- match.arg(sa4)

  all_areas <- sa42016 %>%
    dplyr::filter(.data$state_name_2016 == "Victoria") %>%
    dplyr::mutate(selected = dplyr::if_else(.data$sa4_name_2016 == .env$sa4, TRUE, FALSE))

  selected_area <- all_areas %>%
    dplyr::filter(.data$selected == TRUE)

  all_areas %>%
    ggplot() +
    geom_sf(aes(alpha = .data$selected),
      size = 0.25,
      fill = djprtheme::djpr_royal_blue,
      colour = djprtheme::djpr_cool_grey_11
    ) +
    geom_curve(
      data = selected_area,
      aes(x = .data$cent_long, y = .data$cent_lat),
      xend = 147, yend = -35,
      curvature = 0.2,
      colour = "#1F1547"
    ) +
    geom_point(
      data = selected_area,
      aes(x = .data$cent_long, y = .data$cent_lat),
      colour = "#1F1547",
      size = 3,
      shape = "circle filled",
      stroke = 1.5,
      fill = "white"
    ) +
    annotate("label",
      x = 147,
      y = -34.5,
      label = stringr::str_wrap(sa4, 14),
      colour = "#1F1547",
      size = 24 / .pt,
      fontface = "bold",
      lineheight = 0.9,
      label.size = 0
    ) +
    scale_alpha_manual(values = c(
      `FALSE` = 0.2,
      `TRUE` = 0.8
    )) +
    theme_void() +
    theme(legend.position = "none")
}

viz_reg_sa4unemp_cf_broadregion <- function(data = filter_dash_data(
                                              c(
                                                "A84599659L",
                                                "A84600019W",
                                                "A84600187J",
                                                "A84599557X",
                                                "A84600115W",
                                                "A84599851L",
                                                "A84599923L",
                                                "A84600025T",
                                                "A84600193C",
                                                "A84599665J",
                                                "A84600031L",
                                                "A84599671C",
                                                "A84599677T",
                                                "A84599683L",
                                                "A84599929A",
                                                "A84600121T",
                                                "A84600037A",
                                                "A84595516F",
                                                "A84595471L"
                                              )
                                            ) %>%
                                              dplyr::group_by(series_id) %>%
                                              dplyr::mutate(value = slider::slide_mean(.data$value, before = 2, complete = TRUE)) %>%
                                              dplyr::filter(.data$date >= max(.data$date) - (365.25 * 5)),
                                            sa4 = "Geelong") {
  in_melb <- grepl("Melbourne|Mornington", sa4)

  broad_region <- dplyr::if_else(in_melb,
    "Greater Melbourne",
    "Regional Victoria"
  )

  data <- data %>%
    mutate(sa4 = dplyr::if_else(.data$sa4 == "Victoria - North West",
      "North West",
      .data$sa4
    ))

  sa4_df <- data %>%
    dplyr::filter(.data$sa4 == .env$sa4) %>%
    mutate(col_var = sa4)

  current_sa4_ur <- sa4_df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::pull(value) %>%
    round2(1)

  comparator_id <- dplyr::if_else(in_melb,
    "A84595516F",
    "A84595471L"
  )

  comparator_df <- data %>%
    dplyr::filter(
      .data$series_id == comparator_id,
      .data$date >= min(sa4_df$date)
    ) %>%
    mutate(col_var = broad_region)

  current_comp_ur <- comparator_df %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::pull(value) %>%
    round2(1)

  sa4_cf_comp <- dplyr::case_when(
    current_sa4_ur > current_comp_ur ~ "higher",
    current_sa4_ur < current_comp_ur ~ "lower",
    current_sa4_ur == current_comp_ur ~ "the same",
    TRUE ~ NA_character_
  )

  comb <- dplyr::bind_rows(
    comparator_df,
    sa4_df
  )

  comb %>%
    djpr_ts_linechart(
      col_var = col_var,
      label_num = paste0(round(.data$value, 1), "%")
    ) +
    scale_x_date(
      breaks = scales::breaks_pretty(5),
      date_labels = "%b\n%Y",
      expand = expansion(mult = c(0.05, 0.25))
    ) +
    scale_y_continuous(
      limits = function(limits) c(0, limits[2]),
      labels = function(x) paste0(x, "%"),
      breaks = scales::breaks_pretty(4),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = paste0(
        "The unemployment rate is ",
        sa4_cf_comp,
        " in ",
        sa4,
        dplyr::if_else(sa4_cf_comp == "the same", " and ", " than "),
        "in ",
        broad_region,
        " as a whole"
      ),
      subtitle = paste0(
        "Unemployment rate in ",
        sa4,
        " and ",
        broad_region
      )
    )
}

table_region_focus <- function(data = filter_dash_data(
                                 c(
                                   "A84600141A",
                                   "A84600144J",
                                   "A84600145K",
                                   "A84599655C",
                                   "A84599658K",
                                   "A84599659L",
                                   "A84600015L",
                                   "A84600018V",
                                   "A84600019W",
                                   "A84600183X",
                                   "A84600186F",
                                   "A84600187J",
                                   "A84599553R",
                                   "A84599556W",
                                   "A84599557X",
                                   "A84600111L",
                                   "A84600114V",
                                   "A84600115W",
                                   "A84599847W",
                                   "A84599850K",
                                   "A84599851L",
                                   "A84599919W",
                                   "A84599922K",
                                   "A84599923L",
                                   "A84600021J",
                                   "A84600024R",
                                   "A84600025T",
                                   "A84600189L",
                                   "A84600192A",
                                   "A84600193C",
                                   "A84600075R",
                                   "A84600078W",
                                   "A84600079X",
                                   "A84599661X",
                                   "A84599664F",
                                   "A84599665J",
                                   "A84600027W",
                                   "A84600030K",
                                   "A84600031L",
                                   "A84599667L",
                                   "A84599670A",
                                   "A84599671C",
                                   "A84599673J",
                                   "A84599676R",
                                   "A84599677T",
                                   "A84599679W",
                                   "A84599682K",
                                   "A84599683L",
                                   "A84599925T",
                                   "A84599928X",
                                   "A84599929A",
                                   "A84600117A",
                                   "A84600120R",
                                   "A84600121T",
                                   "A84600033T",
                                   "A84600036X",
                                   "A84600037A"
                                 )
                               ) %>%
                                 dplyr::group_by(.data$series_id) %>%
                                 dplyr::mutate(value = slider::slide_mean(.data$value, before = 2, complete = TRUE)),
                               sa4 = "Geelong") {
  in_melb <- grepl("Melbourne|Mornington", sa4)

  broad_region <- dplyr::if_else(in_melb,
    "Greater Melbourne",
    "Regional Victoria"
  )

  latest_date <- format(max(data$date), "%b %Y")

  data <- data %>%
    dplyr::mutate(sa4 = dplyr::if_else(.data$sa4 == "Victoria - North West",
      "North West",
      .data$sa4
    ))

  data <- data %>%
    dplyr::mutate(gcc_restofstate = dplyr::if_else(.data$gcc_restofstate ==
      "Rest of Vic.",
    "Regional Victoria",
    .data$gcc_restofstate
    )) %>%
    dplyr::mutate(geog = dplyr::if_else(.data$sa4 != "",
      .data$sa4,
      .data$gcc_restofstate
    )) %>%
    dplyr::filter(.data$geog %in% c(.env$broad_region, .env$sa4))

  table_df <- data %>%
    dplyr::group_by(.data$geog, .data$indicator) %>%
    dplyr::mutate(
      d_month = dplyr::if_else(indicator == "Employed total",
        100 * ((.data$value / dplyr::lag(.data$value, 1)) - 1),
        .data$value - dplyr::lag(.data$value, 1)
      ),
      d_year = dplyr::if_else(.data$indicator == "Employed total",
        100 * ((.data$value / dplyr::lag(.data$value, 12)) - 1),
        .data$value - dplyr::lag(.data$value, 12)
      )
    ) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(
      .data$indicator, .data$value, .data$geog,
      .data$d_month, .data$d_year
    ) %>%
    dplyr::ungroup()

  table_df <- table_df %>%
    dplyr::mutate(across(
      c(.data$value, .data$d_month, .data$d_year),
      ~ round2(.x, 1)
    )) %>%
    dplyr::mutate(
      value = dplyr::if_else(.data$indicator == "Employed total",
        scales::comma(.data$value * 1000),
        paste0(.data$value, "%")
      ),
      d_month = dplyr::if_else(.data$indicator == "Employed total",
        paste0(.data$d_month, "%"),
        paste0(.data$d_month, " ppts")
      ),
      d_year = dplyr::if_else(.data$indicator == "Employed total",
        paste0(.data$d_year, "%"),
        paste0(.data$d_year, " ppts")
      )
    )

  table_df <- table_df %>%
    dplyr::rename({{ latest_date }} := .data$value,
      `Change over month` = .data$d_month,
      `Change over year` = .data$d_year
    )

  table_df <- table_df %>%
    tidyr::gather(
      key = "series", value = "value",
      -.data$indicator, -.data$geog
    ) %>%
    tidyr::spread(key = .data$geog, value = .data$value)

  table_df <- table_df %>%
    dplyr::group_by(.data$indicator) %>%
    mutate(order = dplyr::case_when(
      series == "Change over month" ~ 2,
      series == "Change over year" ~ 3,
      TRUE ~ 1
    )) %>%
    dplyr::arrange(desc(.data$indicator), .data$order) %>%
    dplyr::select(-.data$order)

  col_header_style <- list(
    `font-weight` = "600"
  )

  table_df <- table_df %>%
    dplyr::select(.data$indicator, .data$series, {{ sa4 }}, dplyr::everything())

  table_df %>%
    rename(
      region = 3,
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
          minWidth = 30
        ),
        region = reactable::colDef(
          name = names(table_df)[3],
          align = "center",
          minWidth = 40,
          headerStyle = col_header_style
        ),
        aggregate = reactable::colDef(
          name = names(table_df)[4],
          align = "center",
          minWidth = 40,
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
}

viz_reg_melvic_line <- function(data = filter_dash_data(c(
                                  "A84600144J",
                                  "A84600078W",
                                  "A84595516F",
                                  "A84595471L"
                                ),
                                df = dash_data
                                ) %>%
                                  dplyr::group_by(series_id) %>%
                                  dplyr::mutate(value = slider::slide_mean(.data$value, before = 2, complete = TRUE)) %>%
                                  dplyr::filter(!is.na(value))) {
  latest <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$date == max(.data$date),
      .data$indicator == "Unemployment rate"
    ) %>%
    dplyr::select(.data$value, .data$gcc_restofstate) %>%
    dplyr::mutate(value = paste0(round2(value, 1), " per cent")) %>%
    tidyr::spread(key = .data$gcc_restofstate, value = value)


  title <- paste0(
    "The unemployment rate in Greater Melbourne was ",
    latest$`Greater Melbourne`,
    " while the rate in the rest of Victoria was ",
    latest$`Rest of Vic.`,
    " in ",
    format(max(data$date), "%B %Y")
  )

  data <- data %>%
    mutate(gcc_restofstate = gsub("Melbourne", "Melb", .data$gcc_restofstate,
      fixed = TRUE
    ))

  max_date <- data %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    mutate(label = paste0(
      stringr::str_wrap(.data$gcc_restofstate, 9),
      "\n",
      round2(.data$value, 1)
    ))

  days_in_data <- as.numeric(max(data$date) - min(data$date))

  data %>%
    dplyr::mutate(tooltip = paste0(
      .data$gcc_restofstate, "\n",
      format(.data$date, "%B %Y"),
      "\n",
      round2(.data$value, 1)
    )) %>%
    ggplot(aes(x = date, y = value, col = gcc_restofstate)) +
    geom_line() +
    ggiraph::geom_point_interactive(aes(tooltip = .data$tooltip),
      size = 3,
      colour = "white",
      alpha = 0.01
    ) +
    geom_point(
      data = max_date,
      fill = "white",
      stroke = 1.5,
      size = 2.5,
      shape = 21
    ) +
    ggrepel::geom_label_repel(
      data = max_date,
      aes(label = label),
      hjust = 0,
      nudge_x = days_in_data * 0.05,
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
    facet_wrap(~indicator, scales = "free_y") +
    djprtheme::theme_djpr() +
    djpr_colour_manual(2) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(4),
      labels = function(x) paste0(x, "%")
    ) +
    scale_x_date(
      expand = expansion(
        add = c(0, days_in_data * 0.25)
      ),
      date_labels = "%b\n%Y"
    ) +
    coord_cartesian(clip = "off") +
    theme(
      axis.title = element_blank(),
      panel.spacing = unit(1.5, "lines")
    ) +
    labs(
      title = title,
      subtitle = "Employment to population ratio and unemployment rate and in Greater Melbourne and the rest of Victoria",
      caption = paste0(caption_lfs_det_m(), " Data not seasonally adjusted. Smoothed using a 3 month rolling average.")
    )
}

viz_reg_emp_regionstates_sincecovid_line <- function(data = filter_dash_data(c("A84600075R",
                                                                               "A84599625R",
                                                                               "A84599781T",
                                                                               "A84599607K",
                                                                               "A84600243R",
                                                                               "A84599715V",
                                                                               "A84599631K"
                                                                               ),
                                      df = dash_data
                                      ) %>%
                                      dplyr::group_by(.data$series_id) %>%
                                      dplyr::mutate(
                                      value = slider::slide_mean(.data$value, before = 2, complete = TRUE)) %>%
                                      dplyr::filter(date >= as.Date("2020-01-01"))) {

  df <- data %>%
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
        TRUE ~ .data$state)
    ) %>%
    dplyr::mutate(
      state_group = dplyr::if_else(
        .data$state %in% c(
        "Reg. Vic", "Reg. NSW"
      ),
      .data$state,
      "Other")
    )

  df <- df %>%
    dplyr::group_by(.data$state) %>%
    dplyr::mutate(value = 100 * ((.data$value /
                                  .data$value[.data$date == as.Date("2020-03-01")]) - 1))

  latest <- df %>%
    dplyr::select(.data$date, .data$state, .data$value) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    ungroup() %>%
    dplyr::mutate(rank = dplyr::min_rank(-.data$value))

  vic_rank <- latest$rank[latest$state == "Reg. Vic"]
  vic_level <- paste0(round2(latest$value[latest$state == "Reg. Vic"], 1), "%")

  title <- dplyr::case_when(
    vic_rank == 1 ~ paste0(
      "Employment in regional Victoria changed by ", vic_level,
      " since March 2020 and is the highest of any Australian regional area in ",
      format(max(data$date), "%B %Y")),
    vic_rank == 2 ~ paste0(
      "Employment in regional Victoria changed by ", vic_level,
      " since March 2020 and is the second highest of any Australian regional area in ",
      format(max(data$date), "%B %Y")),
    vic_rank == 3 ~ paste0(
      "Employment in regional Victoria changed by ", vic_level,
      " since March 2020 and is the third highest of any Australian regional area in ",
      format(max(data$date), "%B %Y")),
    vic_rank == 4 ~ paste0(
      "Employment in regional Victoria changed by ", vic_level,
      " since March 2020 and is the fourth highest of any Australian regional area in ",
      format(max(data$date), "%B %Y")),
    vic_rank == 5 ~ paste0(
      "Employment in regional Victoria changed by ", vic_level,
      " since March 2020 and is the fifth highest of any Australian regional area in ",
      format(max(data$date), "%B %Y")),
    vic_rank == 6 ~ paste0(
      "Employment in regional Victoria changed by ", vic_level,
      " since March 2020 and is the second lowest of any Australian regional area in ",
      format(max(data$date), "%B %Y")),
    vic_rank == 7 ~ paste0(
      "Employment in regional Victoria changed by ", vic_level,
      " since March 2020 and is the lowest of any Australian regional area in ",
      format(max(data$date), "%B %Y")),
    )

  df %>%
    djpr_ts_linechart(
      col_var = .data$state,
      label_num = paste0(round(.data$value, 1), "%"),
      hline = 0
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(5),
      labels = function(x) paste0(x, "%")
    ) +
    scale_colour_manual(values = c(
      "Reg. Vic" = djprtheme::djpr_royal_blue,
      "Reg. NSW" = djprtheme::djpr_green,
      "Other" = "grey87"
    )) +
    labs(
      title = title,
      subtitle = "Cumulative change in employment for regional states and territories of Australia since March 2020",
      caption = paste0(caption_lfs_det_m(), "Data smoothed using a 3 month rolling average.")
    )

}

viz_reg_regionstates_dot <- function(data = filter_dash_data(c("A84599628W",
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
                                                                   "A84599612C"),
                                                             df = dash_data),
                                     selected_indicator = "unemp_rate")
{

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
    dplyr::mutate(series = gsub(";.*", "", series),
                  series = gsub(">> Rest of ", "Reg. ", series),
                  series = dplyr::if_else(grepl("Northern Territory", series),
                                          "Reg. NT",
                                          series),
                  series = stringr::str_trim(series)
    )

  # 3 month average
  df <- df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(
      value = slider::slide_mean(.data$value, before = 2, complete = TRUE)) %>%
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
    dplyr::filter(.data$date == max(.data$date),
                  .data$series == "Reg. Vic.") %>%
    dplyr::select(.data$series, .data$value, .data$date) %>%
    tidyr::pivot_wider(names_from = .data$series, values_from = .data$value)

  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "unemployment rate",
    selected_indicator == "part_rate" ~ "participation rate",
    selected_indicator == "emp_pop" ~ "employment to population ratio",
    TRUE ~ NA_character_)

  title <- paste0(
    "The ", indic_long,
    " in regional Victoria was ",
    round2(latest_values$`Reg. Vic.`, 1),
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
        .data$date,
        "\n",
        round2(.data$value, 1)
      ))
    ) +
    ggrepel::geom_text_repel(
      data = df %>%
        dplyr::filter(.data$series == "Reg. Vic."),
      aes(label = format(.data$date, "%b %Y")),
      size = 14 / .pt,
      direction = "y",
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
    labs(title = title,
         subtitle = paste0(tools::toTitleCase(indic_long), " in regional areas of Australia"),
         caption = paste0(caption_lfs(), "Data smoothed using a 3 month rolling average."),
         y = paste0("", indic_long))
}

viz_reg_regionstates_bar <- function(data = filter_dash_data(c("15-24_employed_rest of nsw",
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
                                                                "55+_unemployed_rest of wa"),
                                                             df = dash_data),
                                     selected_indicator = "unemp_rate")
{

  # to be part of second region focus box: unemp rate, part rate and emp to pop ratio
  # 3 bar charts to compare regional vic with other regional areas in Australia
  # one bar chart for each age class

  # apply 3 month smoothing & reduce data to only most recent date
  df <- data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::mutate(value = slider::slide_mean(value, before = 2, complete = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, age, gcc_restofstate, indicator, value) %>%
    dplyr::filter(.data$date == max(.data$date))

  # calculate participation, unemployment rate and employment to pop ratio for each regional area
  df <- df %>%
    tidyr::pivot_wider(names_from = .data$indicator, values_from = .data$value) %>%
    dplyr::mutate(partrate = ((Employed + Unemployed) / (Employed + Unemployed + NILF))) %>%
    dplyr::mutate(unemprate = (Unemployed / (Employed + Unemployed))) %>%
    dplyr::mutate(emppop = (Employed / (Employed + Unemployed + NILF)))

  # depending on selected_indicator, chose measure to be calculated - workaround for now
  df <- df %>%
    dplyr::select(.data$date, .data$age, .data$gcc_restofstate, .data$unemprate) %>%
    dplyr::rename(value = .data$unemprate)

  # df_partrate <- df %>%
  #   dplyr::select(.data$date, .data$age, .data$gcc_restofstate, .data$partrate) %>%
  #   dplyr::rename(value = .data$partrate)
  #
  # df_emppop <- df %>%
  #   dplyr::select(.data$date, .data$age, .data$gcc_restofstate, .data$emppop) %>%
  #   dplyr::rename(value = .data$emppop)

  # this would be smoother but can't get it to work
  # df <- df %>%
  #   dplyr::select(.data$date, .data$age, .data$gcc_restofstate, .data$unemprate, .data$partrate, .data$emppop) %>%
  #   dplyr::mutate(
  #     indic_long = dplyr::case_when(
  #       selected_indicator == "unemp_rate" ~ "unemployment rate",
  #       selected_indicator == "part_rate" ~ "participation rate",
  #       selected_indicator == "emp_pop" ~ "employment to population ratio",
  #       TRUE ~ NA_character_
  #     )
  #   )
  #
  # df <- df %>%
  #   dplyr::mutate(
  #     value = dplyr::case_when(
  #       df$indic_long == "unemployment rate" ~ unemprate,
  #       df$indic_long == "participation rate" ~ partrate,
  #       df$indic_long == "employment to population ratio" ~ emppop,
  #       TRUE ~ NA_character_
  #     )
  #   )

  # remove spaces for gcc_restofstate IDs
  df <- df %>%
    dplyr::mutate(gcc_restofstate = gsub("Rest of ", "Reg", gcc_restofstate),
                  gcc_restofstate = stringr::str_trim(gcc_restofstate)
    )

  # calculate rates for regional Australia - unfinished
  df_wide <- df %>%
    tidyr::spread(key = .data$gcc_restofstate, value = .data$value) %>%
    dplyr::mutate(
      RegAus = .data$RegNSW +
        .data$RegNT +
        .data$RegQld +
        .data$RegSA +
        .data$RegTas. +
        .data$RegVic. +
        .data$RegWA
      )

  # getting it into a long df again, with 'gcc_restofstate' and value for each date and age - unfinished
  df_long <- df_wide %>%
    tidyr::gather(
      key = "date", value = "value", -.data$age
    ) %>%
    dplyr::mutate(indicator = factor(.data$indicator,
                                     levels = c(
                                       "Regional Australia"
                                     )))





  indic_long <- dplyr::case_when(
    selected_indicator == "unemp_rate" ~ "unemployment rate",
    selected_indicator == "part_rate" ~ "participation rate",
    selected_indicator == "emp_pop" ~ "employment to population ratio",
    TRUE ~ NA_character_)

  title <- paste0(
    "The ", indic_long,
    " in regional areas for different age classes in ", format(max(data$date), "%B %Y")
  )

  df <- df %>%
    dplyr::mutate(fill_col = dplyr::if_else(
      .data$state %in% c("Vic", "Aus"), .data$state, "Other"
    ))

  df_15 <- df %>%
    dplyr::select(.data$age == `15-24`)

  df_25 <- df %>%
    dplyr::select(.data$age ==`25-54`)

  df_55 <- df %>%
    dplyr::select(.data$age ==`55+`)

  # replicate that for the other age classes, once the code works
  df_15 %>%
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
      subtitle = "subtitle goes here",
      caption = caption_lfs_det_m()
    )

}
