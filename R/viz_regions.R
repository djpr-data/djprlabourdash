#' Function to create the graphs for the 'Regions' subpage on the dashboard.
#' @param data the dataframe containing data to visualise
#' @examples
#' \dontrun{
#'
#' dash_data <- load_dash_data()
#'
#' #for 'viz_emp_regions_sincecovid':
#' data <- filter_dash_data(c("A84600141A",
#'                            "A84600075R")) %>%
#'         dplyr::filter(date >= as.Date("2020-01-01"))
#'
#' #for 'viz_reg_unemprate_multiline':
#' data <- filter_dash_data(c("A84600253V",
#'                            "A84599659L",
#'                            "A84600019W",
#'                            "A84600187J",
#'                            "A84599557X",
#'                            "A84600115W",
#'                            "A84599851L",
#'                            "A84599923L",
#'                            "A84600025T",
#'                            "A84600193C",
#'                            "A84599665J",
#'                            "A84600031L",
#'                            "A84599671C",
#'                            "A84599677T",
#'                            "A84599683L",
#'                            "A84599929A",
#'                            "A84600121T",
#'                            "A84600037A"))
#'}


map_unemprate_vic <- function(data = filter_dash_data(c("A84600253V",
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
                                                        "A84600037A")) %>%
                                group_by(series_id) %>%
                                mutate(value = zoo::rollmeanr(value, 3, fill = NA)) %>%
                                dplyr::filter(.data$date == max(.data$date)),
                              title = "") {

  # Call SA4 shape file, but only load Victoria and exclude 'weird' areas (migratory and other one)
  sa4_shp <- absmapsdata::sa42016 %>%
    dplyr::filter(.data$state_name_2016 == "Victoria") %>%
    dplyr::filter(.data$sa4_code_2016 < 297)

  # Fix issue with different naming for North West region in Victoria
  data <- data %>%
    dplyr::mutate(
      sa4 = dplyr::if_else(.data$sa4 == "Victoria - North West",
                                 "North West",
                                 .data$sa4))

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
    sf::st_transform('+proj=longlat +datum=WGS84')

  metro_outline <- mapdata %>%
    dplyr::filter(.data$sa4_name_2016 %in% metro_boundary_sa4) %>%
    dplyr::summarise(areasqkm_2016 = sum(.data$areasqkm_2016))



  # Produce dynamic map, all of Victoria ----
  # Ignore warning message:
  # //sf layer has inconsistent datum (+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs).
  # //Need '+proj=longlat +datum=WGS84'
  map <- mapdata %>%
    leaflet::leaflet() %>%
    leaflet::setView(
      lng = 145.4657, lat = -36.41472, # coordinates of map at first view
      zoom = 6
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
    ) %>%
    # thickness of metro outline
    leaflet.extras::setMapWidgetStyle(list(background = "white")) # background colour

  # Display dynamic map: can zoom in, zoom out and hover over regions displaying distinct data----
  map
}

# Comparison of change in employment since Mar-20 in Greater Melbourne region and Rest of Victoria
title_reg_emp_regions_sincecovid_line <- function(data) {

  current <- data %>%
    dplyr::group_by(.data$series) %>%
    dplyr::mutate(value = 100 * ((.data$value / .data$value[date == as.Date("2020-03-01")]) -1)
    ) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$gcc_restofstate, .data$value) %>%
    tidyr::spread(key = .data$gcc_restofstate, value = .data$value)

  diff <- current$`Greater Melbourne` - current$`Rest of Vic.`
  case_when(abs(diff) < 0.1 ~
              "Employment in Greater Melbourne has caught up with the rest of Victoria",
            sign(diff) == -1 ~
              "Employment in Greater Melbourne has not kept pace with the rest of Victoria",
            TRUE ~ "Employment in Greater Melbourne has grown faster than the rest of Victoria")
}

viz_reg_emp_regions_sincecovid_line <- function(data = filter_dash_data(c("A84600141A",
                                                                 "A84600075R")) %>%
                                         dplyr::group_by(series_id) %>%
                                         dplyr::mutate(value = zoo::rollmeanr(value, 3, fill = NA)) %>%
                                         dplyr::filter(date >= as.Date("2020-01-01")),
                                         title = title_reg_emp_regions_sincecovid_line(data = data)) {

  df <- data %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(value = 100 * ((value / value[date == as.Date("2020-03-01")]) -1)
                  )

  df %>%
    djpr_ts_linechart(col_var = gcc_restofstate,
                      label_num = paste0(round(.data$value, 1), "%"),
                      y_labels = function(x) paste0(x, "%"),
                      hline = 0) +
    labs(title = title,
         subtitle = "Cumulative change in employment (%) in Greater Melbourne and the rest of Victoria since March 2020",
         caption = paste0("Source: ABS Labour Force.",
                          "Note: Latest data is from ",
                          format(max(df$date), "%B %Y"),
                          ". Not seasonally adjusted. Data smoothed using a 3 month rolling average."))
}

title_reg_unemprate_multiline <- function(data) {

  highest_current_ur <- data %>%
    dplyr::filter(.data$date == max(data$date)) %>%
    dplyr::filter(.data$value == max(data$value)) %>%
    dplyr::pull(.data$sa4)

  highest_current_ur <- data$sa4[data$value == max(data$value)]
  paste0(highest_current_ur, " has the highest unemployment rate in Victoria")
}

viz_reg_unemprate_multiline <- function(data = filter_dash_data(c("A84600253V",
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
                                                                  "A84600037A")) %>%
                                          dplyr::group_by(series_id) %>%
                                          dplyr::mutate(value = zoo::rollmeanr(value, 3, fill = NA)) %>%
                                          dplyr::filter(!is.na(value)),
                                        title = title_reg_unemprate_multiline(data = data)) {

  data <- data %>%
    dplyr::mutate(tooltip = paste0(.data$sa4, "\n", format(.data$date, "%b %Y"),
                                   "\n", round2(.data$value, 1), "%"),
                  sa4 = gsub(" and South ", " & S. ", .data$sa4, fixed = TRUE))


  max_y <- max(data$value)
  mid_x <- median(data$date)

  data <- data %>%
    dplyr::mutate(sa4 = dplyr::if_else(.data$sa4 == "", "Victoria", .data$sa4),
                  is_vic = dplyr::if_else(.data$sa4 == "Victoria", TRUE, FALSE))


  vic <- data %>%
    filter(sa4 == "Victoria") %>%
    select(-sa4)

  facet_labels <- data %>%
    group_by(sa4, is_vic) %>%
    summarise() %>%
    mutate(x = mid_x,
           y = max_y)

  data$sa4 <- factor(data$sa4,
                     levels = c("Victoria", sort(unique(data$sa4[data$sa4 != "Victoria"]))))

  data %>%
    ggplot(aes(x = date, y = value, col = is_vic)) +
    geom_line(aes(group = sa4)) +
    geom_label(data = facet_labels,
              aes(label = stringr::str_wrap(sa4, 11),
                  y = max_y,
                  x = mid_x),
              nudge_y = 0.1,
              lineheight = 0.85,
              label.padding = unit(0.05, "lines"),
              label.size = 0,
              size = 12 / .pt) +
    geom_line(data = vic) +
    ggiraph::geom_point_interactive(aes(tooltip = .data$tooltip),
                                    size = 3,
                                    colour = "white",
                                    alpha = 0.01) +
    facet_wrap(~factor(sa4),
               ncol = 6) +
    scale_colour_manual(values = c(`TRUE` = "#2A6FA2",
                                   `FALSE` = "#62BB46")) +
    djprtheme::theme_djpr() +
    coord_cartesian(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                       limits = function(limits) c(0, limits[2]),
                       breaks = function(limits) c(0,
                                                   min(c(limits[2],
                                                         10))),
                       labels = function(x) paste0(round2(x), "%")
                       ) +
    scale_x_date(date_labels = "%Y",
                 breaks = scales::breaks_pretty(n = 3)
                 ) +
    theme(axis.title = element_blank(),
          strip.text = element_blank()) +
    labs(title = title,
         subtitle = "Unemployment rate by region (SA4), per cent",
         caption = paste0("Source: ABS Labour Force. Note: Latest data is from ",
                          format(max(data$date), "%B %Y"),
                          ". Not seasonally adjusted. Data smoothed using a 3 month rolling average."))

}

viz_reg_unemprate_bar <- function(data = filter_dash_data(c("A84600253V",
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
                                                            "A84600037A"),
                                                          df = dash_data) %>%
                                    dplyr::group_by(series_id) %>%
                                    dplyr::mutate(value = zoo::rollmeanr(value, 3, fill = NA)) %>%
                                    dplyr::filter(.data$date == max(.data$date)),
                                    title = "") {

  data <- data %>%
    dplyr::filter(.data$sa4 != "") %>%
    dplyr::mutate(sa4 = dplyr::if_else(grepl("Warrnambool", .data$sa4),
                                       "Warrnambool & S. West",
                                       .data$sa4))

  data %>%
    ggplot(aes(x = reorder(sa4, value),
               y = value)) +
    geom_col(
      col = "grey85",
      aes(fill = -value)) +
    geom_text(nudge_y = 0.1,
              aes(label = round(value, 1)),
              colour = "black",
              hjust = 0,
              size = 12 / .pt) +
    coord_flip(clip = "off") +
    scale_fill_distiller(palette = "Blues") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    djprtheme::theme_djpr(flipped = TRUE) +
    theme(axis.title.x = element_blank(),
          panel.grid = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_blank()) +
    labs(title = title)

}
