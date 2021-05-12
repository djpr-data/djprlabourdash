#' Function to create the graphs for the 'Regions' subpage on the dashboard.
#' @param data the dataframe containing data to visualise
#' @examples
#' \dontrun{
#'
#' data <- load_dash_data()
#' #for 'map_unemprate_vic'
#' ids <- c("A84600253V",
#'         "A84600145K",
#'         "A84599659L",
#'         "A84600019W",
#'         "A84600187J",
#'         "A84599557X",
#'         "A84600115W",
#'         "A84599851L",
#'         "A84599923L",
#'         "A84600025T",
#'         "A84600193C",
#'         "A84600079X",
#'         "A84599665J",
#'         "A84600031L",
#'         "A84599671C",
#'         "A84599677T",
#'         "A84599683L",
#'         "A84599929A",
#'         "A84600121T",
#'         "A84600037A")
#'
#' data <- data %>%
#'  dplyr::filter(series_id %in% ids) %>%
#'  janitor::clean_names() %>%
#'  tidyr::unnest(cols = dplyr::everything()) %>%
#'  dplyr::filter(.data$date == max(.data$date))
#'
#' #for 'viz_emp_regions_sincecovid':
#' data <- filter_dash_data(c("A84600141A",
#'                            "A84600075R"), data) %>%
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
#'                            "A84600037A", data)


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
                                                        "A84600037A"),
                                                      df = dash_data) %>%
                                dplyr::group_by(.data$series) %>%
                                dplyr::filter(.data$date == max(.data$date)),
                              title = "") {

  # Call SA4 shape file, but only load Victoria and exclude 'weird' areas (migratory and other one)
  sa4_shp <- absmapsdata::sa42016 %>%
    dplyr::filter(.data$state_name_2016 == "Victoria") %>%
    dplyr::filter(.data$sa4_code_2016 < 297)

  # Fix issue with different naming for North West region in Victoria
  data$sa4[data$sa4 == "Victoria - North West"] <- "North West"

  # Join shape file with data to create mapdata ----
  mapdata <- sa4_shp %>%
    dplyr::left_join(data, by = c("sa4_name_2016" = "sa4"))

  # Create colour bins
  pal <- leaflet::colorBin("Blues", mapdata$value, 5) # last object is number of bins

  # Create metro boundary (Greater Melbourne) ----
  metro_boundary_sa4 <- c(
    "Melbourne - Inner", "Melbourne - Inner East", "Melbourne - Inner South", "Melbourne - North East",
    "Melbourne - North West", "Melbourne - Outer East", "Melbourne - South East", "Melbourne - West",
    "Mornington Peninsula"
  )

  metro_outline <- mapdata %>%
    dplyr::filter(.data$sa4_name_2016 %in% metro_boundary_sa4) %>%
    dplyr::summarise(areasqkm_2016 = sum(.data$areasqkm_2016))

  # Produce dynamic map, all of Victoria ----
  # Ignore warning message:
  # //sf layer has inconsistent datum (+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs).
  # //Need '+proj=longlat +datum=WGS84'
  map <- leaflet::leaflet(data = mapdata) %>%
    leaflet::setView(
      lng = 145.4657, lat = -36.41472, # coordinates of map at first view
      zoom = 7
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
        "<strong>%s</strong><br/>Unemployment rate: %.2f", # label title, strong = bold, %.2f = 2 dec points
        mapdata$sa4_name_2016, # region name displayed in label
        mapdata$value
      ) %>% # eco data displayed in label
        lapply(htmltools::HTML),
      labelOptions = leaflet::labelOptions( # label options
        style = list(
          "font-weight" = "normal", # "bold" makes it so
          padding = "3px 8px"
        ),
        textsize = "15px", # text size of label (15 px seems good size)
        noHide = FALSE, # TRUE makes labels permanently visible (messy)
        direction = "auto"
      ) # text box flips from side to side as needed
    ) %>%
    leaflet::addLegend(
      position = "bottomright", # options: topright, bottomleft etc.
      pal = pal, # colour palette as defined
      values = mapdata$value, # fill data
      labFormat = leaflet::labelFormat(transform = identity),
      title = "Unemployment Rate (%)", # label title
      opacity = 1
    ) %>%
    # label opacity
    leaflet::addPolygons(
      data = metro_outline, #
      fill = F,
      stroke = T,
      color = "black",
      weight = 2
    ) %>%
    # thickness of metro outline
    leaflet.extras::setMapWidgetStyle(list(background = "white")) # background colour

  # Display dynamic map: can zoom in, zoom out and hover over regions displaying distinct data----
  map
}

# Comparison of change in employment since Mar-20 in Greater Melbourne region and Rest of Victoria
viz_emp_regions_sincecovid <- function(data = filter_dash_data(c("A84600141A",
                                                                 "A84600075R"), df = dash_data) %>%
                                       dplyr::filter(date >= as.Date("2020-01-01")), title = "") {

  df <- data %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(value = value / value[date == as.Date("2020-03-01")])

  df %>%
    djpr_ts_linechart() +
    labs(title = title,
         subtitle = "Change in employment (%) in Victorian regions since Mar-20",
         caption = "Source: ABS Labour Force.")
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
                                                                  "A84600037A", df = dash_data, title = ""))) {

  vic <- data %>%
    filter(sa4 == "") %>%
    select(-sa4)

  data %>%
    dplyr::filter(sa4 != "") %>%
    ggplot(aes(x = date, y = value, col = sa4)) +
    geom_line() +
    geom_line(data = vic, col = "black") +
    facet_wrap(~sa4) +
    djprtheme::theme_djpr() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       limits = c(0, 16)) +
    theme(axis.title = element_blank()) +
    labs(title = title,
         subtitle = "Unemployment rate (%) by region (SA4)",
         caption = "Source: ABS Labour Force.")

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
                                    dplyr::group_by(.data$series) %>%
                                    dplyr::filter(.data$date == max(.data$date)),
                                  title = "") {

  data %>%
    dplyr::filter(.data$sa4 != "") %>%
    ggplot(aes(x = reorder(sa4, value),
               y = value)) +
    ggiraph::geom_col_interactive(fill = djprtheme::djpr_pal(1),
                                  aes(tooltip = round(value, 1))) +
    geom_text(nudge_y = -0.5,
              aes(label = round(value, 1)),
              colour = "white",
              size = 14 / .pt) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    djprtheme::theme_djpr(flipped = TRUE) +
    theme(axis.title.x = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_blank()) +
    labs(title = "",
         subtitle = "",
         caption = "Source: ABS Labour Force.")

}
