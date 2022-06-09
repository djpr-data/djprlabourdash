#
# viz_overview_ur <- function(data = filter_dash_data("A84423354L")) {
#   data %>%
#     dplyr::slice_tail(n = 6) %>%
#     ggplot(aes(x = date, y = value, data_id = date)) +
#     ggiraph::geom_col_interactive(fill = "#BCD3EF") +
#     theme_void()
# }
# #
# # djpr_girafe(ggobj = last_plot(),
# #             options = opts_hover(css = ggiraph::girafe_css("fill:#2A6FA2; stroke:#1279BF;"))
# #             )
# #
#
# viz_overview_illustration <- function(){
#
#   df <- data_reg_unemp_emppop_partrate_vic() |>
#     dplyr::mutate(
#       sa4 = dplyr::if_else(.data$sa4 == "Victoria - North West",
#                            "North West",
#                            .data$sa4
#       )
#     )
#
#   mapdata <- sa42016 %>%
#     dplyr::filter(.data$state_name_2016 == "Victoria") %>%
#     dplyr::filter(.data$sa4_code_2016 < 297) |>
#     dplyr::left_join(df, by = c("sa4_name_2016" = "sa4"))
#
#   mp <- ggplot(mapdata) +
#     geom_sf(aes(fill = value), size = 0) +
#     scale_fill_viridis_c() +
#     #theme_void() +
#     theme(legend.position = c(0.1, 0.75)) +
#     theme(panel.background = element_rect(fill = '#201547', colour = '#201547'),
#           plot.background = element_rect(fill = '#201547', colour = '#201547'),
#           panel.grid = element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank(),
#           axis.text.y=element_blank(),
#           axis.ticks.y=element_blank())
#
#   filename = tempfile(fileext = '.svg')
#   ggsave(file=filename, plot=mp, width=10, height=8)
#   mp_imported <- magick::image_read_svg(filename, height = 1000)
#
#   g <- grid::rasterGrob(mp_imported, interpolate=TRUE)
#
#   bg_data <- dash_data |>
#     filter(series_id == 'A84423354L',
#            date >= '1995-01-01')
#
#   current <- bg_data |>
#     filter(date == max(date))
#
#
#   bg <- ggplot(bg_data) +
#     geom_area(aes(x = date, y = value), colour = '#201547', fill = '#201547') +
#     geom_line(aes(x = date, y = current$value), colour = 'red') +
#     geom_text(x = min(bg_data$date) + days(1450),
#               y = current$value - 0.3,
#               colour = 'red',
#               label = glue::glue('current unemployment {round2(current$value, 1)}%')) +
#     ggplot2::ylim(0, NA) +
#     theme_minimal() +
#     #annotation_custom(g, xmin=as.Date('1994-01-01'), xmax=as.Date('2018-01-01'), ymin=-1, ymax=9) +
#     labs(x = '', y = '') +
#     theme(panel.background = element_rect(fill = '#f1f1ef', colour = '#f1f1ef'),
#           plot.background = element_rect(fill = '#f1f1ef', colour = '#f1f1ef'),
#           panel.grid = element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank(),
#           axis.text.y=element_blank(),
#           axis.ticks.y=element_blank())
#
#
# }


map_overview <- function(...){

  # Aquire data
  data <- filter_dash_data(
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
      "A84599658K",
      "A84599660W",
      "A84600018V",
      "A84600020F",
      "A84600186F",
      "A84600188K",
      "A84599556W",
      "A84599558A",
      "A84600114V",
      "A84600116X",
      "A84599850K",
      "A84599852R",
      "A84599922K",
      "A84599924R",
      "A84600024R",
      "A84600026V",
      "A84600192A",
      "A84600194F",
      "A84599664F",
      "A84599666K",
      "A84600030K",
      "A84600032R",
      "A84599670A",
      "A84599672F",
      "A84599676R",
      "A84599678V",
      "A84599682K",
      "A84599684R",
      "A84599928X",
      "A84599930K",
      "A84600120R",
      "A84600122V",
      "A84600036X",
      "A84600038C"
    )
  ) %>%
    dplyr::select(date, indicator, sa4, value)

  # Fix issue with different naming for North West region in Victoria
  data <- data %>%
    dplyr::filter(date == max(date), indicator == "Unemployment rate") %>% # !!!!!!
    dplyr::mutate(
      sa4 = dplyr::if_else(
        .data$sa4 == "Victoria - North West",
        "North West",
        .data$sa4
      )
    )

  # Join polygons
  data <- sa42016 %>%
    dplyr::filter(.data$state_name_2016 == "Victoria") %>%
    dplyr::filter(.data$sa4_code_2016 < 297) %>%
    dplyr::left_join(data, by = c("sa4_name_2016" = "sa4"))

  # Generate bounding box
  bbox <- data %>%
    filter(date == max(date)) %>%
    sf::st_bbox() %>%
    as.vector()

  names(bbox) <- c("lng1", "lat1", "lng2", "lat2")

  # Generate palette
  pal <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::pull(value) %>%
    leaflet::colorNumeric(
      palette = "Blues",
      domain = c(min(.), max(.)),
      alpha = TRUE
    )

  # Metropolitan boundary
  metro_boundary <- c(
    "Melbourne - Inner", "Melbourne - Inner East", "Melbourne - Inner South", "Melbourne - North East",
    "Melbourne - North West", "Melbourne - Outer East", "Melbourne - South East", "Melbourne - West",
    "Mornington Peninsula"
  )

  metro_outline <- data %>%
    dplyr::filter(.data$sa4_name_2016 %in% metro_boundary) %>%
    dplyr::summarise(areasqkm_2016 = sum(.data$areasqkm_2016))

  # Generate map
  data %>%
    leaflet::leaflet(
      options = leaflet::leafletOptions(
        background         = "white",
        dragging           = FALSE,
        attributionControl = FALSE,
        zoomControl        = FALSE,
        minZoom            = 6.6,
        maxZoom            = 6.6
        )
      ) %>%
    leaflet::setMaxBounds(
      lng1 = bbox["lng1"],
      lat1 = bbox["lat1"],
      lng2 = bbox["lng2"],
      lat2 = bbox["lat2"]
    ) %>%
    # size of map at first view
    leaflet::addPolygons(
      color            = "white",           # colour of boundary lines, 'transparent' for no lines
      weight           = 1,                 # thickness of boundary lines
      fillColor        = ~ pal(data$value), # pre-defined above
      fillOpacity      = 1.0,               # strength of fill colour
      smoothFactor     = 0.5,               # smoothing between region
      stroke           = TRUE,
      highlightOptions = leaflet::highlightOptions(
        color        = "black", # boundary colour of region you hover over
        weight       = 2, # thickness of region boundary
        bringToFront = FALSE
      ), # FALSE = metro outline remains
      label            = sprintf(
        "<strong>%s</strong><br/>Unemployment: %.1f",
        data$sa4_name_2016, # region name displayed in label
        data$value
      ) %>% # eco data displayed in label
        lapply(shiny::HTML),
      labelOptions = leaflet::labelOptions( # label options
        style = list(
          "font-weight" = "normal", # "bold" makes it so
          padding = "3px 8px"
        ),
        textsize = "12px", # text size of label
        noHide = FALSE, # TRUE makes labels permanently visible (messy)
        direction = "auto"
      ) # text box flips from side to side as needed
    )


}
