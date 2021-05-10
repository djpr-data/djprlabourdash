#' Function to create the graphs for the 'Regions' subpage on the dashboard.
#'

map_unemprate_vic <- function(data) {

  # Call SA4 shape file, but only load Victoria and exclude 'weird' areas (migratory and other one)
  sa4_shp <- absmapsdata::sa42016 %>% dplyr::filter(state_name_2016 =="Victoria") %>% dplyr::filter(sa4_code_2016 < 297)

  # Fix issue with different naming for North West region in Victoria
  data$sa4[data$sa4 == "Victoria - North West"] <- "North West"

  # Join shape file with data to create mapdata ----
  mapdata <- sa4_shp %>%
    dplyr::left_join(data, by = c("sa4_name_2016" = "sa4"))

  # Create colour bins
  pal <- leaflet::colorBin("Blues", data$value, 5)           # last object is number of bins

  #Create metro boundary (Greater Melbourne) ----
#  metro_boundary_sa4 <- c(
#    "Melbourne - Inner","Melbourne - Inner East","Melbourne - Inner South","Melbourne - North East",
#    "Melbourne - North West","Melbourne - Outer East","Melbourne - South East","Melbourne - West",
#    "Mornington Peninsula")

#  metro_outline <- mapdata %>% dplyr::filter(sa4_name_2016 %in% metro_boundary_sa4) %>%
#    dplyr::summarise(areasqkm_2016 = sum(areasqkm_2016))

  # Produce dynamic map, all of Victoria ----
  # Ignore warning message:
  # //sf layer has inconsistent datum (+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs).
  # //Need '+proj=longlat +datum=WGS84'
  map <- leaflet::leaflet(data = mapdata) %>%
    leaflet::setView(lng = 145.4657, lat = -36.41472,   # coordinates of map at first view
            zoom = 7) %>%                      # size of map at first view
    leaflet::addPolygons(
      color = "grey",                          # colour of boundary lines, 'transparent' for no lines
      weight = 1,                              # thickness of boundary lines
      fillColor = ~pal(mapdata$value),       # pre-defined above
      fillOpacity = 1.0,                       # strength of fill colour
      smoothFactor = 0.5,                      # smoothing between region
      stroke = T,
      highlightOptions = leaflet::highlightOptions(     # to highlight regions as you hover over them
        color = "black",                       # boundary colour of region you hover over
        weight = 2,                            # thickness of region boundary
        bringToFront = FALSE),                 # FALSE = metro outline remains
      label = sprintf(                         # region label definition
        "<strong>%s</strong><br/>Unemployment rate: %g",   # label title, strong = bold
        mapdata$sa4_name_2016,                 # region name displayed in label
        mapdata$value) %>%                   # eco data displayed in label
        lapply(htmltools::HTML),
      labelOptions = leaflet::labelOptions(             # label options
        style = list(
          "font-weight" = "normal",            # "bold" makes it so
          padding = "3px 8px"),
        textsize = "15px",                     # text size of label (15 px seems good size)
        noHide = FALSE,                        # TRUE makes labels permanently visible (messy)
        direction = "auto")                    # text box flips from side to side as needed
    ) %>%
    leaflet::addLegend(position = "bottomright",        # options: topright, bottomleft etc.
              pal = pal,                       # colour palette as defined
              values = mapdata$value,        # fill data
              labFormat = leaflet::labelFormat(transform = identity),
              title = "Unemployment Rate (%)",        # label title
              opacity = 1)  %>%                # label opacity
    leaflet::addPolygons(data = metro_outline,          #
                fill = F,
                stroke = T,
                color = "black",
                weight = 2)  %>%               # thickness of metro outline
    leaflet.extras::setMapWidgetStyle(list(background= "white"))    # background colour

  # Display dynamic map: can zoom in, zoom out and hover over regions displaying distinct data----
  map

  # Export a static image of the map (into your working directory)----
  mapview::mapshot(map, file = "Victoria_SA4_unemprate.png")

  # Export the dynamic map with all functions as html page (into your working directory)----
  htmlwidgets::saveWidget(map, file = "Victoria_SA4_unemprate.html")

}
