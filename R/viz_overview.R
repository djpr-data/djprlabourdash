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

viz_overview_illustration <- function(){

  df <- data_reg_unemp_emppop_partrate_vic() |>
    dplyr::mutate(
      sa4 = dplyr::if_else(.data$sa4 == "Victoria - North West",
                           "North West",
                           .data$sa4
      )
    )

  mapdata <- sa42016 %>%
    dplyr::filter(.data$state_name_2016 == "Victoria") %>%
    dplyr::filter(.data$sa4_code_2016 < 297) |>
    dplyr::left_join(df, by = c("sa4_name_2016" = "sa4"))

  mp <- ggplot(mapdata) +
    geom_sf(aes(fill = value), size = 0) +
    scale_fill_viridis_c() +
    #theme_void() +
    theme(legend.position = c(0.1, 0.75)) +
    theme(panel.background = element_rect(fill = '#201547', colour = '#201547'),
          plot.background = element_rect(fill = '#201547', colour = '#201547'),
          panel.grid = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

  filename = tempfile(fileext = '.svg')
  ggsave(file=filename, plot=mp, width=10, height=8)
  mp_imported <- magick::image_read_svg(filename, height = 1000)

  g <- grid::rasterGrob(mp_imported, interpolate=TRUE)

  bg_data <- dash_data |>
    filter(series_id == 'A84423354L',
           date >= '1995-01-01')

  current <- bg_data |>
    filter(date == max(date))


  bg <- ggplot(bg_data) +
    geom_area(aes(x = date, y = value), colour = '#201547', fill = '#201547') +
    geom_line(aes(x = date, y = current$value), colour = 'red') +
    geom_text(x = min(bg_data$date) + days(1450),
              y = current$value - 0.3,
              colour = 'red',
              label = glue::glue('current unemployment {round2(current$value, 1)}%')) +
    ggplot2::ylim(0, NA) +
    theme_minimal() +
    #annotation_custom(g, xmin=as.Date('1994-01-01'), xmax=as.Date('2018-01-01'), ymin=-1, ymax=9) +
    labs(x = '', y = '') +
    theme(panel.background = element_rect(fill = '#f1f1ef', colour = '#f1f1ef'),
          plot.background = element_rect(fill = '#f1f1ef', colour = '#f1f1ef'),
          panel.grid = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())


}
