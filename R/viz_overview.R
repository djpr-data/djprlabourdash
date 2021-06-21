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
