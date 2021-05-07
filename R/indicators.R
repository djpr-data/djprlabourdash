#' Function to make graph 'I-1-1', which is the first graph on the Indicators page
#' x = employed Victorians, seasonally adjusted '000
#' y = months, starting from

viz_emptotal_vic <- function(data) {

  data <- data %>%
    group_by(series) %>%
    mutate(value = 100 * ((value / value[date == as.Date("2020-03-01")]) -1) )

  data %>%
    ggplot(aes(x = date, y = value, col = series)) +
    geom_hline(yintercept = 0) +
    geom_line_interactive() +
    geom_point_interactive(size = 3,
                           colour = "white",
                           alpha = 0.01,
                           aes(tooltip = value)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    djprtheme::djpr_colour_manual(2) +
    djprtheme::theme_djpr() +
    theme(axis.title.x = element_blank() )

}


