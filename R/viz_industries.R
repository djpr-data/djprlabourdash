#' Function to create the graphs for the 'Regions' subpage on the dashboard.
#' @param data the dataframe containing data to visualise
#' @examples
#' \dontrun{
#'
#' dash_data <- load_dash_data()
#'
#' # for 'viz_industries_empchange_sincecovid_bar':
#' data <- filter_dash_data(c("A84601680F",
#'                              "A84601683L",
#'                              "A84601686V",
#'                              "A84601665J",
#'                              "A84601704L",
#'                              "A84601707V",
#'                              "A84601710J",
#'                              "A84601638A",
#'                              "A84601653X",
#'                              "A84601689A",
#'                              "A84601656F",
#'                              "A84601713R",
#'                              "A84601668R",
#'                              "A84601695W",
#'                              "A84601698C",
#'                              "A84601650T",
#'                              "A84601671C",
#'                              "A84601641R",
#'                              "A84601716W",
#'                              "A84601662A")
#'
#' ) %>%
#'   dplyr::filter(date >= as.Date("2020-01-01"))
#' }
#' @import djprtheme

viz_industries_empchange_sincecovid_bar <- function(data = filter_dash_data(c("A84601680F",
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
                                                             "A84601662A"),
                                                             df = dash_data) %>%
        dplyr::group_by(.data$series) %>%
        dplyr::mutate(value = 100 * ((.data$value / .data$value[date == as.Date("2020-02-01")]) - 1)))
{
  #reduce to only latest month (indexing already done above in data input into function)                                {
  data <- data %>%
    dplyr::group_by(.data$series) %>%
    dplyr::filter(.data$date == max(.data$date))

  #add entry for data$industry for Victoria; employed total
  data <- data %>%
    dplyr::mutate(
      industry = dplyr::if_else(.data$industry == "",
                           "Victoria, all industries",
                           .data$industry
      )
    )

  #draw bar chart for all 19 industries plus Vic total
  data %>%
    ggplot(aes(
      x = reorder(industry, value),
      y = value
    )) +
    geom_col(
      col = "grey85",
      aes(fill = -value)
    ) +
    geom_text(
      nudge_y = 0.1,
      aes(label = round(value, 1)),
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
    labs(title = title)

}

viz_industries_emp_table <- function(data = filter_dash_data(c("A84601680F",
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
                                                             df = dash_data)))
{




}

