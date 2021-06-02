#' Function to create the graphs for the 'Industries' subpage on the dashboard.
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
                           .data$industry)
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
                                                                "A84601664F",
                                                             df = dash_data)),
                                     chosen_industry = "Agriculture, Forestry and Fishing")
{

  latest_date <- format(max(data$date), "%b %Y")

  #add entry for data$industry for "Victoria, all industries" where ""
  data <- data %>%
    dplyr::mutate(
      industry = dplyr::if_else(.data$industry == "",
                                "Victoria, all industries",
                                .data$industry)
    )

  #filter out the chosen industry and vic_total
  data <- data %>%
    group_by(indicator) %>%
    dplyr::filter(.data$industry %in% c("Victoria, all industries", .env$chosen_industry))

  table_df <- data %>%
    dplyr::group_by(industry, indicator) %>%
    dplyr::mutate(
      d_quarter = 100 * ((value / dplyr::lag(value,1)) - 1),
      d_year = 100 * ((value / dplyr::lag(value,4)) - 1)
    ) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(
      .data$indicator, .data$value, .data$industry,
      .data$d_quarter, .data$d_year
    ) %>%
    dplyr::ungroup()

  table_df <- table_df %>%
    dplyr::mutate(across(
      c(value, d_quarter, d_year),
      ~ round2(.x, 1)
    ))

  table_df <- table_df %>%
    dplyr::mutate(across(
      c(value, d_quarter, d_year),
      ~ round2(.x, 1)
    )) %>%
    dplyr::mutate(
      value = scales::comma(value * 1000),
      d_quarter = paste0(d_quarter, "%"),
      d_year = paste0(d_year, "%"))

  table_df <- table_df %>%
    dplyr::rename({{ latest_date }} := value,
                  `Change over quarter` = d_quarter,
                  `Change over year` = d_year
    )

  table_df <- table_df %>%
    tidyr::gather(
      key = series, value = value,
      -indicator, -industry
    ) %>%
    tidyr::spread(key = industry, value = value)

  table_df <- table_df %>%
    dplyr::group_by(.data$indicator) %>%
    mutate(order = dplyr::case_when(
      series == "Change over quarter" ~ 2,
      series == "Change over year" ~ 3,
      TRUE ~ 1
    )) %>%
    dplyr::arrange(desc(indicator), order) %>%
    dplyr::select(-order)

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


