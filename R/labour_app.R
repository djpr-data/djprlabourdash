#' @import djprshiny
#' @import djprdashdata
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import djprtheme
#' @importFrom rlang .data .env

labour_server <- function(input, output, session) {
  # Load data and create persistent objects ----

  Sys.setenv("R_DJPRLABOURDASH_TABLEDEST" = "dashboard")
  dash_data <<- get_dash_data()
  dash_data_updated <<- attr(dash_data, "date_updated")
  if (shiny::isRunning()) {
    shinyjs::hide("loading_page")
    shinyjs::show("main_content")
  }

  series_latestdates <- dash_data %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::pull(.data$date)

  plt_change <- reactive(input$plt_change) %>%
    debounce(2)

  # Overview ------
  # Overview: footnote and main table ----
  # Indicators -----
  # Indicators: Employment ----
  # Sex -----
  # Age ----
  # Long-term unemployment ------
  # Aboriginal ------
  # Disability ------
  # Migration ------
  # Regions ------
  # Victorian Regions -------
  # Australian Regions -----------
  # Industries ------
  # Links to pages -----

  app <- function(...) {
    djprtheme::djpr_use_fonts()
    flextable::set_flextable_defaults(
      font.family = getOption("djprtheme.base_font_family")
    )

    jobs_dash_cache <- cachem::cache_disk(
      dir = file.path(".", "app-cache")
    )

    shinyOptions(
      cache = jobs_dash_cache
    )

    shiny::shinyApp(labour_ui(), labour_server)
  }

}
