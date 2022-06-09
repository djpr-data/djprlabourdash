#' @import djprshiny
#' @import djprdashdata
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import djprtheme
#' @importFrom rlang .data .env

app <- function(...) {

  if(!exists("djpr_box_server", asNamespace("djprshiny"))){
    stop(
      "Please install the latest version of djprshiny with the following:\n",
      "   remotes::install_github('djpr-data/djprshiny')"
    )
  }

  djprtheme::djpr_use_fonts()
  flextable::set_flextable_defaults(
    font.family = getOption("djprtheme.base_font_family")
  )
  jobs_dash_cache <- cachem::cache_disk(
    dir = file.path(".", "app-cache")
  )

  shinyOptions(cache = jobs_dash_cache)

  shiny::shinyApp(labour_ui(), labour_server)
}
