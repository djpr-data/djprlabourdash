#' @import  djprshiny
#' @import shiny


labour_server <- function(input, output, session) {
  purrr::pmap(
    .l = list(
      id = list(
        "plot1",
        "plot2"
      ),
      plot_function = list(
        example_plot,
        example_plot
      ),
      date_slider = list(
        FALSE,
        TRUE
      )
    ),
    .f = djpr_plot_server
  )
}

app <- function(...) {
  shiny::shinyApp(labour_ui(), labour_server)
}
