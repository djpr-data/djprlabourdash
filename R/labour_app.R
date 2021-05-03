#' @import djprshiny
#' @import djprdashdata
#' @import shiny

labour_server <- function(input, output, session) {

  dash_data <- load_and_hide()

  plot_params <- tibble::tribble(
                 ~id, ~plot_function,                                                   ~data, ~date_slider,
             "plot1",  example_plot, filter_lfs_data("unemployment rate", state = "victoria", df = dash_data), TRUE,
             "plot2",  example_plot, filter_lfs_data("unemployment rate", state = "victoria", df = dash_data),  FALSE
             )


  purrr::pmap(
    .l = plot_params,
    .f = djpr_plot_server
  )
}

app <- function(...) {
  shiny::shinyApp(labour_ui(), labour_server)
}
