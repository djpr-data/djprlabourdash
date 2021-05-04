#' @import djprshiny
#' @import djprdashdata
#' @import shiny

labour_server <- function(input, output, session) {

  dash_data <- load_and_hide()

  djpr_plot_server(id = "plot1",
                   plot_function = example_plot,
                   date_slider = TRUE,
                   data = filter_dash_data(c("A84423242V", "A84423466F"),
                                           df = dash_data),
                   check_box_options = c("Males", "Females"),
                   check_box_var = sex
  )
}

app <- function(...) {
  shiny::shinyApp(labour_ui(), labour_server)
}
