#' @import djprshiny
#' @import djprdashdata
#' @import shiny

labour_server <- function(input, output, session) {
  dash_data <- load_and_hide()

  # Overview ------

  output$main_table <- reactable::renderReactable({
    # req(dash_data)
    ids <- c(
      "A84423349V",
      "A84423356T",
      "A84423355R",
      "A84423354L",
      "A84423350C"
    )

    df <- dash_data %>%
      dplyr::filter(.data$series_id %in% ids) %>%
      tidyr::unnest(cols = dplyr::everything())

    overview_table(data = df)
  })

  djpr_plot_server(
    id = "plot1",
    plot_function = example_plot,
    date_slider = TRUE,
    data = filter_dash_data(c("A84423242V", "A84423466F"),
                            df = dash_data
    ),
    check_box_options = c("Males", "Females"),
    check_box_var = sex,
    plt_change = reactive(input$plt_change)
  )

  # Indicators -----

  djpr_plot_server(
    id = "emp_growth_sincecovid",
    plot_function = viz_empgrowth_sincecovid,
    date_slider = TRUE,
    data = filter_dash_data(c("A84423043C", "A84423349V"),
                            df = dash_data),
    date_slider_value_min = as.Date("2020-01-01"),
    plt_change = reactive(input$plt_change)
  )

}

app <- function(...) {
  shiny::shinyApp(labour_ui(), labour_server)
}
