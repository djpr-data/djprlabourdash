page_aboriginalUI <- function(...) {
  shiny::fluidRow(
    djpr_h2_box("Aboriginal Victorians: jobactive caseload"),
    shinydashboard::box(
      uiOutput("table_jobactive_aboriginal") %>%
                          djpr_with_spinner(),
      width = 12
      ),
    djpr_plot_box("gr_abor_jobactive_sincecovid_line", width = 12),
    djpr_plot_box("gr_abor_jobactive_bar", width = 12),
    djpr_plot_box("aboriginal_footnote", width = 12)
  )
}


page_aboriginal <- function(input, output, session, plt_change, series_latestdates, footnote) {
  output$table_jobactive_aboriginal <- renderUI({
    table_jobactive_aboriginal() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_abor_jobactive_sincecovid_line",
    viz_gr_abor_jobactive_sincecovid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "jobactive_indigenous_ballarat",
      "jobactive_indigenous_bendigo",
      "jobactive_indigenous_barwon",
      "jobactive_indigenous_gippsland",
      "jobactive_indigenous_goulburn/murray",
      "jobactive_indigenous_inner metropolitan melbourne",
      "jobactive_indigenous_north eastern melbourne",
      "jobactive_indigenous_north western melbourne",
      "jobactive_indigenous_south coast of victoria",
      "jobactive_indigenous_south eastern melbourne and peninsula",
      "jobactive_indigenous_western melbourne",
      "jobactive_indigenous_wimmera mallee",
      "jobactive_total_ballarat",
      "jobactive_total_bendigo",
      "jobactive_total_barwon",
      "jobactive_total_gippsland",
      "jobactive_total_goulburn/murray",
      "jobactive_total_inner metropolitan melbourne",
      "jobactive_total_north eastern melbourne",
      "jobactive_total_north western melbourne",
      "jobactive_total_south coast of victoria",
      "jobactive_total_south eastern melbourne and peninsula",
      "jobactive_total_western melbourne",
      "jobactive_total_wimmera mallee"
    ),
    df = dash_data
    ) %>%
      dplyr::filter(date >= as.Date("2019-03-31")),
    date_slider = FALSE
  )

  djpr_plot_server("gr_abor_jobactive_bar",
    viz_gr_abor_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_indigenous_ballarat",
      "jobactive_indigenous_bendigo",
      "jobactive_indigenous_barwon",
      "jobactive_indigenous_gippsland",
      "jobactive_indigenous_goulburn/murray",
      "jobactive_indigenous_inner metropolitan melbourne",
      "jobactive_indigenous_north eastern melbourne",
      "jobactive_indigenous_north western melbourne",
      "jobactive_indigenous_south coast of victoria",
      "jobactive_indigenous_south eastern melbourne and peninsula",
      "jobactive_indigenous_western melbourne",
      "jobactive_indigenous_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )
}
