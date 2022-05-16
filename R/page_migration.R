page_migrationUI <- function(...) {
  djpr_tab_panel(
    title = "Migrants",
    br(),
    paste0("This section explores the labour force status of migrants in Australia."),
    br(),
    h2(br(), "Jobactive caseload for refugees"),
    uiOutput("table_jobactive_refugees") %>%
      djpr_with_spinner(),
    br(),
    djpr_plot_ui("gr_refugee_jobact_sincecovid_line"),
    br(),
    djpr_plot_ui("gr_refugee_jobactive_bar"),
    br(),
    htmlOutput("migration_footnote"),
    br()
  )
}


page_migration <- function(input, output, session, plt_change) {
  output$table_jobactive_refugees <- renderUI({
    table_jobactive_refugees() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_refugee_jobact_sincecovid_line",
    viz_gr_refugee_jobact_sincecovid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "jobactive_refugee_ballarat",
      "jobactive_refugee_bendigo",
      "jobactive_refugee_barwon",
      "jobactive_refugee_gippsland",
      "jobactive_refugee_goulburn/murray",
      "jobactive_refugee_inner metropolitan melbourne",
      "jobactive_refugee_north eastern melbourne",
      "jobactive_refugee_north western melbourne",
      "jobactive_refugee_south coast of victoria",
      "jobactive_refugee_south eastern melbourne and peninsula",
      "jobactive_refugee_western melbourne",
      "jobactive_refugee_wimmera mallee",
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

  djpr_plot_server("gr_refugee_jobactive_bar",
    viz_gr_refugee_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_refugee_ballarat",
      "jobactive_refugee_bendigo",
      "jobactive_refugee_barwon",
      "jobactive_refugee_gippsland",
      "jobactive_refugee_goulburn/murray",
      "jobactive_refugee_inner metropolitan melbourne",
      "jobactive_refugee_north eastern melbourne",
      "jobactive_refugee_north western melbourne",
      "jobactive_refugee_south coast of victoria",
      "jobactive_refugee_south eastern melbourne and peninsula",
      "jobactive_refugee_western melbourne",
      "jobactive_refugee_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )
}
