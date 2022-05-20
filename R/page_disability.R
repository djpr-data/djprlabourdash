page_disabilityUI <- function(...) {
  fluidRow(
    br(),
    paste0("This section explores the labour force status of people with disabilities."),
    br(),
    h2(br(), "People with disabilities: jobactive caseload"),
    uiOutput("table_jobactive_pwd") %>%
      djpr_with_spinner(),
    paste0("The data above include people with disabilities aged 15 and above."),
    br(),
    djpr_plot_ui("gr_pwd_jobact_sincecovid_line"),
    br(),
    djpr_plot_ui("gr_pwd_jobactive_bar"),
    br(),
    htmlOutput("disability_footnote"),
    br()
  )
}


page_disability <- function(input, output, session, plt_change, series_latestdates, footnote) {
  output$table_jobactive_pwd <- renderUI({
    table_jobactive_pwd() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_pwd_jobact_sincecovid_line",
    viz_gr_pwd_jobact_sincecovid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "jobactive_pwd_ballarat",
      "jobactive_pwd_bendigo",
      "jobactive_pwd_barwon",
      "jobactive_pwd_gippsland",
      "jobactive_pwd_goulburn/murray",
      "jobactive_pwd_inner metropolitan melbourne",
      "jobactive_pwd_north eastern melbourne",
      "jobactive_pwd_north western melbourne",
      "jobactive_pwd_south coast of victoria",
      "jobactive_pwd_south eastern melbourne and peninsula",
      "jobactive_pwd_western melbourne",
      "jobactive_pwd_wimmera mallee",
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

  djpr_plot_server("gr_pwd_jobactive_bar",
    viz_gr_pwd_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_pwd_ballarat",
      "jobactive_pwd_bendigo",
      "jobactive_pwd_barwon",
      "jobactive_pwd_gippsland",
      "jobactive_pwd_goulburn/murray",
      "jobactive_pwd_inner metropolitan melbourne",
      "jobactive_pwd_north eastern melbourne",
      "jobactive_pwd_north western melbourne",
      "jobactive_pwd_south coast of victoria",
      "jobactive_pwd_south eastern melbourne and peninsula",
      "jobactive_pwd_western melbourne",
      "jobactive_pwd_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )
}
