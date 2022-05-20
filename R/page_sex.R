page_sexUI <- function(...) {
  fluidRow(
    br(),
    h2(br(), "Overview"),
    uiOutput("table_gr_sex"),
    br(),
    h2(br(), "Labour force status by sex"),
    djpr_plot_ui("gr_gen_emp_bar", interactive = FALSE),
    djpr_plot_ui("gr_full_part_line"),
    h2(br(), "Unemployment by sex"),
    djpr_plot_ui("gr_gen_unemp_line"),
    h2(br(), "Employment to population ratio by sex"),
    djpr_plot_ui("gr_gen_emppopratio_line"),
    h2(br(), "Participation rate by sex"),
    djpr_plot_ui("gr_gen_partrate_line"),
    h2(br(), "Jobactive caseload by sex"),
    uiOutput("table_jobactive_female") %>%
      djpr_with_spinner(),
    djpr_plot_ui("gr_female_jobact_sincecovid_line"),
    br(),
    djpr_plot_ui("gr_female_jobactive_bar"),
    br()
  )
}


page_sex <- function(input, output, session, plt_change, series_latestdates, footnote) {
  output$table_gr_sex <- renderUI({
    table_gr_sex() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Groups: line chart of emp-pop by sex
  djpr_plot_server("gr_gen_emppopratio_line",
    plot_function = viz_gr_gen_emppopratio_line,
    data = filter_dash_data(c(
      "A84423244X",
      "A84423468K"
    ),
    df = dash_data
    ),
    date_slider_value_min = Sys.Date() - (365.25 * 10),
    plt_change = plt_change
  )

  # Bar chart: LF status by sex, latest month

  djpr_plot_server("gr_gen_emp_bar",
    viz_gr_gen_emp_bar,
    date_slider = F,
    plt_change = plt_change,
    interactive = FALSE,
    data = filter_dash_data(c(
      "A84423469L",
      "A84423245A",
      "A84423801C",
      "A84423577W",
      "A84423461V",
      "A84423237A",
      "A84423463X",
      "A84423239F",
      "A84423462W",
      "A84423238C"
    ), df = dash_data) %>%
      dplyr::group_by(.data$series) %>%
      dplyr::filter(.data$date == max(.data$date))
  )

  # Line chart: participation by sex over time
  djpr_plot_server("gr_gen_partrate_line",
    viz_gr_gen_partrate_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "A84423355R",
      "A84423243W",
      "A84423467J"
    ),
    df = dash_data
    ),
    date_slider_value_min = Sys.Date() - (365.25 * 5)
  )

  # Line chart: unemployment rate by sex
  djpr_plot_server("gr_gen_unemp_line",
    viz_gr_gen_unemp_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "A84423354L",
      "A84423242V",
      "A84423466F"
    ),
    df = dash_data
    ),
    date_slider_value_min = Sys.Date() - (365.25 * 10)
  )

  djpr_plot_server("gr_full_part_line",
    viz_gr_full_part_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "A84423237A",
      "A84423461V",
      "A84423245A",
      "A84423469L"
    ),
    df = dash_data
    ),
    date_slider_value_min = Sys.Date() - (365.25 * 5)
  )

  # Jobactive by sex
  output$table_jobactive_female <- renderUI({
    table_jobactive_female() %>%
      flextable::htmltools_value()
  })

  djpr_plot_server("gr_female_jobact_sincecovid_line",
    viz_gr_female_jobact_sincecovid_line,
    plt_change = plt_change,
    data = filter_dash_data(c(
      "jobactive_female_ballarat",
      "jobactive_female_bendigo",
      "jobactive_female_barwon",
      "jobactive_female_gippsland",
      "jobactive_female_goulburn/murray",
      "jobactive_female_inner metropolitan melbourne",
      "jobactive_female_north eastern melbourne",
      "jobactive_female_north western melbourne",
      "jobactive_female_south coast of victoria",
      "jobactive_female_south eastern melbourne and peninsula",
      "jobactive_female_western melbourne",
      "jobactive_female_wimmera mallee",
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

  djpr_plot_server("gr_female_jobactive_bar",
    viz_gr_female_jobactive_bar,
    data = filter_dash_data(c(
      "jobactive_female_ballarat",
      "jobactive_female_bendigo",
      "jobactive_female_barwon",
      "jobactive_female_gippsland",
      "jobactive_female_goulburn/murray",
      "jobactive_female_inner metropolitan melbourne",
      "jobactive_female_north eastern melbourne",
      "jobactive_female_north western melbourne",
      "jobactive_female_south coast of victoria",
      "jobactive_female_south eastern melbourne and peninsula",
      "jobactive_female_western melbourne",
      "jobactive_female_wimmera mallee"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider = FALSE,
    download_button = FALSE,
    width_percent = 75
  )

  observeEvent(input$link_sex, {
    updateNavbarPage(session, "navbarpage", "tab-sex")
  })
}
