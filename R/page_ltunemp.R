page_ltunempUI <- function(...) {
  fluidRow(
    br(),
    paste0("Long-term unemployment is defined as a duration of unemployment of 12 months or more, "),
    paste0("calculated from the time a person either last worked in any job for two weeks or more, "),
    paste0("or began actively looking for work (whichever is the more recent). "),
    paste0("Measuring long-term unemployment is important as it impacts on communities both socially "),
    paste0("and economically. Compared to short-term unemployed people, those unemployed for longer "),
    paste0("periods of time can experience higher levels of competition, decreased confidence and motivation."),
    djpr_plot_ui("gr_ltunemp_line"),
    djpr_plot_ui("gr_ltunvic_bar",
      interactive = FALSE
    ),
    djpr_plot_ui("gr_ltunvic_area",
      interactive = FALSE
    ),
    br(),
    htmlOutput("inclusion_footnote"),
    br()
  )
}

page_ltunemp <- function(input, output, session, plt_change, series_latestdates, footnote) {
  djpr_plot_server("gr_ltunemp_line",
    viz_gr_ltunemp_line,
    data = filter_dash_data(c(
      "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
      "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
      "A84423687K",
      "A84423089K",
      "A84597681W"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    date_slider_value_min = as.Date("2000-01-01")
  )

  djpr_plot_server("gr_ltunvic_bar",
    viz_gr_ltunvic_bar,
    data = filter_dash_data(c(
      "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
      "unemployed total ('000)_victoria_13 weeks and under 26 weeks (3-6 months)",
      "unemployed total ('000)_victoria_26 weeks and under 52 weeks (6-12 months)",
      "unemployed total ('000)_victoria_4 weeks and under 13 weeks (1-3 months)",
      "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
      "unemployed total ('000)_victoria_under 4 weeks (under 1 month)"
    ),
    df = dash_data
    ),
    plt_change = plt_change,
    interactive = FALSE,
    date_slider = FALSE
  )

  djpr_plot_server("gr_ltunvic_area",
    viz_gr_ltunvic_area,
    data = filter_dash_data(c(
      "unemployed total ('000)_victoria_104 weeks and over (2 years and over)",
      "unemployed total ('000)_victoria_13 weeks and under 26 weeks (3-6 months)",
      "unemployed total ('000)_victoria_26 weeks and under 52 weeks (6-12 months)",
      "unemployed total ('000)_victoria_4 weeks and under 13 weeks (1-3 months)",
      "unemployed total ('000)_victoria_52 weeks and under 104 weeks (1-2 years)",
      "unemployed total ('000)_victoria_under 4 weeks (under 1 month)"
    ),
    df = dash_data
    ),
    interactive = FALSE,
    plt_change = plt_change
  )

  observeEvent(input$link_ltunemp, {
    updateNavbarPage(session, "navbarpage", "tab-ltunemp")
  })
}
