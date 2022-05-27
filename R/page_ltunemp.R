page_ltunempUI <- function(...) {
  fluidRow(
    br(),
    paste0("Long-term unemployment is defined as a duration of unemployment of 12 months or more, "),
    paste0("calculated from the time a person either last worked in any job for two weeks or more, "),
    paste0("or began actively looking for work (whichever is the more recent). "),
    paste0("Measuring long-term unemployment is important as it impacts on communities both socially "),
    paste0("and economically. Compared to short-term unemployed people, those unemployed for longer "),
    paste0("periods of time can experience higher levels of competition, decreased confidence and motivation."),
    djpr_async_ui("gr_ltunemp_line",
                  date_slider('gr_ltunemp_line',
                              table_no = '6202012',
                              min = as.Date('2000-01-01')
                              )),
    djpr_async_ui("gr_ltunvic_bar"),
    djpr_async_ui("gr_ltunvic_area"),
    br(),
    htmlOutput("inclusion_footnote"),
    br()
  )
}

page_ltunemp <- function(input, output, session, plt_change, series_latestdates, footnote) {
  djpr_async_server(
    id       = "gr_ltunemp_line",
    plot_fun = viz_gr_ltunemp_line,
    date_range = input$dates
  )

  djpr_async_server("gr_ltunvic_bar",
    viz_gr_ltunvic_bar
  )

  djpr_async_server("gr_ltunvic_area",
    viz_gr_ltunvic_area
  )

  observeEvent(input$link_ltunemp, {
    updateNavbarPage(session, "navbarpage", "tab-ltunemp")
  })
}
