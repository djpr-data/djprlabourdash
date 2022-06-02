page_sexUI <- function(...) {
  fluidRow(

    # No padding column with width = 4
    column_nopad(
      width = 4,

      djprshiny::djpr_h1_box("Sex"),

      shinydashboard::box(
        width = 12,
        style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
        "Some content."
      )
    ),

    box(
      width = 8,
      uiOutput("table_gr_sex") %>%
        djpr_with_spinner()
      ),

    djpr_h2_box( "Labour force status by sex"),
    djpr_async_ui(width = 12, "gr_gen_emp_bar"),

    djpr_async_ui(
      width = 12,
      id = "gr_full_part_line",
      date_slider(
        "gr_full_part_line",
        table_no = "6202012",
        value = c(Sys.Date() - (365.25 * 5), data_dates$`6202012`$max))
      ),

    djpr_h2_box( "Unemployment by sex"),

    djpr_async_ui(
      width = 12,
      id= "gr_gen_unemp_line",
      date_slider (
        "gr_gen_unemp_line",
        table_no = "6202012",
        value = c(Sys.Date() - (365.25 * 10), data_dates$`6202012`$max)
        )
    ),

    djpr_h2_box("Employment to population ratio by sex"),

    djpr_async_ui(
      width = 12,
      "gr_gen_emppopratio_line",
      date_slider(
        "gr_gen_emppopratio_line",
        table_no = "6202012",
        value = c(Sys.Date() - (365.25 * 10), data_dates$`6202012`$max)
        )
      ),

    djpr_h2_box( "Participation rate by sex"),

    djpr_async_ui(
      width = 12,
      "gr_gen_partrate_line",
      date_slider("gr_gen_partrate_line", table_no="6202012")),

    djpr_h2_box( "Jobactive caseload by sex"),

    box(
      width = 12,
      uiOutput("table_jobactive_female") %>%
        djpr_with_spinner()
    ),

    djpr_async_ui(
      width = 12,
      id = "gr_female_jobact_sincecovid_line",
      date_slider("gr_female_jobact_sincecovid_line", table_no = "jobactive")
    ),

    djpr_async_ui(width = 12, "gr_female_jobactive_bar"),

    box(
      width = 12,
      shiny::uiOutput("inclusion_footnote")
      )
  )
}


page_sex <- function(input, output, session, plt_change, series_latestdates, footnote) {

  output$table_gr_sex <- renderUI({
    table_gr_sex() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  # Groups: line chart of emp-pop by sex
  djpr_async_server(
    id= "gr_gen_emppopratio_line",
    plot_fun = viz_gr_gen_emppopratio_line,
    dates = input$dates,
    data = filter_dash_data(c(
      "A84423244X",
      "A84423468K"
    ),
    df = dash_data
    )
  )

  # Bar chart: LF status by sex, latest month

  djpr_async_server(
    id = "gr_gen_emp_bar",
    plot_fun = viz_gr_gen_emp_bar,
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
  djpr_async_server(
    id= "gr_gen_partrate_line",
    plot_fun= viz_gr_gen_partrate_line,
    dates = input$dates,
    data = filter_dash_data(c(
      "A84423355R",
      "A84423243W",
      "A84423467J"
    ),
    df = dash_data
    )
  )

  # Line chart: unemployment rate by sex
  djpr_async_server(
   id  = "gr_gen_unemp_line",
   plot_fun = viz_gr_gen_unemp_line,
   dates = input$dates,
    data = filter_dash_data(c(
      "A84423354L",
      "A84423242V",
      "A84423466F"
    ),
    df = dash_data
    )
  )

  djpr_async_server(
    id = "gr_full_part_line",
    plot_fun = viz_gr_full_part_line,
    dates = input$dates,
    data = filter_dash_data(c(
      "A84423237A",
      "A84423461V",
      "A84423245A",
      "A84423469L"
    ),
    df = dash_data
    )
  )

  # Jobactive by sex
  output$table_jobactive_female <- renderUI({
    table_jobactive_female() %>%
      flextable::htmltools_value()
  })

  djpr_async_server(
    id = "gr_female_jobact_sincecovid_line",
    plot_fun = viz_gr_female_jobact_sincecovid_line,
    dates = input$dates
     )

  djpr_async_server(
    id = "gr_female_jobactive_bar",
    plot_fun = viz_gr_female_jobactive_bar,
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
    )
  )

  observeEvent(input$link_sex, {
    updateNavbarPage(session, "navbarpage", "tab-sex")
  })
}
