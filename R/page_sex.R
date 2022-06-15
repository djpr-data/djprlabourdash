page_sexUI <- function(...) {
  fluidRow(


    djprshiny::djpr_h1_box("Sex"),
    shinydashboard::box(
      width = 12,
      style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
      "This page provides key labour force indicators by gender. ",
      "It also contains information on females assisted through the jobactive program."
    ),

    box(
      width = 12,
      uiOutput("table_gr_sex") %>%
        djpr_with_spinner()
    ),
    djpr_h2_box("Labour force status by sex"),
    djpr_box_ui(width = 12, "gr_gen_emp_bar"),
    djpr_box_ui(
      width = 12,
      id = "gr_full_part_line",
      date_slider(
        "gr_full_part_line",
        table_no = "6202012",
        value = c(Sys.Date() - (365.25 * 5), data_dates$`6202012`$max)
      )
    ),
    djpr_h2_box("Unemployment by sex"),
    djpr_box_ui(
      width = 12,
      id = "gr_gen_unemp_line",
      date_slider(
        "gr_gen_unemp_line",
        table_no = "6202012",
        value = c(Sys.Date() - (365.25 * 10), data_dates$`6202012`$max)
      )
    ),
    djpr_h2_box("Employment to population ratio by sex"),
    djpr_box_ui(
      width = 12,
      "gr_gen_emppopratio_line",
      date_slider(
        "gr_gen_emppopratio_line",
        table_no = "6202012",
        value = c(Sys.Date() - (365.25 * 10), data_dates$`6202012`$max)
      )
    ),
    djpr_h2_box("Participation rate by sex"),
    djpr_box_ui(
      width = 12,
      "gr_gen_partrate_line",
      date_slider("gr_gen_partrate_line", table_no = "6202012")
    ),
    djpr_h2_box("Jobactive caseload by sex"),
    box(
      width = 12,
      uiOutput("table_jobactive_female") %>%
        djpr_with_spinner()
    ),
    djpr_box_ui(
      width = 12,
      id = "gr_female_jobact_sincecovid_line",
      date_slider("gr_female_jobact_sincecovid_line", table_no = "jobactive")
    ),
    djpr_box_ui(width = 12, "gr_female_jobactive_bar"),
    box(
      width = 12,
      style = "padding:10px;",
      HTML(
        # "This dashboard is produced by the <b>Strategy and Priority ",
        # "Projects - Data + Analytics</b> team at the Victorian Department ",
        # "of Jobs, Precincts and Regions.",
        "The latest data in this ",
        "dashboard is for ", format(data_dates$`6202012`$max, "%B %Y"), '.',
        "We are committed to making our websites accessible to all users.",
        "We are aware that parts of these dashboards are not fully accessible.",
        "If you require this information in an alternative format or would",
        "like to provide feedback please ",
        "<a href='mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard'>email us</a>.",
        "</br>"
      ),
      div(
        style = "text-align: center;",
        tags$a(
          class = "legalLink",
          href = "#shiny-tab-legal",
          "Copyright | Disclaimer"
        )
      )
    )
  )
}


page_sex <- function(input, output, session) {
  output$table_gr_sex <- renderUI({
    table_gr_sex() %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  }) %>%
    bindCache(data_dates$`6202012`$max)

  # Groups: line chart of emp-pop by sex
  djpr_box_server(
    id = "gr_gen_emppopratio_line",
    plot_fun = viz_gr_gen_emppopratio_line,
    dates = input$dates
  )

  # Bar chart: LF status by sex, latest month

  djpr_box_server(
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
  djpr_box_server(
    id = "gr_gen_partrate_line",
    plot_fun = viz_gr_gen_partrate_line,
    dates = input$dates
  )

  # Line chart: unemployment rate by sex
  djpr_box_server(
    id = "gr_gen_unemp_line",
    plot_fun = viz_gr_gen_unemp_line,
    dates = input$dates
  )

  djpr_box_server(
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
      flextable::htmltools_value(ft.shadow = FALSE)
  })

  djpr_box_server(
    id = "gr_female_jobact_sincecovid_line",
    plot_fun = viz_gr_female_jobact_sincecovid_line,
    dates = input$dates
  )

  djpr_box_server(
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
}
