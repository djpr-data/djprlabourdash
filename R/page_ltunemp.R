page_ltunempUI <- function(...) {
  fluidRow(

    # No padding column with width = 4
    column_nopad(
      width = 4,
      djprshiny::djpr_h1_box("Long-term unemployed"),
      shinydashboard::box(
        width = 12,
        style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
        "Long-term unemployment is defined as a duration of unemployment of 12 months or more, ",
        "calculated from the time a person either last worked in any job for two weeks or more, ",
        "or began actively looking for work (whichever is the more recent). ",
        "Measuring long-term unemployment is important as it impacts on communities both socially ",
        "and economically. Compared to short-term unemployed people, those unemployed for longer ",
        "periods of time can experience higher levels of competition, decreased confidence and motivation."
      )
    ),
    djpr_box_ui(
      id = "gr_ltunemp_line",
      width = 8,
      date_slider(
        id = "gr_ltunemp_line",
        table_no = "UM2",
        value = c(as.Date("2000-01-01"), data_dates$`6202012`$max)
      )
    ),
    djpr_box_ui("gr_ltunvic_bar", width = 12),
    djpr_box_ui(
      "gr_ltunvic_area",
      width = 12,
      date_slider("gr_ltunvic_area", "UM2")
    ),
    box(
      width = 12,
      style = "padding:10px;",
      HTML(
        "This dashboard is produced by the <b>Strategy and Priority ",
        "Projects - Data + Analytics</b> team at the Victorian Department ",
        "of Jobs, Precincts and Regions. The <b>latest data in this ",
        "dashboard is for ", format(data_dates$`6202012`$max, "%B %Y"),
        '</b>. Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.'
      )
    )
  )
}

page_ltunemp <- function(input, output, session) {
  djpr_box_server(
    id = "gr_ltunemp_line",
    plot_fun = viz_gr_ltunemp_line,
    date_range = input$dates
  )

  djpr_box_server(
    "gr_ltunvic_bar",
    viz_gr_ltunvic_bar
  )

  djpr_box_server(
    "gr_ltunvic_area",
    viz_gr_ltunvic_area,
    dates = input$dates
  )

  observeEvent(input$link_ltunemp, {
    updateNavbarPage(session, "navbarpage", "tab-ltunemp")
  })
}
