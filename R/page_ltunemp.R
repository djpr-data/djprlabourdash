page_ltunempUI <- function(...) {
  fluidPage(

    fluidRow(
      # No padding column with width = 4
      column(
        width = 4,
        djprshiny::djpr_h1_box("Long-term unemployed") %>% fluidRow(),
        shinydashboard::box(
          width = 12,
          style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
          "Long-term unemployment is defined as a duration of unemployment of 12 months or more, ",
          "calculated from the time a person either last worked in any job for two weeks or more, ",
          "or began actively looking for work (whichever is the more recent). ",
          "Measuring long-term unemployment is important as it impacts on communities both socially ",
          "and economically. Compared to short-term unemployed people, those unemployed for longer ",
          "periods of time can experience higher levels of competition, decreased confidence and motivation."
        ) %>% fluidRow()
      ),
      djpr_box_ui(
        id = "gr_ltunemp_line",
        width = 8,
        date_slider(
          id = "gr_ltunemp_line",
          table_no = "UM2",
          value = c(as.Date("2000-01-01"), data_dates$`6202012`$max)
        )
      )
    ),

    djpr_box_ui("gr_ltunvic_bar", width = 12) %>% fluidRow(),
    djpr_box_ui(
      "gr_ltunvic_area",
      width = 12,
      date_slider("gr_ltunvic_area", "UM2")
    ) %>% fluidRow(),
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
    ) %>% fluidRow()
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
