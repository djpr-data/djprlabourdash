page_overviewUI <- function(...) {
  shiny::tagList(

    # No padding column with width = 4
    column_nopad(
      width = 4,

      djprshiny::djpr_h1_box("DJPR Jobs Dashboard"),

      shinydashboard::box(
        width = 12,
        style = "padding: 15px;font-size: 20px;background: #C0E4B5;",
        "This site provides a guide to the latest movements Victorian ",
        "employment information. "
      )
    ),
    box(
      width = 8,
      shiny::uiOutput("main_table", height = "800px") %>%
        djpr_with_spinner(proxy.height = "800px")
    ),
    box(
      width = 12,
      style = "padding:10px;",
      HTML(
        "This dashboard is produced by the <b>Strategy and Priority ",
        "Projects - Data + Analytics</b> team at the Victorian Department ",
        "of Jobs, Precincts and Regions. The latest data in this ",
        "dashboard is for </b>", format(data_dates$`6202012`$max, "%B %Y"),
        '</b>. Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.\n'
      ),
      shiny::div(style = 'position:right;bottom:20px',
                 shiny::actionLink('fromoverview_tolegal',
                                   "Disclaimer and Copyright",
                                   style = 'material-flat',
                                   color = 'success')
                 )
      ),

  )
}

page_overview <- function(input, output, session) {
  output$main_table <- renderUI({
    req(dash_data)
    table_overview() %>%
      flextable::htmltools_value(ft.shadow = FALSE)
  }) %>%
    bindCache(
      data_dates$`6202012`$max,
      data_dates$`6202016`$max,
      data_dates$`6202019`$max,
      data_dates$`6202023`$max,
      data_dates$`6291016`$max
    )

  output$overview_diagram <- renderPlot({
    viz_overview_illustration()
  })

  observeEvent(input$fromoverview_tolegal, {
    shinydashboard::updateTabItems(session,
                                   "tabs",
                                   selected = "legal")
  })
}
