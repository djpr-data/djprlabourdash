page_overviewUI <- function(...) {
  loading_content <- div(
    id = "loading_page",
    br(),
    br(),
    br(),
    br(),
    djprshiny::centred_row(h4(" Loading data, please wait..."))
  )

  main_content <- shinyjs::hidden(
    div(
      id = "main_content",
      br(),
      br(),
      br(),
      br(),
      djprshiny::centred_row(
        h1("DJPR Jobs Dashboard")
      ),
      br(),
      djprshiny::centred_row(
        uiOutput("main_table", height = "800px") %>%
          djpr_with_spinner(hide.ui = TRUE)
      ),
      br(),
      djprshiny::centred_row(htmlOutput("overview_footnote")),
      br(),
      br()
    )
  )

  tabPanel(
    title = "Overview",
    ggiraph_js(),
    HTML(""),
    value = "tab-overview",
    shinyjs::useShinyjs(),
    loading_content,
    main_content
  )
}

page_overview <- function(input, output, session, plt_change) {

  output$main_table <- renderUI({
    req(dash_data)
    table_overview() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

  observeEvent(input$link_overview, {
    updateNavbarPage(session, "navbarpage", "tab-overview")
  })

}
