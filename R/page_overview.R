page_overviewUI <- function(...) {

  shiny::tagList(

    # No padding column with width = 4
    div(
      class = "col-sm-4",
      style = "padding:0px;",

      djprshiny::djpr_h2_box("DJPR Jobs Dashboard"),

      shinydashboard::box(
        width = 12,
        "Some content"
      )
    ),


    div(
      class = "col-sm-8",
      div(
        class = "box",
        shiny::uiOutput("main_table", height = "800px") %>%
          djpr_with_spinner(proxy.height = "800px")
      )
    ),

    shinydashboard::box(
      width = 12,
      shiny::uiOutput("overview_footnote")
    )

  )

}

page_overview <- function(input, output, session, plt_change, series_latestdates, footnote) {

  output$main_table <- renderUI({
    req(dash_data)
    table_overview() %>%
      flextable::htmltools_value()
  }) %>%
    bindCache(series_latestdates)

}
