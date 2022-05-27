page_aboriginalUI <- function(...) {
  shiny::fluidRow(
    djpr_h2_box("Aboriginal Victorians: jobactive caseload"),
    shinydashboard::box(
      uiOutput("table_jobactive_aboriginal") %>%
                          djpr_with_spinner(),
      width = 12
      ),
  shiny::fluidRow(
    djpr_async_ui("gr_abor_jobactive_sincecovid_line", width = 6),
    djpr_async_ui("gr_abor_jobactive_bar", width = 6)
  ),
  htmlOutput("aboriginal_footnote"))
}


page_aboriginal <- function(input, output, session, plt_change, series_latestdates, footnote) {
  output$table_jobactive_aboriginal <- renderUI({
    table_jobactive_aboriginal() %>%
      flextable::htmltools_value()
  })

  djpr_async_server("gr_abor_jobactive_sincecovid_line",
    viz_gr_abor_jobactive_sincecovid_line
  )

  djpr_async_server("gr_abor_jobactive_bar",
    viz_gr_abor_jobactive_bar
  )
}
