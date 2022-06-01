page_aboriginalUI <- function(...) {
  shiny::fluidRow(

    column_nopad(
      width = 4,

      djprshiny::djpr_h1_box("Aboriginal Victorians",
                             colour = djprtheme::djpr_blue),

      shinydashboard::box(
        width = 12,
        style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
        "Jobactive caseload data."
      )
    ),

    box(
      width = 8,
      uiOutput("table_jobactive_aboriginal") %>%
        djpr_with_spinner()
    ),

    djpr_async_ui("gr_abor_jobactive_sincecovid_line", width = 6),
    djpr_async_ui("gr_abor_jobactive_bar", width = 6),

    box(
      width = 12,
      shiny::uiOutput("aborifinal_footnote")
      )
  )
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
