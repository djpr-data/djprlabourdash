page_aboriginalUI <- function(...) {
  shiny::fluidRow(

    column_nopad(
      width = 4,

      djprshiny::djpr_h1_box("Aboriginal Victorians"),

      shinydashboard::box(
        width = 12,
        style = "padding: 15px;font-size: 15px;background: #C0E4B5;",
        "This page contains the number of Aboriginal people assisted through the jobactive program.",
        "The jobactive program is a commonwealth program designed to provide services to the majority of job seekers."
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
      style = "padding:10px;",
      HTML(
        "This dashboard is produced by the <b>Strategy and Priority ",
        "Projects - Data + Analytics</b> team at the Victorian Department ",
        "of Jobs, Precincts and Regions. The <b>latest data in this ",
        "dashboard is for ",  format(data_dates$jobactive$max, "%B %Y"),
        '</b>. Please <a href="mailto:spp-data@ecodev.vic.gov.au?subject=DJPR Jobs Dashboard">email us</a> with any comments or feedback.'

      )
    )
  )
}


page_aboriginal <- function(input, output, session) {
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
