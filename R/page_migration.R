page_migration <- function(...) {
  fluidRow(
    br(),
    paste0("This section explores the labour force status of migrants in Australia."),
    br(),
    h2(br(), "Jobactive caseload for refugees"),
    uiOutput("table_jobactive_refugees") %>%
      djpr_with_spinner(),
    br(),
    djpr_plot_ui("gr_refugee_jobact_sincecovid_line"),
    br(),
    djpr_plot_ui("gr_refugee_jobactive_bar"),
    br(),
    htmlOutput("migration_footnote"),
    br()
  )
}
