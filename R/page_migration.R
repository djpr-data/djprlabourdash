page_migration <- function(...) {
  djpr_tab_panel(
    title = "Migrants",
    br(),
    paste0("This section explores the labour force status of migrants in Australia."),
    br(),
    br(),
    h2(br(), "Jobactive caseload for refugees"),
    uiOutput("table_jobactive_refugees") %>%
      djpr_with_spinner(),
    br(),
    htmlOutput("migration_footnote"),
    br()
  )
}
