page_migration <- function(...) {
  djpr_tab_panel(
    title = "Migrants",
    br(),
    paste0("This section explores the labour force status of migrants in Australia."),
    br(),
    br(),
    br(),
    htmlOutput("migration_footnote"),
    br()
  )
}
