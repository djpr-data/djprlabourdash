page_migration <- function(...) {
  djpr_tab_panel(
    title = "Migrants",
    br(),
    paste0("This section explores the labour force status of migrants in Australia."),
    br(),
    # this table is just a placeholder and needs to be removed once we have functions for migrants
    djpr_plot_ui("gr_migration_test_line"),
    br(),
    htmlOutput("inclusion_footnote"),
    br()
  )
}
