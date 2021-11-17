page_disability <- function(...) {
  djpr_tab_panel(
    title = "People with disability",
    br(),
    paste0("This section explores the labour force status of people with disability."),
    br(),
    # this table is just a placeholder and needs to be removed once we have functions for people with disabilities
    djpr_plot_ui("gr_disability_test_line"),
    br(),
    htmlOutput("inclusion_footnote"),
    br()
  )
}
