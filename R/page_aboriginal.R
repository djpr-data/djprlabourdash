page_aboriginal <- function(...) {
  djpr_tab_panel(
    title = "Aboriginal and Torre Strait Islander Peoples",
    br(),
    paste0("This section explores the labour force status for Aboriginal "),
    paste0("and Torre Strait Islander Peoples"),
    br(),
    # this table is just a placeholder and needs to be removed once we have functions for aboriginals
    djpr_plot_ui("gr_aboriginal_test_line"),
    br(),
    htmlOutput("inclusion_footnote"),
    br()
  )
}
