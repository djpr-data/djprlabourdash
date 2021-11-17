page_disability <- function(...) {
  djpr_tab_panel(
    title = "People with disability",
    br(),
    paste0("This section explores the labour force status of people with disability."),
    br(),
    br(),
    br(),
    htmlOutput("disability_footnote"),
    br()
  )
}
