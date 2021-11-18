page_aboriginal <- function(...) {
  djpr_tab_panel(
    title = "Aboriginal Victorians",
    br(),
    paste0("This section explores the labour force status for Aboriginal Victorians"),
    br(),
    br(),
    br(),
    htmlOutput("aboriginal_footnote"),
    br()
  )
}
