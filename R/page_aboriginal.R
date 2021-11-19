page_aboriginal <- function(...) {
  djpr_tab_panel(
    title = "Aboriginal and Torre Strait Islander Peoples",
    br(),
    paste0("This section explores the labour force status for Aboriginal "),
    paste0("and Torre Strait Islander Peoples"),
    br(),
    br(),
    br(),
    htmlOutput("aboriginal_footnote"),
    br()
  )
}
