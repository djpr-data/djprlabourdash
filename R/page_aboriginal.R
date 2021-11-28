page_aboriginal <- function(...) {
  djpr_tab_panel(
    title = "Aboriginal Victorians",
    br(),
    paste0("This section explores the labour force status for Aboriginal Victorians"),
    br(),
    br(),
    uiOutput("table_jobactive_aboriginal") %>%
      djpr_with_spinner(),
    br(),
    htmlOutput("aboriginal_footnote"),
    br()
  )
}
