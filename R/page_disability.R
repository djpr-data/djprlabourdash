page_disability <- function(...) {
  djpr_tab_panel(
    title = "People with disabilities",
    br(),
    paste0("This section explores the labour force status of people with disabilities."),
    br(),
    br(),
    h2(br(), "Victorian jobactive caseload for people with disabilities"),
    uiOutput("table_jobactive_pwd") %>%
      djpr_with_spinner(),
    br(),
    br(),
    htmlOutput("disability_footnote"),
    br()
  )
}
