page_disability <- function(...) {
  djpr_tab_panel(
    title = "People with disabilities",
    br(),
    paste0("This section explores the labour force status of people with disabilities."),
    br(),
    br(),
    h2(br(), "People with disabilities: jobactive caseload"),
    uiOutput("table_jobactive_pwd") %>%
      djpr_with_spinner(),
    br(),
    br(),
    paste0("The data above include people with disabilities aged 15 and above."),
    br(),
    br(),
    htmlOutput("disability_footnote"),
    br()
  )
}
