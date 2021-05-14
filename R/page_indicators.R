page_indicators <- function(...) {
  djpr_tab_panel(
    title = "Indicators",
    h1("Employment"),
    htmlOutput("ind_empgrowth_sincecovid_text"),
    djpr_plot_ui("ind_emp_sincecovid_line"),
    htmlOutput("ind_emp_dotpoints"),
    reactable::reactableOutput("ind_emp_table"),
    br(),
    br()
  )
}
