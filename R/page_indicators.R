page_indicators <- function(...) {
  djpr_tab_panel(
    title = "Indicators",
    h1("Employment"),
    reactable::reactableOutput("ind_emp_table"),
    reactable_caption(),
    br(),
    htmlOutput("ind_empgrowth_sincecovid_text"),
    djpr_plot_ui("ind_emp_sincecovid_line"),
    # htmlOutput("ind_emp_dotpoints"),
    br(),
    djpr_plot_ui("ind_empgro_line"),
    br(),
    djpr_plot_ui("ind_emppopratio_line"),
    br(),
    h1("Unemployment"),
    djpr_plot_ui("ind_unemp_states_dot"),
    br()
  )
}
