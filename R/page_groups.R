page_groups <- function(...) {
  djpr_tab_panel(
    title = "Groups",
    h1("Compare groups"),
    br(),
    djpr_plot_ui("gr_gen_emp_bar"),
    djpr_plot_ui("gr_gen_partrate_line"),
    djpr_plot_ui("gr_gen_unemp_line"),
    djpr_plot_ui("gr_yth_emp_sincecovid_line"),
    htmlOutput("groups_footnote"),
    br()
  )
}
