page_sex <- function(...) {
  djpr_tab_panel(
    title = "Sex",
    br(),
    h2(br(), "Overview"),
    uiOutput("table_gr_sex"),
    br(),
    h2(br(), "Labour force status by sex"),
    djpr_plot_ui("gr_gen_emp_bar", interactive = FALSE),
    djpr_plot_ui("gr_full_part_line"),
    h2(br(), "Unemployment by sex"),
    djpr_plot_ui("gr_gen_unemp_line"),
    h2(br(), "Employment to population ratio by sex"),
    djpr_plot_ui("gr_gen_emppopratio_line"),
    h2(br(), "Participation rate by sex"),
    djpr_plot_ui("gr_gen_partrate_line")
  )
}
