page_inclusion <- function(...) {
  djpr_tab_panel(
    title = "Women and men",
    h2("Overview table"),
    br(),
    uiOutput("table_gr_sex"),
    djpr_plot_ui("gr_gen_emp_bar",
                 interactive = FALSE
    ),
    djpr_plot_ui("gr_gen_emppopratio_line"),
    djpr_plot_ui("gr_gen_unemp_line"),
    djpr_plot_ui("gr_gen_partrate_line"),
    djpr_plot_ui("gr_full_part_line")
  )
}
