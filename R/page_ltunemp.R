page_inclusion <- function(...) {
  djpr_tab_panel(
    title = "Long-term unemployed",
    br(),
    djpr_plot_ui("gr_ltunemp_line"),
    djpr_plot_ui("gr_ltunvic_bar",
                 interactive = FALSE
    ),
    djpr_plot_ui("gr_ltunvic_area",
                 interactive = FALSE
    ),
    br(),
    htmlOutput("inclusion_footnote"),
    br()
  )
}
