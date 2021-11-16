page_ltunemp <- function(...) {
  djpr_tab_panel(
    title = "Long-term unemployed",
    br(),
    paste0("Long-term unemployment is defined as a duration of unemployment of 12 months or more, "),
    paste0("calculated from the time a person either last worked in any job for two weeks or more, "),
    paste0("or began actively looking for work (whichever is the more recent). "),
    paste0("Measuring long-term unemployment is important as it impacts on communities both socially "),
    paste0(" and economically. Compared to short-term unemployed people, those unemployed for longer "),
    paste0("periods of time can experience higher levels of competition, decreased confidence and motivation."),
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
