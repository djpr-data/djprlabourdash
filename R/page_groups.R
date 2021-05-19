page_groups <- function(...) {
  djpr_tab_panel(
    title = "Groups",
    h1("Key groups"),
    tagList(
      "This page contains information about the labour force status of key groups of ",
      "Victorians, such as women, and young people.",
      "For more information about overall labour force indicators ",
      "see the ",
      actionLink("link_indicators", "indicators page"),
      ". For information about how employment and unemployment varies across Victoria, see the ",
      actionLink("link_regions", "regions page"), "."
    ),
    br(),
    djpr_plot_ui("gr_gen_emp_bar"),
    djpr_plot_ui("gr_gen_partrate_line"),
    djpr_plot_ui("gr_gen_unemp_line"),
    djpr_plot_ui("gr_yth_emp_sincecovid_line"),
    htmlOutput("groups_footnote"),
    br()
  )
}
