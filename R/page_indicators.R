page_indicators <- function(...) {
  djpr_tab_panel(
    title = "Indicators",
    h1("Employment"),
    htmlOutput("text_empgrowth_sincecovid"),
    djpr_plot_ui("emp_growth_sincecovid"),
    paste0(rep("Text goes here", 100), collapse = ""),
    h2("Some subtitle"),
    paste0(rep("Text goes here", 100), collapse = ""),
    h2("Some other subtitle"),
    h1("Employment"),
    djpr_plot_ui("plot")

  )
}
