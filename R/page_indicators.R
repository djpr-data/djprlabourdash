page_indicators <- function(...) {
  djpr_tab_panel(
    title = "Indicators",
    h1("Employment"),
    djpr_plot_ui("emp_growth_sincecovid"),
    paste0(rep("Text goes here", 100), collapse = ""),
    h2("Some subtitle"),
    paste0(rep("Text goes here", 100), collapse = ""),
    h2("Some other subtitle"),
    h1("Title"),
    djpr_plot_ui("plot"),
    paste0(rep("Text goes here", 100), collapse = ""),

    h1("Another title"),
    paste0(rep("Text goes here", 100), collapse = ""),

    h2("Subsection A"),
    paste0(rep("Text goes here", 100), collapse = ""),

    h2("Subsection B"),
    paste0(rep("Text goes here", 100), collapse = ""),

    h1("A final title"),
    paste0(rep("Text goes here", 100), collapse = "")
  )
}
