page_aboriginal <- function(...) {
  shiny::fluidRow(
    djpr_h2_box("Aboriginal Victorians: jobactive caseload"),
    shinydashboard::box(
      uiOutput("table_jobactive_aboriginal") %>%
                          djpr_with_spinner(),
      width = 12
      ),
    djpr_plot_box("gr_abor_jobactive_sincecovid_line", width = 12),
    djpr_plot_box("gr_abor_jobactive_bar", width = 12),
    djpr_plot_box("aboriginal_footnote", width = 12)
  )
}
