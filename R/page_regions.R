page_regions <- function(...) {
  djpr_tab_panel(
    title = "Regions",
    h1("Unemployment by region"),
    br(),
    djpr_plot_title("The unemployment rate varies widely across Victoria"),
    djpr_plot_subtitle("Unemployment rate by region (SA4), per cent"),
    fluidRow(
      column(6,
             leaflet::leafletOutput("reg_unemprate_map")),
      column(6,
             plotOutput("reg_unemprate_bar"))
    ),
    djpr_plot_caption("Source: ABS Labour Force. Note: regional data is smoothed using a 3-month rolling average."),
    br(),
    "TEXT HERE",
    djpr_plot_ui("reg_emp_regions_sincecovid_line"),
    br(),
    "TEXT HERE",
    djpr_plot_ui("reg_unemprate_multiline")

  )
}
